##d010_download_clinical_data.r
##2015-01-13 dmontaner@cipf.es
##2015-04-28 julenmendieta92@gmail.com
##Collecting data from TCGA

## The scripts uses TCGA DCC Web Services to find out all CLINICAL data.

## Find the platform here:
## http://tcga-data.nci.nih.gov/tcgadccws/GetHTML?query=Platform
## "bio" seems to be the appropriated one

## grep -B 1 -A 2 E6B279BC-97A1-4B9A-9632-06326C418E1C nationwidechildrens.org_biospecimen_sample_coad.txt 

date ()
Sys.info ()[c("nodename", "user")]
commandArgs ()
rm (list = ls ())
R.version.string ##"R version 3.2.0 (2015-04-16)"
library (RCurl); packageDescription ("RCurl", fields = "Version") #"1.95-4.5"
library (XML); packageDescription ("XML", fields = "Version") #"3.98-1.1"
#help (package = XML)
#help (package = RCurl)

try (source (".job.r")); try (.job)

options (width = 170)
#options (width = 1000)

setwd (file.path (.job$dir$raw, "clinical"))

################################################################################
## DOWNLOAD DATA
base   <- "https://tcga-data.nci.nih.gov"
my.url <- "http://tcga-data.nci.nih.gov/tcgadccws/GetXML?query=Archive[Platform[@name=bio]][ArchiveType[@type=Level_2]][@deployStatus=Available][@isLatest=1]"

char <- getURL (my.url)
char

mix <- xmlInternalTreeParse (char)
class (mix)
mix <- xmlChildren (xmlChildren (mix)[["httpQuery"]])[["queryResponse"]]
class (mix)
mix

ns <- getNodeSet (mix, path = "class")
class (ns)
length (ns)
ns

datos <- xmlToDataFrame (ns, stringsAsFactors = FALSE)
dim (datos)
datos[1:3,]
colnames (datos) <- paste0 ("V", 1:ncol (datos))
colnames (datos)[c (1, 2, 3, 7)] <- c("date", "baseName", "location", "name")
datos[1:3,]

datos[,"fecha"] <- as.Date (datos[,"date"], "%m-%d-%Y")
datos[,c ("date", "fecha")]

datos[,"url"] <- paste0 (base, datos[,3])

datos[,"tag"] <- sapply (strsplit (datos[,"baseName"], split = "_"), function (x) x[2])
table (duplicated (datos[,"tag"]))

##En nuestro caso se repite LUAD, y tenemos un fichero con solo una revisión (genome) vs otro algo mas reciente con 27 revisiones y 
#mas muestras (nationwidechildrens). El de nationwide es el único que se puede relacionar con las proteinas

datos[1:3,]

system.time ({
  for (url in datos[,"url"]) {
    filename <- basename (url)
    download.file (url = url, destfile = filename, method = "curl")
    untar (tarfile = filename, compressed = TRUE)    
  }
})

################################################################################
## FIND UUID
ficheros <- dir (pattern = "biospecimen_shipment_portion", recursive= TRUE)  #Guardamos en ficheros el stream de cada carpeta y los ficheros que contiene
length (ficheros)

datos <- NULL
for (fi in ficheros) {
  print (fi)
  nombres <- unlist (strsplit (readLines (fi)[1], split = "\t"))
  #print (nombres)
  datos0 <- read.table (fi, header = FALSE, sep = "\t", quote = "", col.names = nombres, as.is = TRUE, na.strings = "[Not Available]", skip = 2)
  datos0[,"file"] <- fi
  print (dim (datos0))
  try (datos <- rbind (datos, datos0))
}
dim (datos)

datos[,"tag"] <- sapply (strsplit (sapply (strsplit (datos[,"file"], split = "_"), function (x) x[2]), split = "\\."), function (x) x[1])


if (any (duplicated (datos[,"bcr_shipment_portion_uuid"]))) stop ("DUPLICATED") ## OK NO DUPS

##protein data
load (file.path (.job$dir$proces, "prot_exp.RData"))

dim (prot.exp)

prot.ids <- colnames (prot.exp)
prot.ids[1:3]

table (datos[,"bcr_shipment_portion_uuid"] %in% prot.ids)
table (prot.ids %in% datos[,"bcr_shipment_portion_uuid"]) #deberian esta todos ???
table (tolower (prot.ids) %in% tolower (datos[,"bcr_shipment_portion_uuid"])) #deberian esta todos ???
setdiff (tolower (prot.ids), tolower (datos[,"bcr_shipment_portion_uuid"])) #deberian esta todos ???

malos <- !tolower (prot.ids) %in% tolower (datos[,"bcr_shipment_portion_uuid"])
table (malos)
table (prot.exp.info[malos, "tag"])

datos[,"has.prot"] <- tolower (datos[,"bcr_shipment_portion_uuid"]) %in% tolower (prot.ids)
table (datos[,"has.prot"])
table (datos[,"tag"], datos[,"has.prot"])

##paso a miR
#con esto guardamos los uuid de proteinas que tienen datos clinicos
uuidcomun <- prot.ids[tolower (prot.ids) %in% tolower (datos[,"bcr_shipment_portion_uuid"])]

##miRNA data
load (file.path (.job$dir$proces, "miR_exp.RData"))

#Comprobación previa de que no hay repetidos en un mismo grupo. SI QUE LOS HAY.
for (tag in names(miR.exp)) {
  miR.barcodesub <- colnames(miR.exp[[tag]])
  miR.barcodesub <- sub (".-...-....-..", "", miR.barcodesub)  
  #if (TRUE %in% duplicated(miR.barcodesub)) stop ("Duplicated patient in the same study")
}
#Con este bucle guardamos todas las barcodes
miR.barcodes <- c()
for (tag in names(miR.exp)) {
  miR.barcodesub <- colnames(miR.exp[[tag]])
  miR.barcodes <- append(miR.barcodes, miR.barcodesub)
}
miR.barcodes[1:3]

#En este punto habria que reducir la barcode a lo que nos interesa seria dejar solo XXXX-00-0000-00 quitando X-00X-0000-00
miR.barcodes <- sub (".-...-....-..", "", miR.barcodes)  
#Y también el dato referente al vial de datos[,"bcr_sample_barcode"]
datosbarcode <- gsub('.{1}$', '', datos[,"bcr_sample_barcode"])
table (duplicated (datosbarcode))
table (table (datosbarcode))

table (datosbarcode %in% miR.barcodes)
table (miR.barcodes %in% datosbarcode) #deberian esta todos ???

table (unique (datosbarcode) %in% unique (miR.barcodes))
table (unique (miR.barcodes) %in% unique (datosbarcode))



table (tolower (miR.barcodes) %in% tolower (datosbarcode)) #deberian esta todos ???
setdiff (tolower (miR.barcodes), tolower (datosbarcode)) #deberian esta todos ???

######
#DEPENDIENDO DEL ORDEN EN QUE COMPARES LOS DE ARRIBA SALEN MAS O MENOS, ESO ES NORMAL?
######

malos <- !tolower (miR.barcodes) %in% tolower (datosbarcode)
table (malos)
table (miR.exp.info[malos, "tag"])

datos[,"has.miR"] <- tolower (datosbarcode) %in% tolower (miR.barcodes)
table (datos[,"has.miR"])
table (datos[,"tag"], datos[,"has.miR"])
################################################################################


###EXIT
warnings ()
sessionInfo ()
q ("no")
