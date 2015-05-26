##d030_download_protein_expression.r
##2015-01-13 dmontaner@cipf.es
##2015-05-19 julenmendieta92@gmail.com
##Collecting data from TCGA

## The scripts uses TCGA DCC Web Services to find out all the PROTEIN EXPRESSION data.

## Batch effect is very strong.
## Do just comparisons whiting each disease.

## Find the platform here:
## http://tcga-data.nci.nih.gov/tcgadccws/GetHTML?query=Platform

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

setwd (file.path (.job$dir$raw, "protein_exp"))

################################################################################
## DOWNLOAD DATA

base   <- "https://tcga-data.nci.nih.gov"
my.url <- "http://tcga-data.nci.nih.gov/tcgadccws/GetXML?query=Archive[Platform[@name=MDA_RPPA_Core]][ArchiveType[@type=Level_3]][@deployStatus=Available][@isLatest=1]"

char <- getURL (my.url)
char

mix <- xmlInternalTreeParse (char)
class (mix)
mix <- xmlChildren (xmlChildren (mix)[["httpQuery"]])[["queryResponse"]]
class (mix)
mix

ns <- getNodeSet (mix, path = "class")
class (ns)
ns

datos <- xmlToDataFrame (ns, stringsAsFactors = FALSE)
dim (datos)
colnames (datos) <- paste0 ("V", 1:ncol (datos))
colnames (datos)[c (1, 3, 7)] <- c("date", "location", "name")
datos[1:3, 1:8]

datos[,"fecha"] <- as.Date (datos[,"date"], "%m-%d-%Y")
datos[,c ("date", "fecha")]

datos[,"url"] <- paste0 (base, datos[,3])
##datos[,"gzfile"] <- basename (datos[,3])
##datos[,"gzfolder"]  <- sub (".tar.gz", "", datos[,"gzfile"])

rownames (datos) <- datos[,"name"]

datos[1:3,]
#Hasta aqui hemos tomado los links a una serie de archivos. Si quieres veros copia el link de mu.url cambiando XML por HTML

system.time ({
  for (url in datos[,"url"]) {
    filename <- basename (url)
    download.file (url = url, destfile = filename, method = "curl")
    untar (tarfile = filename, compressed = TRUE)    
  }
})
##Todo lo de download data simplemente descarga los archivos comprimidos que podemos obtener desde el data matrix
##Como diferencia se ve que 'CHANGES_DCC.txt' 'DESCRIPTION.txt' 'MANIFEST.txt' 'README_DCC.txt' son algo diferentes a los que vienen desde el array, y están todos juntos dentro de una carpeta

################################################################################

## SOME CHECKS
length (dir (recursive= TRUE))  #Mira el total de ficheros utiles en la carpeta de trabajo y sus subcarpetas

ficheros <- dir (pattern = "protein_expression", recursive= TRUE) #Guarda en ficheros todos los ficheros que tengan 'protein_expression ' en el nombre??
length (ficheros)  #Mira la longitud de la variable ficheros
ficheros[1:3]  ##No sirve para nada más que ver los primeros 3 ficheros de la lista (te indica la carpeta descargada y cada uno de los archivos dentro)

li <- strsplit (ficheros, ".protein_expression.Level_3.")  #Separa el nombre de los ficheros usando como separador protein_expression. Al final queda una especie de código
table (sapply (li, length)) ##OK all 2
uuid <- sapply (li, function (x) x[2])  #Guarda en uuid la zona de la derecha que hay en li (los codigos y el .txt)
uuid <- sub (".txt", "", uuid)  #Borra el .txt de uuid
length (uuid)

folder <- dirname (ficheros)  #Guardamos en folder las carpetas en las que están los ficheros. Las carpetas tienen el nombre que se ha quedado al descomprimir el tar en el que estaban

fi <- sub ("mdanderson.org_", "", ficheros)  #Elimina mdanderson.org_ de la variable que contiene el nombre de los ficheros y la carpeta en la q están (mdanderson... está en el inicio del nombre de la carpeta)
li <- strsplit (fi, "\\.") #Separa el contenido de fi (nombre carepta y ficheros) por '\' y '.'
## Lo de arriba creo que esta hecho para windows, en linux seria con \/ --> primera barra indica que se lea la segunda como un caracter y no parte de una orden o algo asi
table (sapply (li, length)) ##OK all the same
tag <- sapply (li, function (x) x[1])  #Guarda en tag la primera posicion de li, que es la que hace referencia a la enfermedad, para cada fichero
length (tag)

finfo <- as.data.frame (list (fichero = ficheros, uuid = uuid, folder = folder, tag = tag), stringsAsFactors= FALSE)  #Guarda en esta variable el nombre de la carpeta (q contiene información del nivel) + " " + la referenciaa la enfermedad
class (finfo)  #Mira de que clase es finfo
dim (finfo)
sapply (finfo, class)
table (finfo[,"tag"])  #Crea una tabla en la que acumula la variable tag (info de la enfermedad) de cada fichero. Por tanto muestra cada enfermedad y el numero de ficheros que la estudian en finfo

##Todo esto es para ver que la ID de la primera linea del fichero coincide con la del nombre del fichero. OJO!! No todos los datos (miRNAseq por ejemplo) tienen un ID en la primera fila
finfo[,"uuid.infile"] <- NA  #Crea una tabla o vector con espacios libres para cada dato, para introducir el ID de dentro de cada fichero
#Con el bucle guarda en cada hueco la primera linea de cada fichero, que tiene Sample REF\t y el ID
system.time ({  #Mira cuanto tarda en ejecutarse este bucle
  for (i in 1:nrow (finfo)) {
    finfo[i, "uuid.infile"] <- readLines (con = finfo[i, "fichero"], n = 1)
  }
})
##El bucle sirve para guardar despues de la referencia a la enfermedad una columna con sample y despues la identidad asociada a ese fichero

finfo[,"uuid.infile"] <- sub ("Sample REF\t", "", finfo[,"uuid.infile"])  #quitamos Sample REF\t de la secuencia de caracteres
table (finfo[,"uuid"] == finfo[,"uuid.infile"], exclude = NULL) ## OK all the same

if (any (finfo[,"uuid"] != finfo[,"uuid.infile"])) stop ("UUIDs do not match")  #Mira que todas las IDs esten bien puestas

summary (finfo)

########################################

## Some duplicated UUIDS ????
dups <- duplicated (finfo[,"uuid"])  #Mira los valores de uuid, las IDs, que se repiten
table (dups)  #Muestra la cantidad de IDs q estan repetidas y la cantidad que no

duplicados <- finfo[dups, "uuid"]  #Guarda en una tabla los duplicados
finfo[,"is.dup"] <- finfo[,"uuid"] %in% duplicados  #Añade al final d finfo si esta duplicado 'True' o no 'False'
table (finfo[,"is.dup"], exclude = NULL)

table (table (finfo[,"uuid"], exclude = NULL), exclude = NULL)  #Te dice cuantos y cuantas veces están repetidos (el 1 en el 0 veces no se que pinta ahi).
table (finfo[,"tag"], finfo[,"is.dup"])  #Con esto genera una tabla en la que se muestra por enfermedad los ficheros duplicados

##sort by date and SERIAL INDEX
table (finfo$folder %in% rownames (datos)) ## OK
#Lo de aqui arriba solo funciona si primero has cargado los datos en la parte de descargar datos
finfo[,"fecha"] <- datos[finfo$folder, "fecha"]  #Supongo que esto es para guardar los datos de las fechas al final d finfo o algo asi
orden <- order (finfo[,"fecha"], finfo[,"is.dup"], finfo[,"uuid"], finfo[,"folder"], decreasing = TRUE)  #Esto para guardar la variable orden
finfo <- finfo[orden,]  #Esto ya para ordenar por fecha
finfo[finfo$is.dup,][1:10,]  #Con esto vemos los 10 primeros IDs duplicados

malos <- finfo[finfo$is.dup,]  #Guardamos en la variable malos los duplicados
orden <- order (malos[,"uuid"])  
malos <- malos[orden,]  #Los ordenamos por ID
table (malos[,"tag"])  #Mostramos en una tabla cuantos hay por enfermedad
malos[1:10,]  #Aqui se muestra la info de los 10 primeros
tail (malos)  #El final

dup <- duplicated (finfo[,"uuid"])  #Guardamos en la variable dup los IDs con informacion de si estan duplicadoos? o solo la info?
table (dup) 
finfo <- finfo[!dup,]  #Guaradmos en finfo todos los que no están duplicados?
table (duplicated (finfo[,"uuid"]))

rownames (finfo) <- finfo[,"uuid"]  #Guardamos como informacion de fila las IDs

################################################################################
tags <- unique(tag)
## READ DATA
datos.li <- list ()
system.time ({
  for (t in tags) {
    for (i in 1:nrow (finfo)) {  #por cada fila del contenido de finfo
      if (finfo[i, "tag"] == t) {
        datos.li[[t]][[finfo[i, "uuid"]]] <-  #guardar en la lista datos.li, en donde esta la ID ("i" es el numero de cada linea)
          read.table (file = finfo[i, "fichero"], header = TRUE, sep = "\t", quote = "", skip = 1, colClasses = c ("character", "numeric"), row.names = 1)
      }
    } 
  }
})

## MAKE MATRIX
#mirar los barcodes de un tag
prot.exp <- list()
for (t in tags) {  
  id.list <- lapply (datos.li[[t]], rownames)  #Guarda en una lista todos las prot que hay analizadas en cada fihcero indicando el ID del mismo.
  ids <- sort (unique (unlist (id.list)))  #Esto sera para guardar en ids el nombre de todos las proteinas analizadas en orden alfabetico
  length (ids)
  
  #Generamos para cada caso una matriz con las medidas necesarias
  tabla <- list()
  tabla[[t]] <- matrix (NA, nrow = length (ids), ncol = length (datos.li[[t]]))
  dim (tabla[[t]])
  rownames (tabla[[t]]) <- ids  #nombra cada fila de la matriz tabla según el miR al que hace referencia
  colnames (tabla[[t]]) <- names (datos.li[[t]])  #Aqui a las columnas les da el ID al que hacen referencia, y 
  
  #Ahora guardamos los datos de expresión asociados a cada prot y uuid
  for (uuid in names (datos.li[[t]])) {
    mat <- datos.li[[t]][[uuid]]
    tabla[[t]][,uuid] <- mat[ids,]
  }
  prot.exp[[t]] <- tabla[[t]]
}
##
#Da AVISO pero funciona
#Warning message:
#In is.na(x) : is.na() applied to non-(list or vector) of type 'NULL'



################################################################################
## ## Some Plots
## table (finfo$tag)

## boxplot (prot.exp[,c(which (finfo$tag == "ACC"), which (finfo$tag == "BLCA"))])
## abline (h = 0, col = "blue")
## boxplot (prot.exp[,c(which (finfo$tag == "KIRP"), which (finfo$tag == "KIRC"))])
## abline (h = 0, col = "blue")

## orden <- order (finfo$tag)
## boxplot (prot.exp[,orden])
## abline (h = 0, col = "blue")

## ran <- sample (colnames (prot.exp), size = 100)
## boxplot (prot.exp[,ran])
## abline (h = 0, col = "blue")

################################################################################

### SAVE
prot.exp.info <- finfo
save (list = c("prot.exp", "prot.exp.info"), file = file.path (.job$dir$proces, "prot_exp.RData"))


###EXIT
warnings ()
sessionInfo ()
q ("no")
