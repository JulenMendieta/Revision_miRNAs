##d010_read_sample_info.r
##2014-05-23 dmontaner@cipf.es
##Collecting miRNA data from The Cancer Genome Atlas
##This script organizes all sample information

date ()
Sys.info ()[c("nodename", "user")]
commandArgs ()
rm (list = ls ())
R.version.string ##"R version 3.2.0 (2015-04-16)"
#library (); packageDescription ("", fields = "Version") #

try (source (".job.r")); try (.job)

options (width = 200)


## FUNCTION
barcode2 <- function (bc, to = "sample") {
  y <- sapply (bc, strsplit, split = "-")
  lon <- sapply (y, length)
  L <- unique (lon)
  if (length (L) != 1) stop ("unequal pa terns")
  mat <- matrix (unlist (y), ncol = L, byrow = TRUE)
  #He cambiado el stop a 3 para añadir toda la info de la muestra
  sample.number <- substr (mat[,4], start = 1, stop = 3)
  if (to == "sample") {
    res  <- paste (mat[,1], mat[,2], mat[,3], sample.number, sep = "-")
  } else { ##patient
    res <- paste (mat[,1], mat[,2], mat[,3], sep = "-")
  }
  return (res)
}


###DATOS
setwd (file.path (.job$dir$raw, "clinical"))


################################################################################
## File Info: information about the files available
################################################################################

ficheros <- dir (pattern = "biospecimen_sample", recursive= TRUE)
sample.list <- list()
for (fi in ficheros) {
  #Guardamos los nombres de las columnas y les damos un formato X
  columnas <- read.table (fi, header = FALSE, sep = "\t", quote = "", colClasses = "character", nrow = 1)
  columnas <- unlist (columnas, use.names = FALSE)
  columnas <- make.names (columnas)
  
  #Tomamos los datos de los ficheros
  sinfo <- read.table (fi, header = TRUE, sep = "\t", quote = "", as.is = TRUE, na.strings = "[Not Available]", skip = 1)
  #Les añadimos los nombres de las columnas
  colnames (sinfo) <- columnas
  
  ## eliminate duplicated lines (just in case)
  sinfo <- unique (sinfo)
  
  ## include sample and patient barcodes and file name
  sinfo[,"Sample"]  <- barcode2 (sinfo$bcr_sample_barcode, to = "sample")
  sinfo[,"patient"] <- barcode2 (sinfo$bcr_sample_barcode, to = "patient")
  tag <- sapply (strsplit (sapply (strsplit (fi, split = "_"), function (x) x[2]), split = "\\."), function (x) x[1])
  ## keep just sample and type information
  dim (sinfo)
  sinfo <- unique (sinfo[,c("Sample", "patient", "sample_type")])
  dim (sinfo)
  
  #Lo guardamos en la lista final
  sample.list[[tag]] <- sinfo
}


###SALVAMOS
save (list = "sample.list", file = file.path (.job$dir$proces, "sample_info_all.RData"))


###EXIT
warnings ()
sessionInfo ()
q ("no")