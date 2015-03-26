## e010_tabla_datos_clinicos.r
## 2015-03-26 julenmendieta92@gmail.com
## Script para generar una tabla con el contenido de los ficheros "clinical_patient" (No esta presente en todos los canceres)
date ()
Sys.info ()[c("nodename", "user")]
commandArgs ()
rm (list = ls ())
R.version.string ##"R version 3.1.2 (2014-10-31)"

setwd ("/home/jmendieta/Documents/revision_mirnas/datos/raw/clinical")

ficheros <- dir (pattern = "clinical_patient", recursive= TRUE)  #Guardamos en ficheros el stream de cada carpeta y los ficheros que contiene
ficherosTotales <- dir (recursive= FALSE)   #Con esto guardamos la cantidad total de ficheros que hay
ficheros
#La diferencia de estos dos dirá cuantos no tenian el fichero de datos clinicos del paciente
length(ficherosTotales)
length (ficheros)

datosClinicos <- list ()
for (fi in ficheros) {
  print (fi)
  
  nombres <- unlist (strsplit (readLines (fi)[1], split = "\t"))
  datos0 <- read.table (fi, header = TRUE, sep = "\t", quote = "", col.names = nombres, as.is = TRUE, 
                        na.strings = c("[Not Available]", "[Not Applicable]", "[Not Evaluated]", "[Unknown]"), skip = 3)
  
  orden <- order (colnames (datos0))
  colnames (datos0)[orden]
  datos0 <- datos0[,orden]
  datosClinicos[[fi]] <- datos0
}

summary(datosClinicos)

setwd ("/home/jmendieta/Documents/revision_mirnas/datos/procesados")

saveRDS(datosClinicos, file="e010_tabla_datos_clinicos.RData")
