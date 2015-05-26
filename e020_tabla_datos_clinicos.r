## e020_tabla_datos_clinicos.r
## 2015-03-26 julenmendieta92@gmail.com
##Modificado: 2015-04-23
## Script para generar una tabla con el contenido de los ficheros "clinical_patient" (No esta presente en todos los canceres)
date ()
Sys.info ()[c("nodename", "user")]
commandArgs ()
rm (list = ls ())
R.version.string ##"R version 3.2.0 (2015-04-16)"

try (source (".job.r")); try (.job)
setwd (file.path (.job$dir$raw, "clinical"))

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
  #Anexo: todo a minusculas y eliminar espacios si los hay. Con los datos que tengo en principio no hace falta.
  nombres <- tolower(nombres)
  nombres <- gsub(" ", "_", nombres)
  
  
  datos0 <- read.table (fi, header = TRUE, sep = "\t", quote = "", col.names = nombres, as.is = TRUE, 
                        na.strings = c("[Not Available]", "[Not Applicable]", "[Not Evaluated]", "[Unknown]", "NA"), skip = 3)
  
  orden <- order (colnames (datos0))
  colnames (datos0)[orden]
  datos0 <- datos0[,orden]
  
  #
  table (sapply (datos0, class))
  #Con esto leemos todo lo que hay en el fichero
  todo.na <- apply (is.na (datos0), 2, all)
  table (todo.na)
  datos0 <- datos0[,!todo.na]  #Quitamos los datos en los que todo sea NA
  
  #Guardamos el tag
  tag <- sapply (strsplit (sapply (strsplit (fi, split = "_"), function (x) x[2]), split = "\\."), function (x) x[1])
  #Guardamos en la lista final
  datosClinicos[[tag]] <- datos0
  
  
}

summary(datosClinicos)

save (list = "datosClinicos", file = file.path (.job$dir$proces, "clinical_info_all.RData"))

###EXIT
warnings ()
sessionInfo ()
q ("no")
