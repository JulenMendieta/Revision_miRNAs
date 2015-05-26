## e040_manejo_datos_clinicos.r
## 2015-03-26 julenmendieta92@gmail.com
## Script para manejar los datos clinicos y ver los diferentes datos clinicos que hay y en cuantos de los cánceres.
date ()
Sys.info ()[c("nodename", "user")]
commandArgs ()
rm (list = ls ())
R.version.string ##"R version 3.2.0 (2015-04-16)"

try (source (".job.r")); try (.job)

options (width = 170)
#options (width = 1000)

setwd (file.path (.job$dir$raw, "clinical"))
ficheros <- dir (pattern = "clinical_patient", recursive= TRUE)  #Guardamos en ficheros el stream de cada carpeta y los ficheros que contiene

setwd (file.path (.job$dir$proces))

load (file.path (.job$dir$proces, "clinical_info_all.RData"))
summary(datosClinicos)

load (file.path (.job$dir$proces, "estadistica_columnas_datos_clinicos.RData"))
presenciaColumnas <- mat
mat <- NULL

table (rowSums (presenciaColumnas))  #Esto nos muestra cuantas cabeceras hay solo en x ficheros (cuantas en 1, cuantas en 2, cuantas en 4 etc.)
colnames (presenciaColumnas) <- NULL
touse <- rowSums (presenciaColumnas) == length(datosClinicos) #Guardamos solo las cabeceras que esten en todos los ficheros
touse
presenciaColumnas[touse,]
rownames (presenciaColumnas)[touse]  #Muestra los nombres de las cabeceras q estan en todos los ficheros



# ###PROBLEMA: ¿Pregunto al usuario mejor cual quiere que sea el mínimo de columnas?
# tamanyofich <- length(ficheros)
# ##Voy a poner un bucle para incluir datos hasta tener un numero aceptable (digamos 10):
# while(table(touse)[[2]] < 10) {   #Mientras que el numero de TRUE en touse sea menor que 10
#   touse[rowSums (mat) == tamanyofich - 1] = TRUE
#   tamanyofich <- tamanyofich - 1
# }  
# ##
# 
# touse
# mat[touse,]
# rownames (mat)[touse]  #Muestra los nombres de las 10 cabeceras mas frecuentes

setwd (file.path (.job$dir$raw, "clinical"))

#Con esto guardamos las columnas que son comunes a todos los ficheros

tablist <- list ()
for (fi in ficheros) {
  nombres <- unlist (strsplit (readLines (fi)[1], split = "\t"))
  datos0 <- read.table (fi, header = TRUE, sep = "\t", quote = "", col.names = nombres, as.is = TRUE, na.strings = c("[Not Available]", "[Not Applicable]", "[Not Evaluated]", "[Unknown]", "NA"), skip = 3)
  #Con esto miramos cuantas columnas de las presentes en todos los datos tenemos hasta ahora
  table (colnames(datos0) %in% rownames (presenciaColumnas)[touse])
  mantener <- colnames(datos0) %in% rownames (presenciaColumnas)[touse]
  #Y las guardamos
  datos0 <- datos0[mantener]
  ##Hasta aqui
  
  #Guardamos el tag
  tag <- sapply (strsplit (sapply (strsplit (fi, split = "_"), function (x) x[2]), split = "\\."), function (x) x[1])
  
  print (dim (datos0))
  tablist[[tag]] <- datos0
  
  #Con este bucle guardamos en una lista todos los datos que hay en cada fichero
}

save (list = "tablist", file = file.path (.job$dir$proces, "datos_clinicos_comunes.RData"))

###EXIT
warnings ()
sessionInfo ()
q ("no")
