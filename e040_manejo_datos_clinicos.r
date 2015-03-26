## e040_manejo_datos_clinicos.r
## 2015-03-26 julenmendieta92@gmail.com
## Script para manjear los datos clinicos. Está sin terminar y puede que no se use
date ()
Sys.info ()[c("nodename", "user")]
commandArgs ()
rm (list = ls ())
R.version.string ##"R version 3.1.2 (2014-10-31)"

setwd ("/home/jmendieta/Documents/revision_mirnas/datos/procesados")
datosClinicos <- readRDS("e010_tabla_datos_clinicos.RData")
summary(datosClinicos)

infoDatosClinicos <- readRDS("e020_revision_datos_clinicos.RData")
infoDatosClinicos


presenciaColumnas <- readRDS("e030_estadistica_columnas_datos_clinicos.RData")
table (rowSums (presenciaColumnas))  #Esto nos muestra cuantas cabeceras hay solo en x ficheros (cuantas en 1, cuantas en 2, cuantas en 4 etc.)
colnames (presenciaColumnas) <- NULL
touse <- rowSums (presenciaColumnas) == length(datosClinicos) #Guardamos solo las cabeceras que esten en todos los ficheros
touse
presenciaColumnas[touse,]
rownames (presenciaColumnas)[touse]  #Muestra los nombres de las cabeceras q estan en todos los ficheros
