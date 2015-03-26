## e020_revision_datos_clinicos.r
## 2015-03-26 julenmendieta92@gmail.com
## Script para revisar los datos clinicos generados en "e010_tabla_datos_clinicos.r" y ver que tipo de dato hay en cada columna
date ()
Sys.info ()[c("nodename", "user")]
commandArgs ()
rm (list = ls ())
R.version.string ##"R version 3.1.2 (2014-10-31)"

setwd ("/home/jmendieta/Documents/revision_mirnas/datos/procesados")
datosClinicos <- readRDS("e010_tabla_datos_clinicos.RData")
infoDatosClinicos <- list()
for (id in names (datosClinicos)) {
  datos0 <- datosClinicos[[id]]
  info <- table (sapply (datos0, class))
  infoDatosClinicos[[id]] <- info
}

saveRDS(infoDatosClinicos, file="e020_revision_datos_clinicos.RData")
