## e060_revision_datos_clinicos.r
## 2015-03-26 julenmendieta92@gmail.com
##Modificado: 20015-04-23
## Script para revisar los datos clinicos generados en "e010_tabla_datos_clinicos.r" y ver que tipo de dato hay en cada columna
date ()
Sys.info ()[c("nodename", "user")]
commandArgs ()
rm (list = ls ())
R.version.string ##"R version 3.2.0 (2015-04-16)"

try (source (".job.r")); try (.job)

options (width = 170)
#options (width = 1000)

setwd (file.path (.job$dir$proces))

load (file.path (.job$dir$proces, "clinical_info_all.RData"))
infoDatosClinicos <- list()
for (id in names (datosClinicos)) {
  datos0 <- datosClinicos[[id]]
  info <- table (sapply (datos0, class))
  infoDatosClinicos[[id]] <- info
}

save (list = "infoDatosClinicos", file = file.path (.job$dir$proces, "revision_datos_clinicos.RData"))

###EXIT
warnings ()
sessionInfo ()
q ("no")