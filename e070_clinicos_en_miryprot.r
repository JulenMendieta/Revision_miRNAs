## d070_clinicos_en_miryprot.r
## 2015-03-26 julenmendieta92@gmail.com
##Modificado: 2015-05-21
## Script para generar una tabla con el contenido de los ficheros "clinical_patient" (No esta presente en todos los canceres)
date ()
Sys.info ()[c("nodename", "user")]
commandArgs ()
rm (list = ls ())
R.version.string ##"R version 3.2.0 (2015-04-16)"

try (source (".job.r")); try (.job)

options (width = 170)
#options (width = 1000)

setwd (file.path (.job$dir$proces))


#Ahora le añadimos los datos

load (file.path (.job$dir$proces, "tabla_clinicos_muestra.RData"))   #Con esto cargamamos datos
load (file.path (.job$dir$proces, "miR_exp.RData"))  #Con esto cargamos miR.exp.info y mir.exp
load (file.path (.job$dir$proces, "prot_exp.RData"))  #Con esto cargamos  prot.exp.info y prot.exp

tag <- names(miR.exp)
#Primero dejamos solo al referencia al paciente en los barcodes de miR.exp
for (t in tag) {
  barcode <- colnames(miR.exp[[t]])
  colnames(miR.exp[[t]]) <- sub("-...-....-..", "", barcode)
}


##Luego creamos dos columnas en datos donde incluiremos TRUE o FALSE dependiendo de que haya datos de esa muestra para miR o prot
#Como no tenemos datos clinicos de todos los tags, cambiamos la variable tag a los tags de los que si tenemos estos datos
tag <- names(datos)
for (t in tag) {
  datos[[t]][, c("prot.expr", "miR.expr")] <- FALSE
}

for (t in tag) {
  for (i in 1:nrow(datos[[t]])) {
    if (datos[[t]][i, "bcr_sample_barcode"] %in% colnames(miR.exp[[t]]) == TRUE) {
      datos[[t]][i, "miR.expr"] <- TRUE
    }
    if (datos[[t]][i, "bcr_shipment_portion_uuid"] %in% colnames(prot.exp[[t]]) == TRUE) {
      datos[[t]][i, "prot.expr"] <- TRUE
    }
  }
  print (t)
  print(table(datos[[t]]["prot.expr"]==TRUE))
  print(table(datos[[t]]["miR.expr"]==TRUE))
}
#Hasta aqui ya tenemos una tabla con todos los datos clínicos, el tipo de muestra, y que indica si tiene datos de expresión de miR y prot

#Creamos una data.frame en la que los datos de cada tumor estan al mismo nivel
tags <- names (datos)
mat <- NULL
for (ta in tags) {
  dt <- datos[[ta]]
  dt[,"tag"] <- ta
  mat <- rbind (mat, dt)
}
dim (mat)  
mat[1:3,]

summary (mat)


#Eliminamos los pacientes que not engan datos de miR y prot
touse <- mat[,"prot.expr"] & mat[, "miR.expr"]
table (touse)
mat <- mat[touse,]

save (list = "mat", file = file.path (.job$dir$proces, "clinicos_miryprot.RData"))

#Miramos los tipos de muestras tomadas, solo hay 45 de tejido normal.
t (t (table (mat[,"sample_type"])))

#Vamos a buscar el cancer que tenga mas muestras con datos de miR y prot
table(mat[,"tag"])
max(table(mat[,"tag"]))  #En este caso vemos que es LGG


# #Miramos a que tumor pertenecen las muestras de tejido sano
# st <- mat[,"sample_type"] == "Solid Tissue Normal"
# table (st)
# mat[st,]
# table (mat[st, "tag"])
# 
# #Comprobamos que para un mismo paciente tengamos muestras tanto de tejido sano como enfermo.
# pacientes <- mat[st, "patient"] 
# buenos<- mat[,"patient"] %in% pacientes 
# table (mat[buenos, "patient"])
# table (mat[buenos, "sample_type"])
# 
# mat[buenos, c("patient", "sample_type")]
# matriz <- mat[buenos,]



###EXIT
warnings ()
sessionInfo ()
q ("no")