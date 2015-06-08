## e080_comparacion_expresion.r
## 2015-03-26 julenmendieta92@gmail.com
##Modificado: 2015-05-26
## Script para comparar los niveles de expresión
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

load (file.path (.job$dir$proces, "clinicos_miryprot.RData"))   #Con esto cargamamos mat
load (file.path (.job$dir$proces, "miR_exp.RData"))  #Con esto cargamos miR.exp.info y mir.exp
load (file.path (.job$dir$proces, "prot_exp.RData"))  #Con esto cargamos  prot.exp.info y prot.exp

tag <- names(miR.exp)
#Primero dejamos solo al referencia al paciente en los barcodes de miR.exp
for (t in tag) {
  barcode <- colnames(miR.exp[[t]])
  colnames(miR.exp[[t]]) <- sub("-...-....-..", "", barcode)
}


#Miramos los tipos de muestras tomadas, solo hay 45 de tejido normal.
t (t (table (mat[,"sample_type"])))

#Vamos a buscar el cancer que tenga mas muestras con datos de miR y prot
table(mat[,"tag"])
max(table(mat[,"tag"]))  #En este caso vemos que es LGG


#Tratamos de generar matrices de miR y prot solo para el cancer de interes y las muestras comunes
matriz <- mat[mat[,"tag"] == "LGG",]
miRNAs <- miR.exp[["LGG"]]
prots <- prot.exp[["LGG"]]

#Guardar solo la expresión de lso que tienen datos comunes
miRNAs <- miRNAs[, colnames(miRNAs) %in% matriz[, "bcr_sample_barcode"]]
prots <- prots[, colnames(prots) %in% matriz[, "bcr_shipment_portion_uuid"]]

table(duplicated(matriz[, "bcr_sample_barcode"]))
table(duplicated(matriz[, "bcr_shipment_portion_uuid"]))
#En este caso no hay Ids duplicados, así que no voy a mirar como tratariamos con ello

#Cambiamos el nombre de columna de las proteinas por el barcode que indica la muestra.
for (i in 1:length(colnames(prots))) {
  colnames(prots)[i] <- matriz[matriz[, "bcr_shipment_portion_uuid"] == colnames(prots)[i], "bcr_sample_barcode"]
}

#Llamamos a las filas de matriz como la muestra a la que representan y borramos la columna que indica el sample_barcode
for (i in 1:length(rownames(matriz))) {
  rownames(matriz)[i] <- matriz[i, "bcr_sample_barcode"]
}
matriz["bcr_sample_barcode"] <- NULL

#Y ahora lo ponemos todo en el mismo orden
miRNAs <- miRNAs[, rownames(matriz)]
prots <- prots[, rownames(matriz)]

table (duplicated (colnames (miRNAs)))
table (duplicated (colnames (prots)))

table (colnames (prots) == colnames (miRNAs))
table (colnames (prots) == rownames (matriz))

matriz[1:3,]

#Se cambia el nombre de las variables y las guardamos
sinfo <- matriz
mirna <-miRNAs
prots


###SAVE
save (list = "sinfo", file = file.path (.job$dir$proces, "clinicos_miryprot_LGG.RData"))
save (list = "mirna", file = file.path (.job$dir$proces, "miR_exp_LGG.RData"))
save (list = "prots", file = file.path (.job$dir$proces, "prot_exp_LGG.RData"))




###EXIT
warnings ()
sessionInfo ()
q ("no")