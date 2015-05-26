## e050_tabla_clinicos_muestra.r
## 2015-03-26 julenmendieta92@gmail.com
##Modificado: 2015-05-19
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


#Ahora le añadimos los datos de biospecimen_sample

load (file.path (.job$dir$proces, "sample_info_all.RData"))   #Con esto cargamamos sample.list
load (file.path (.job$dir$proces, "datos_clinicos_comunes.RData"))  #Con esto cargamos tablist

#Comprobamos que tengan los mismos datos
table(names(sample.list) %in% names(tablist))
table(names(tablist) %in% names(sample.list))

#Por lo que se ve hay un tag entre los ficheros que indican el tipo de muestra, que no aparece entre los que indican los datos clinicos
#Si no tenemos datos clínicos de él, de momento no nos interesa, por lo que lo eliminamos
sample.list <- sample.list[names(sample.list) %in% names(tablist)]
tag <- names(sample.list)




#####Esta parte se me habia olvidado hacerla antes, así que si funciona ya pensare en integrarla mejor 
#Tenemos el problema de que aqui hay IDs repetidos, así que no puedo juntar lo que obtengamops de aqui a sample.list
#Tenemos que hacer una tabla de esto, y juntarle a ella lo de sample.list

setwd (file.path (.job$dir$raw, "clinical"))
ficheros <- dir (pattern = "biospecimen_shipment_portion", recursive= TRUE)  #Guardamos en ficheros el stream de cada carpeta y los ficheros que contiene
length (ficheros)

#Primero guardamos en datos las columans que indican la muestra, el barcode y el ID asociable a expresión de proteinas
datos <- NULL
for (fi in ficheros) {
  print (fi)
  nombres <- unlist (strsplit (readLines (fi)[1], split = "\t"))
  #print (nombres)
  datos0 <- read.table (fi, header = FALSE, sep = "\t", quote = "", col.names = nombres, as.is = TRUE, na.strings = "[Not Available]", skip = 2)
  datos0[,"file"] <- fi
  tag0 <- sapply (strsplit (sapply (strsplit (datos0[,"file"], split = "_"), function (x) x[2]), split = "\\."), function (x) x[1])
  tag0 <- unique(tag0)
  print (dim (datos0))
  datos0 <- datos0[,c("bcr_sample_barcode", "shipment_portion_bcr_aliquot_barcode", "bcr_shipment_portion_uuid")]
  try (datos[[tag0]] <- datos0)
}

names (datos)

#Ahora generamos x columnas libres para añadir los datos clínicos y de muestra.
#Lo siguiente es añadir los datos de "sample_type" de sample.list a la tabla de tablist
#En la tabla de datos clinicos tenemos el ID del paciente, pero en la de sample tenemos ese ID dos veces, cada uno por la muestra a la que pertenece
#Esto significa que tendriamos que añadir una linea extra por cada ID que coincida para el caso en el q hay caso y control


#Guardamos las cabeceras de tablist en una variable
cols <- colnames(tablist[[names(tablist)[1]]])
#eliminamos bcr_patients_barcode 
print(cols[1])
cols <- cols[-1]
#Creamos esas columnas en sample.list
for (t in tag) {
  sample.list[[t]][, cols] <- NA
  #sample.list[[t]][, "bcr_shipment_portion_uuid"] <- NA
}


for (t in tag) {
  #Cada vez uqe tengamos un ID en los dos sitios se le añadira la tabla de clinicos a la de sample.list. 
  for (id in  tablist[[t]][,"bcr_patient_barcode"]) {
    if(id %in% sample.list[[t]][,"patient"]) {
      posidsam <- (sample.list[[t]]["patient"] == id)
      posidtab <- (tablist[[t]][,"bcr_patient_barcode"] == id)
      sample.list[[t]][posidsam, cols] <- tablist[[t]][posidtab, cols]
    } 
  }
}


#Ahora hay que meter en datos lo de sample.list, ya que datos tiene varios ID para la misma muestra en algún punto, y nos sera mas facil
#duplicar lo de sample.list al meterlo ent able que al reves.

#Primero:
  #Creamos las columnas que le faltan a table. Como nos serviremos de sample, que está en las dos, no la vamos a ñadir dos veces
cols <- colnames(sample.list[[1]])
  #Borramos sample
print(cols[1])
cols <- cols[-1]

#Comprobamos que tengan los mismos datos
table(names(sample.list) %in% names(datos))
table(names(datos) %in% names(sample.list))

#Por lo que se ve hay dos tag que no aparecen en los dos ficheros
#Si no tenemos datos asociables a expresión de proteinas o clinicos de éllos, de momento no nos interesan, por lo que los eliminamos
sample.list <- sample.list[names(sample.list) %in% names(datos)]
datos <- datos[names(datos) %in% names(sample.list)]
tag <- names(sample.list)
  #Añadimos las columnas. 
for (t in tag) {
  datos[[t]][, cols] <- NA
}


  #Ahora que ya hemos añadido todas las columnas, solo queda unir las tablas
#Aqui si le digo q mire Sample de sample.list, lo puedo comparar con "bcr_sample_barcode"
for (t in tag) {
  for (muestra in datos[[t]][, "bcr_sample_barcode"]) {
    if (muestra %in% sample.list[[t]][, "Sample"]) {
      posidx <- datos[[t]][, "bcr_sample_barcode"] == muestra
      posidsam <- sample.list[[t]]["Sample"] == muestra
      datos[[t]][posidx, cols] <- sample.list[[t]][posidsam, cols]
    }
  }
}

#####

save (list = "datos", file = file.path (.job$dir$proces, "tabla_clinicos_muestra.RData"))

###EXIT
warnings ()
sessionInfo ()
q ("no")
