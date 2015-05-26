## e000_revision_datos_cabecera.r
## 2015-04-21 julenmendieta92@gmail.com
##Modificado: 2015-04-23
## Script para detectar columnas con datos presentes en todos los estudios
date ()
Sys.info ()[c("nodename", "user")]
commandArgs ()
rm (list = ls ())
R.version.string ##"R version 3.1.2 (2014-10-31)"

setwd ("/home/jmendieta/Documents/revision_mirnas/datos/raw/clinical")

ficheros <- dir (pattern = "clinical_patient", recursive= TRUE)  #Guardamos en ficheros el stream de cada carpeta y los ficheros que contiene
ficheros
length (ficheros)


tabnomb <- c()
lnombres <- list()
for (fi in ficheros) {
  nombres <- unlist (strsplit (readLines (fi)[1], split = "\t"))
  #Anexo: todo a minusculas y eliminar espacios si los hay. Con los datos que tengo en principio no hace falta.
  nombres <- tolower(nombres)
  nombres <- gsub(" ", "_", nombres)

  #Separamos las cabeceras en palabras usando "_" como separador
  palabCabecera <- strsplit(nombres, "_")
  #Hacemos que esten todas al mismo nivel en la lista
  palabCabecera <- unlist(palabCabecera, recursive = FALSE)
  #Eliminamos las que tengan 3 o menos letras
  palabCabecera <- gsub("\\b[a-zA-Z0-9]{1,3}\\b", "", palabCabecera)
  palabCabecera <- palabCabecera[palabCabecera != ""]
  print(length(palabCabecera))
  
  #Guardamos en una variable junto con las cabeceras de los demás ficheros.
  tabnomb <- append(tabnomb, palabCabecera)
  #Guardamos en otra variable los nombres y a que fichero pertenecen para luego poder buscarlos
  lnombres[[fi]] <- nombres
}

tablist <- list ()
for (fi in ficheros) {
  nombres <- unlist (strsplit (readLines (fi)[1], split = "\t"))
  datos0 <- read.table (fi, header = TRUE, sep = "\t", quote = "", col.names = nombres, as.is = TRUE, na.strings = "[Not Available]", skip = 3)
  tablist[[fi]] <- datos0
  #Con este bucle guardamos en una lista todos los datos que hay en cada fichero
}
  
#Ahora toca mirar cuantas palabras se repiten y cuantas veces
frec <- sort(table(tabnomb), decreasing=TRUE)

###
#Vamos a ir mirando una forma de buscar las palabras que nos interesen a ver en que ficheros estan
#Ejemplo para ver en que ficheros sale la palabra "pathologic" o la cabecera que busquemos
grep("prospective_collection", lnombres)
length(grep("prospective_collection", lnombres))
#Ejemplo ver en que ficheros y en que posiciones sale la palabra "pathologic"
posPalab <- lapply(lnombres, function(ch) grep("outcome", ch))
#Ejemplo ver las cabeceras que coinciden con ello
resultPalab <- c()
for (fich in 1:length(posPalab)) {try(resultPalab <- append(resultPalab, (lnombres[[fich]][posPalab[[fich]]])))}
unique(resultPalab)
###


frec[which(frec > 10)]  #Muestra la frecuencia de las palabras que estan presentes mas de 10 veces en todas las muestras
table (rowSums (mat))  #Esto nos muestra cuantas cabeceras hay solo en x ficheros (cuantas en 1, cuantas en 2, cuantas en 4 etc.)
rownames (mat)[touse]  #Muestra los nombres de las cabeceras guardadas en touse

rownames (mat)[rowSums (mat) == length(ficheros)] #Para mirar cuales estan en cuantos. Total archivos = 32

#Mirar el contenido de cabeceras seleccionadas
for (fi in ficheros) {
  print(fi)
  try(print(tablist[[fi]][, "icd_o_3_histology"]) , silent = TRUE)
}

#Mirar de cada una de las cabeceras que salen en cuantos sitios están presentes? O hacer q se muestren si aparecen en mas del 50% o así?

####Controlar las cabeceras que salen en cuantos ficheros estan
touse <- rowSums (mat) == length(ficheros) #Guardamos solo las cabeceras que esten en todos losficheros
tamanyofich <- length(ficheros)
##Voy a poner un bucle para incluir datos hasta tener un numero aceptable (digamos 10):
while(table(touse)[[2]] < 15) {   #Mientras que el numero de TRUE en touse sea menor que 10
  touse[rowSums (mat) == tamanyofich - 1] = TRUE
  tamanyofich <- tamanyofich - 1
}  

touse <- rowSums (mat) >= 15
rownames (mat) [touse]

lon <- sort (rowSums (mat))
lon
cbind (names (lon), lon)

