## e030_estadistica_columnas_datos_clinicos.r
## 2015-03-26 julenmendieta92@gmail.com
## Script para revisar los datos clinicos generados en "e010_tabla_datos_clinicos.r" y ver que columnas hay y su presencia en cada estudio
date ()
Sys.info ()[c("nodename", "user")]
commandArgs ()
rm (list = ls ())
R.version.string ##"R version 3.1.2 (2014-10-31)"

setwd ("/home/jmendieta/Documents/revision_mirnas/datos/procesados")
datosClinicos <- readRDS("e010_tabla_datos_clinicos.RData")
lnombres <- list()
for (id in names (datosClinicos)) {
  datos0 <- datosClinicos[[id]]
  todo.na <- apply (is.na (datos0), 2, all)
  table (todo.na)
  datos0 <- datos0[,!todo.na]  #Quitamos los datos en los que todo sea NA
     
  lon <- apply (datos0, 2, function (x) length (setdiff (unique (x), NA)))  #Guardamos en lon cuantos datos diferentes hay en cada cabecera
  table (lon)
  nombres <- colnames(datos0)   #Con esto miramos todas las cabeceras que han permanecido hasta ahora
  lnombres[[id]] <- nombres     #Guardamos las cabeceras que hay asociadas al nombre del fichero en lnombres
}


nu <- sort (unique (unlist (lnombres)))   #Guardamos en una lista todas las cabeceras de columnas que hay sin repetirlas
mat <- matrix (NA, nrow = length (nu), ncol = length (datosClinicos))  #Creamos una matriz vacia con numero de filas igual a cabeceras posibles y en las columnas el nombre del fichero
rownames (mat) <- nu  #Llamamos a las filas de mat como las cabeceras
colnames (mat) <- names(datosClinicos)  #A las columnas como los ficheros

#Con el bucle guardamos un T o F dependiendo de si en el fichero hay una cabecera que se llame como fila[cabecera] de la matriz
for (id in names (datosClinicos)) {
  mat[,id] <- rownames (mat) %in% lnombres[[id]]
}
mat[1:3,]
dim (mat)
table (rowSums (mat))  #Esto nos muestra cuantas cabeceras hay solo en x ficheros (cuantas en 1, cuantas en 2, cuantas en 4 etc.)
###De aqui guardamos la matriz mat, ya que de ella se extrae cuantas columnas hay en cada fichero
saveRDS(mat, file="e030_estadistica_columnas_datos_clinicos.RData")
