## e000_tabla_clinicos24_03.r
## 2015-03-26 julenmendieta92@gmail.com
## Script sin uso. Es el script completo del análisis de los datos clínicos del que he extraido los cripts útiles

date ()
Sys.info ()[c("nodename", "user")]
commandArgs ()
rm (list = ls ())
R.version.string ##"R version 3.1.2 (2014-10-31)"

setwd ("/home/jmendieta/Documents/untitled folder")

ficheros <- dir (pattern = "clinical_patient", recursive= TRUE)  #Guardamos en ficheros el stream de cada carpeta y los ficheros que contiene
ficheros
length (ficheros)
#Igual hay que crear una matriz de length(ficheros) x 8 (las 8 columnas q interesan de momento)


tablist <- list ()
lnombres <- list ()
for (fi in ficheros) {
  print (fi)
  
  nombres <- unlist (strsplit (readLines (fi)[1], split = "\t"))
  datos0 <- read.table (fi, header = TRUE, sep = "\t", quote = "", col.names = nombres, as.is = TRUE, 
                        na.strings = c("[Not Available]", "[Not Applicable]", "[Not Evaluated]", "[Unknown]"), skip = 3)
  
  datos0[1:3,]
  orden <- order (colnames (datos0))
  colnames (datos0)[orden]
  datos0 <- datos0[,orden]
  
  #
  table (sapply (datos0, class))
  #Con esto leemos todo lo que hay en el fichero
  todo.na <- apply (is.na (datos0), 2, all)
  table (todo.na)
  datos0 <- datos0[,!todo.na]  #Quitamos los datos en los que todo sea NA
  
  lon <- apply (datos0, 2, function (x) length (setdiff (unique (x), NA)))  #Guardamos en lon cuantos datos diferentes hay en cada cabecera
  table (lon)
  
  #Antes de nada copiamos la columna de genero no sea que se borre al aplicar el filtro
  genero <- datos0["gender"]
  #Eliminamos las cabeceras que tengan menores de 1 y mayores o iguales de 10 
  datos0 <- datos0[,(lon > 1  & lon < 10)] 
  #Y nos aseguramos de que la cebecera de genero permanezca
  datos0["gender"] <- genero
  
  datos0[1:3,]
  t(t (colnames (datos0)))  #Muestra una lista con las columnas que hay en datos0
  
  
  
  #El objetivo de esto es quitar lo de sindatos si hay muchos de esos valores 
  mitad <- length(rownames(datos0))/2
  
  #Con esto nos aseguramos de quitar las columnas donde faltan datos en muchos campos, pero a la hora de guardarlas no aparecera todo como NA, lo cual es mejor
  for (ca in colnames(datos0)) {
    cantidadSinDatos <- table(datos0$ca == "NA")  #Esto nos da cuantos no cumplen esto
    try(if (cantidadSinDatos[1] < mitad | length(cantidadSinDatos) == 0) (datos0$ca <- NULL), silent = FALSE)  #Si la cantidad de datos reales es inferior a la mitad de datos totales borramos la columna
    #A su vez, si la tabla es de longitud cero es que todos los datos son NA, por lo que también borramos
    #Con esto vamos quitando donde se encuentre sindatos en un % mayor al 50%. Ponemos try por si no se encuentra ninguna vez sindatos
  }
  
  nombres <- colnames(datos0)   #Con esto miramos todas las cabeceras que han permanecido hasta ahora
  lnombres[[fi]] <- nombres  #Guardamos las cabeceras que hay asociadas al nombre del fichero en lnombres
  
}


nu <- sort (unique (unlist (lnombres)))   #Guardamos en una lista todas las cabeceras de columnas que hay sin repetirlas
mat <- matrix (NA, nrow = length (nu), ncol = length (ficheros))  #Creamos una matriz vacia con numero de filas igual a cabeceras posibles y en las columnas el nombre del fichero
rownames (mat) <- nu  #Llamamos a las filas de mat como las cabeceras
colnames (mat) <- ficheros  #A las columnas como los ficheros
#Con el bucle guardamos un T o F dependiendo de si en el fichero hay una cabecera que se llame como fila[cabecera] de la matriz
for (fi in ficheros) {
  mat[,fi] <- rownames (mat) %in% lnombres[[fi]]
}
mat[1:3,]
dim (mat)
table (rowSums (mat))  #Esto nos muestra cuantas cabeceras hay solo en x ficheros (cuantas en 1, cuantas en 2, cuantas en 4 etc.)

colnames (mat) <- NULL
touse <- rowSums (mat) == length(ficheros) #Guardamos solo las cabeceras que esten en todos losficheros
touse
mat[touse,]
rownames (mat)[touse]  #Muestra los nombres de las cabeceras q estan en todos los ficheros

for (fi in ficheros) {
  nombres <- unlist (strsplit (readLines (fi)[1], split = "\t"))
  datos0 <- read.table (fi, header = TRUE, sep = "\t", quote = "", col.names = nombres, as.is = TRUE, na.strings = "[Not Available]", skip = 3)
    #Con esto miramos cuantas columnas de las presentes en todos los datos tenemos hasta ahora
  table (colnames(datos0) %in% rownames (mat)[touse])
  mantener <- colnames(datos0) %in% rownames (mat)[touse]
  #Y las guardamos
  datos0 <- datos0[mantener]
  ##Hasta aqui
  
  print (dim (datos0))
  tablist[[fi]] <- datos0
  
  #Con este bucle guardamos en una lista todos los datos que hay en cada fichero
}



summary(tablist)

#Guardamos en excel
setwd ("/home/jmendieta/Documents")

library (xlsx)

####Con esto guardamos en hojas diferentes

primeraVuelta <- TRUE
for (fi in ficheros) {
  if (primeraVuelta == TRUE) {
    #En la primera vuelta guardamos el primer fichero 
    write.xlsx2(x = tablist[[fi]], file = "tabla_clinicos.xlsx",
               sheetName = fi, col.names = TRUE, row.names = FALSE)
    #Y guardamos el orden de las columnas de este fichero
    ordencol <- colnames(tablist[[fi]])
    primeraVuelta <- FALSE
  }
  else {
    #Con esto guardamos las demas tablas
    write.xlsx2(x = tablist[[fi]][c(ordencol)], file = "tabla_clinicos.xlsx",
               sheetName = fi, col.names = TRUE, row.names = FALSE, append = TRUE)
  } 
}

#Con esto se guardaria todo, pero da error por diferente numero de lineas en cada fi
#write.xlsx2(x = tablist, file = "tabla_clinicos2.xlsx",
#           sheetName = fi, col.names = TRUE, row.names = FALSE)



####Guardar todo en la misma hoja
library(XLConnect)


primeraVuelta <- TRUE
for (fi in ficheros) {
  if (primeraVuelta == TRUE) {
    #En la primera vuelta guardamos el primer fichero
    write.xlsx2(x = t(colnames(tablist[[fi]])), file = "tabla_clinicos2.xlsx",
                sheetName = "Datos", col.names = FALSE, row.names = FALSE)
    wb <- loadWorkbook( "tabla_clinicos2.xlsx" )
    appendWorksheet( wb, data = fi, sheet = "Datos" ) 
    appendWorksheet( wb, data = tablist[[fi]], sheet = "Datos" ) 
    #Guardamos el orden de las columnas de este fichero
    ordencol <- colnames(tablist[[fi]])
    primeraVuelta <- FALSE
  }
  else {
    #Con esto guardamos las demas tablas
    appendWorksheet( wb, data = fi, sheet = "Datos" ) 
    appendWorksheet( wb, data = tablist[[fi]][c(ordencol)], sheet = "Datos" ) 
    
  } 
}
saveWorkbook( wb )
