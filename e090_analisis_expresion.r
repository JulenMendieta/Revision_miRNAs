-Hacer un script para analisis de expresión de miRNAs con edger.cla  https://github.com/dmontaner-papers/gsa4mirna/blob/gh-pages/scripts/f020_edgeR_unpaired.r
-Hacer un script para analisis de expresión de proteinas con ?wilcox.test
-Hacer un analisis de correlación entre miR y proteinas con cor ()
## e090_analisis_expresion.r
## 2015-06-05 julenmendieta92@gmail.com
##Modificado: 
## Script para comparar realizar análisis de expresión de miR con edger.cla, de proteinas con wilcox.test y correlación entre ambos

date ()
Sys.info ()[c("nodename", "user")]
commandArgs ()
rm (list = ls ())
R.version.string ##"R version 3.2.0 (2015-04-16)"
library (edgeR); packageDescription ("edgeR", fields = "Version") #"3.10.1"

try (source (".job.r")); try (.job)

options (width = 170)
#options (width = 1000)

setwd (file.path (.job$dir$proces))


#Ahora le añadimos los datos

load (file.path (.job$dir$proces, "clinicos_miryprot_LGG.RData"))   #Con esto cargamamos sinfo
load (file.path (.job$dir$proces, "miR_exp_LGG.RData"))  #Con esto cargamos miR.exp.info y mirna
load (file.path (.job$dir$proces, "prot_exp_LGG.RData"))  #Con esto cargamos  prot.exp.info y prots

#Empezamos con la correlación
cor(mirna, prots)
#dimensiones incompatibles

##########################################################################################################################################3
##Vamos con los miR
#Hacer un script para analisis de expresión de miRNAs con edger.cla  https://github.com/dmontaner-papers/gsa4mirna/blob/gh-pages/scripts/f020_edgeR_unpaired.r
#En lugar de casos controles aqui tenemos el genero, por lo q primero eliminamos los datos que no se pueden asociar a un sexo en las 3 variables
no.sex <- is.na(sinfo[,"gender"]) 
#Se eliminan de los datos clincios
sinfo <- sinfo[-which(no.sex == TRUE),]
#Se eliminan de miR
mirna <- mirna[,no.sex==FALSE]
#Se eliminan de prots
prots <- prots[,no.sex==FALSE]

res.edger <- list ()

## at least RPKM or similar will be needed before the fold change is computed
media0 <- rowMeans (mirna[,sinfo$gender == "MALE", drop = FALSE])
media1 <- rowMeans (mirna[,sinfo$gender == "FEMALE", drop = FALSE])
mifold <- log2 ((media1 + 0.001) / (media0 + 0.001))

## edgeR classic analysis
clase <- sinfo$gender   ##does not need to be a factor
edger.cla <- DGEList (counts = mirna, group = clase)
edger.cla <- calcNormFactors     (edger.cla)       ## HAS TO BE COMPUTED IN THIS ORDER
edger.cla <- estimateCommonDisp  (edger.cla)
edger.cla <- estimateTagwiseDisp (edger.cla)
edger.cla.res <- exactTest       (edger.cla)
##topTags (edger.cla.res)
print (cor (mifold, edger.cla.res$table[,"logFC"], use = "pairwise.complete.obs"))
res.edger <- edger.cla.res

names (res.edger)

## SAVING
save (list = "res.edger", file = file.path (.job$dir$proces, "res_dif_exp_miR_unpaired.RData"))

################################################################################################################################################

#Ahora las proteinas
#analisis de expresión de proteinas con ?wilcox.test
wilcox.test(x = prots[,sinfo$gender == "MALE"], y = prots[,sinfo$gender == "FEMALE"],
            alternative = c("two.sided", "less", "greater"),
            mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
            conf.int = FALSE, conf.level = 0.95)

###EXIT
warnings ()
sessionInfo ()
q ("no")


