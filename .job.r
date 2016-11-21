## .job.r
## 2010-03-09 dmontaner@cipf.es
## Modificado: 2016-02-08 julenmendieta92@gmail.com
##    Le voy a añadir lineas para que cree el las carpetas, y así ahorramelo
## script that keeps the settings for each job
## bear in mind that it is dependent on the data folder structure

## .NAME: name of the people for whom the analysis is done.
##        It will be used as the JOB_NAME (no spaces; always lower case, etc...)

## dir$data: path to the directory where all data (raw, generated and results) are stored.
##          The last directory in the path has to be JOB_NAME.

## dir$code: path to the directory where scripts, sample information and documentation are stored.
##          The last directory in the path has to be JOB_NAME.

## NOTE: datadir & codedir may be different or the SAME.

################################################################################

.NAME = "job001" #job name. NO SPACES

################################################################################

.job <- list ()
.job$name <- .NAME
.job$dir <- list ()

# We save the year to be used later
# Line to be changed ###
user = "/home/user/julen"
#
year = date ()
year = unlist(strsplit(year, " "))
year = year[length(year)]
suppressWarnings(dir.create(file.path (user, "trabajos", year)))
suppressWarnings(dir.create(file.path (user, "datos", year)))

### rootDir: Location in MY COMPUTER
.job$dir$data <- file.path (user, "datos", year, .job$name) #starting with ("~") if working in your home directory
suppressWarnings(dir.create(file.path (user, "datos", year, .job$name)))
.job$dir$code <- file.path (user, "trabajos", year, .job$name) #or ("") if working in the root directory !!!
suppressWarnings(dir.create(file.path (user, "trabajos", year, .job$name)))

# .job$dir$data <- file.path ("~", "Desktop", "papers", .job$name, "datos") #starting with ("~") if working in your home directory
# .job$dir$code <- file.path ("~", "Desktop", "papers", .job$name) #or ("") if working in the root directory !!!

# from cluster:
# .job$dir$data <- file.path ("~", .job$name, "datos") #starting with ("~") if working in your home directory
# .job$dir$code <- file.path ("~", .job$name) #or ("") if working in the root directory !!!



### MORE directories
.job$dir$scripts <- file.path (.job$dir$code, "scripts")
suppressWarnings(dir.create(file.path (.job$dir$code, "scripts")))
.job$dir$docs    <- file.path (.job$dir$code, "documents")
suppressWarnings(dir.create(file.path (.job$dir$code, "documents")))

.job$dir$rawdat      <- file.path (.job$dir$data, "data_raw")
suppressWarnings(dir.create(file.path (.job$dir$data, "data_raw")))
.job$dir$annotation  <- file.path (.job$dir$data, "data_annotation")
suppressWarnings(dir.create(file.path (.job$dir$data, "data_annotation")))
.job$dir$proces      <- file.path (.job$dir$data, "data_processed")
suppressWarnings(dir.create(file.path (.job$dir$data, "data_processed")))

#.job$dir$plots  <- file.path (.job$dir$data, "results", "plots")
#.job$dir$res    <- file.path (.job$dir$data, "results", "files")

### Some other parameters
.job$testmode <- FALSE ##testing mode
.job$dec <- "."
.job$idsep <- " /// "  ##separates IDs

################################################################################

rm (list = ".NAME")

##MESSAGE
cat ("\n.job.r has been sourced\n", fill = TRUE)
