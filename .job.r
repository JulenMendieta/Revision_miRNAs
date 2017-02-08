## .job.r
## 2010-03-09 dmontaner@cipf.es
## Modificado: 2017-02-08 julenmendieta92@gmail.com
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
if(!file.exists(file.path (user, "trabajos", year))){dir.create(file.path (user, "trabajos", year))}
if(!file.exists(file.path (user, "datos", year))){dir.create(file.path (user, "datos", year))}

### rootDir: Location in MY COMPUTER
.job$dir$data <- file.path (user, "datos", year, .job$name) #starting with ("~") if working in your home directory
if(!file.exists(file.path (.job$dir$data))){dir.create(file.path (.job$dir$data))}
.job$dir$code <- file.path (user, "trabajos", year, .job$name) #or ("") if working in the root directory !!!
if(!file.exists(file.path (.job$dir$code))){dir.create(file.path (.job$dir$code))}

# .job$dir$data <- file.path ("~", "Desktop", "papers", .job$name, "datos") #starting with ("~") if working in your home directory
# .job$dir$code <- file.path ("~", "Desktop", "papers", .job$name) #or ("") if working in the root directory !!!

# from cluster:
# .job$dir$data <- file.path ("~", .job$name, "datos") #starting with ("~") if working in your home directory
# .job$dir$code <- file.path ("~", .job$name) #or ("") if working in the root directory !!!



### MORE directories
.job$dir$scripts <- file.path (.job$dir$code, "scripts")
if(!file.exists(file.path (.job$dir$scripts))){dir.create(file.path (.job$dir$scripts))}
.job$dir$docs    <- file.path (.job$dir$code, "documents")
if(!file.exists(file.path (.job$dir$docs))){dir.create(file.path (.job$dir$docs))}

.job$dir$rawdat      <- file.path (.job$dir$data, "data_raw")
if(!file.exists(file.path (.job$dir$rawdat))){dir.create(file.path (.job$dir$rawdat))}
.job$dir$annotation  <- file.path (.job$dir$data, "data_annotation")
if(!file.exists(file.path (.job$dir$annotation))){dir.create(file.path (.job$dir$annotation))}
.job$dir$proces      <- file.path (.job$dir$data, "data_processed")
if(!file.exists(file.path (.job$dir$proces))){dir.create(file.path (.job$dir$proces))}

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
