### 
# This is my version of the Benoit data format script

# Loading in the packages and loading the functions, and data structure
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, icesTAF, suncalc)

rm(list=ls())

sourceDir <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  sapply(files, source)
}
sourceDir(file.path('.','Function'))

figurePath    <- file.path('.','Figures')
dataPath      <- file.path('.','Data')
resultPath    <- file.path('.','Results')

# Loading the data on WBAT and overview into R
WBAT.tab <- read.csv(file.path(dataPath,'survey_db.csv'))                       # Creating WBAT.tab from survey_db.csv
WBAT.tab <- WBAT.tab[,c(1:5,8,9)]
WBAT.tab$dataSet_station <- paste0(WBAT.tab$dataSet,'_',WBAT.tab$station)

overview.tab <- read.csv(file.path(dataPath,'data_overview.csv'))               # Creating overview.tab from data_overview.csv
overview.tab$stationSet <- paste0(overview.tab$dataSet,'_',overview.tab$station)
overview.tab$pairingName <- paste0(overview.tab$dataSet,'_',overview.tab$pairing)
overview.tab <- overview.tab %>% select(-c('year','dataSet','station'))

## Reading the CPOD data 
processFlag <- T


load(file.path(dataPath, 'CPOD_all.RData'))

CPOD.all_subset100 <- CPOD.all %>% slice(1:10000)

CPOD.all_subset100$timeIci_str <- as.POSIXct(CPOD.all_subset100$timeIci_str, tz='UTC')
CPOD.all$timeIci_str <- as.POSIXct(CPOD.all$timeIci_str, tz='UTC')

str(CPOD.all)
sum(is.na(CPOD.all$timeIci_str))

invalid_dates <- CPOD.all$timeIci_str[is.na(as.POSIXct(CPOD.all$timeIci_str, format="%Y-%m-%d %H:%M:%S", tz="UTC"))]
invalid_dates

# Check for entries that do not match the expected date-time pattern
invalid_rows <- !grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", CPOD.all$timeIci_str)
invalid_dates <- CPOD.all$timeIci_str[invalid_rows]
invalid_dates

CPOD.all$timeIci_str[invalid_rows] <- NA
CPOD.all$timeIci_str <- as.POSIXct(CPOD.all$timeIci_str, format="%Y-%m-%d %H:%M:%S", tz="UTC")



# Define the expected format as a regular expression
expected_format <- "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"

# Use grepl to check which rows match the expected format
valid_format <- grepl(expected_format, CPOD.all$timeIci_str)

# Count the number of rows that do NOT match the expected format
num_invalid <- sum(!valid_format)
cat("Number of invalid date-time entries:", num_invalid, "\n")



CPOD.all <- CPOD.all %>% 
  separate(PODid, into = c("POD", "dataSet", "station"), sep = "_")
CPOD.all$station[CPOD.all$station == '267878'] <- '267838'




if(processFlag){
load(file.path(dataPath,'CPOD_all.RData'))

CPOD.all <- CPOD.all %>% separate(PODid,into=c("POD","dataSet", "station"),sep = "_")
CPOD.all$station[CPOD.all$station == '267878'] <- '267838'                    # Waarschijnlijk stond er een foutje in de cijferreeks, maar waarom staan deze cijfers uberhaupt in de naam
CPOD.all$timeIci_str <- as.POSIXct(CPOD.all$timeIci_str,tz='UTC')

CPOD.all$dataSet_station <- paste0(CPOD.all$dataSet,'_',CPOD.all$station)
CPOD.all$timeIci_num <- as.numeric(CPOD.all$timeIci_str)
}






















