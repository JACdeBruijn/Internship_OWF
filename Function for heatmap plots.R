################################################################################
# Script for plotting heatmaps of depth over time
# For 1 dataset, yet all variables such as threshold, freq, 4 levels of data drop
################################################################################

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)

rm(list=ls())

sourceDir <- function(directory) {
  files <- list.files(directory, pattern = "\\.R$", full.names = TRUE)
  for (file in files) {
    source(file)
  }
}

sourceDir(file.path('.','Function'))
figurePath    <- file.path('.','Figures')
dataPath      <- file.path('.','Data')
resultPath    <- file.path('.','Results')

################################################################################
# Loading in the overview data sets
################################################################################
WBAT.tab <- read.csv(file.path(dataPath,'survey_db.csv'))                 
WBAT.tab <- WBAT.tab[,c(1:5,8,9)]
WBAT.tab$dataSet_station <- paste0(WBAT.tab$dataSet,'_',WBAT.tab$station)
WBAT.tab <- WBAT.tab %>% 
  filter(!(dataSet %in% c("2022-cpower", "2022-HKZ", "2023-HKN")))

overview.tab <- read.csv(file.path(dataPath,'data_overview.csv'))               # Creating overview.tab from data_overview.csv
overview.tab$stationSet <- paste0(overview.tab$dataSet,'_',overview.tab$station)
overview.tab$pairingName <- paste0(overview.tab$dataSet,'_',overview.tab$pairing)
overview.tab <- overview.tab %>% 
  select(-c('year','dataSet','station')) %>%
  select(stationSet, everything()) %>% 
  filter(pairing != "-1")

################################################################################
# Loading in the correct WBAT data sets
################################################################################

for(stationSet in unique(WBAT.tab$dataSet_station)){                              # Loop that will do everything for each unique dataSet_station that does also occur in overview.tab
  print(stationSet)
  tab.filt <- WBAT.tab[WBAT.tab$dataSet_station == stationSet,]
  
  for(idxDataSet in tab.filt$surveName){                                          # Reading all the data on station and data set for pelegc fish data
    print(idxDataSet)
    load(file.path(dataPath,paste0('WBAT_',idxDataSet,'.RData')))
    
  }
}










  
################################################################################
# This is for later use
  if(flagFirst){
    df.join.all <- df.join
    flagFirst <- F
  }else{
    df.join.all <- rbind(df.join.all,df.join)
  }