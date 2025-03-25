###
## 1_RawDataFormat - CPOD-WBAT data ----
# I have worked through this script - 14/10/2024
# Purpose of script:  It is for pre-processing the RAW data gained from the Cpods and Echosounders. 
#                     The output are the current data set that I got from Benoit!!!!!!
###

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, data.table)

rm(list=ls())

#setwd('D:/PAM_RABO/finless_neonate_HK')        # Setting the active working directory, either D, Z or G drive
# setwd('Z:/APELAFICO/')
#setwd('G:/git/WBAT_APELAFICO')

figurePath    <- file.path('.','figures')       # Making a relative filepath. The '.' stands for the current wd or working directory and figures is the map 
dataPath      <- file.path('.','data')
resultPath    <- file.path('.','results')

processCPOD <- F                                # This is called flagging!!! If in a funtion if(processCPOD) will be used this will be skipped!!!!!
processWBAT <- T                                # This is extremely handy to turn off sections of script

############################################################################--
## Read CPOD files----
############################################################################--

if(processCPOD){                                                                # Example: This script will be skipped as processCPOD is turned off!!!!!!!!!
  fileList <- list.files(file.path(dataPath,'CPOD'),pattern="*.csv")            # Making a file list, in the current wd in map data in submap CPOD all *.csv files will be loaded
  
  flagFirst <- T                                                                # Here FlagFirst is set to True, as this will be the first run
  
  for(file in fileList){                                                        # For all files in the list of file names do...
    CPOD.current              <- fread(file.path(dataPath,'CPOD',file))         # Read all files 
    temp.list <- strsplit(strsplit(file,'.csv')[[1]],'_')[[1]]                  # Get information from the file name, .csv is removed and everything is split on "_", then individual words are further processed
    CPOD.current$dataSet <- temp.list[2]                                        # New column is made for second string (which is dataset name)
    CPOD.current$station <- temp.list[3]                                        # New column is made for third sting (which is station name)
    
    if(flagFirst){                                                              # When FlagFirst is T then go, else do rebind. This is for the first iteration
      CPOD.all <- CPOD.current
      flagFirst <- F
    }else{
      CPOD.all <- rbind(CPOD.all,CPOD.current)                                  # Second iteration will be performed with a rebind instead of filling an empty data set
    }
  }
  
  save('CPOD.all',
       file = file.path(dataPath,'CPOD_all.RData'))                             # Save in DATA, CPOD_all.rdata
}

############################################################################--
## Read WBAT exports----
############################################################################--

if(processWBAT){
  dirList <- list.files(file.path(dataPath,'WBAT'))
  
  for(myDir in dirList[c(43:46)]){
    print(myDir)
    flagFirst <- T
    fileList <- list.files(file.path(dataPath,'WBAT',myDir),pattern="*.txt")
    
    # dir.string <- strsplit(myDir,'_')[[1]]
    # dir.dataSet <- dir.string[1]
    # dir.dataSet <- gsub("-", "", dir.dataSet)
    # dir.phase <- dir.string[2]
    # dir.station <- dir.string[3]
    # dir.frequency   <- dir.string[4]
    
    for(file in fileList){
      print(file)
      temp.list   <- strsplit(strsplit(file,'.txt')[[1]],'_')[[1]]
      if(length(temp.list) == 6){
        dataSet <- temp.list[1]
        phase   <- 'P1'
        station <- temp.list[2]
        treshold    <- as.numeric(strsplit(temp.list[4],'=')[[1]][2])
        frequency   <- as.numeric(strsplit(temp.list[3],'khz')[[1]][1])
        chunk       <- as.numeric(strsplit(temp.list[5],'=')[[1]][2]) 
      }else if(length(temp.list) == 7){
        dataSet <- temp.list[1]
        phase   <- temp.list[2]
        station <- temp.list[3]
        treshold    <- as.numeric(strsplit(temp.list[5],'=')[[1]][2])
        frequency   <- as.numeric(strsplit(temp.list[4],'khz')[[1]][1])
        chunk       <- as.numeric(strsplit(temp.list[6],'=')[[1]][2]) 
      }
      WBAT.current              <- fread(file.path(dataPath,'WBAT',myDir,file),skip="DATE")
      
      # fix of time strings  
      n.char <- apply(WBAT.current[,'TIME'],1,nchar)
      WBAT.current$TIME[n.char == 7] <- paste0('0',WBAT.current$TIME[n.char == 7])
      WBAT.current$TIME[n.char == 5] <- paste0('000',WBAT.current$TIME[n.char == 5])
      WBAT.current$TIME[n.char == 4] <- paste0('0000',WBAT.current$TIME[n.char == 4])
      
      WBAT.current$datetime <- as.POSIXct(paste0(WBAT.current$DATE,'T',WBAT.current$TIME),format="%Y%m%dT%H%M%S",tz="UTC")
      
      # break into intervals
      thresh <- 60*5
      WBAT.current <- WBAT.current %>% 
        arrange(datetime) %>%
        mutate(timediff = difftime(datetime, lag(datetime), units = 'secs'))
      
      WBAT.current <- WBAT.current %>%
        mutate(timediff = ifelse(is.na(timediff), 1, timediff)) %>%
        mutate(thresh_pass = timediff > thresh,
               thresh_pass = as.numeric(thresh_pass)) %>%  # true = 1
        mutate(thresh_pass = ifelse(timediff==1, 1, thresh_pass))
      
      WBAT.current$IDinter <- c(cumsum(WBAT.current$thresh_pass))
      
      # tidy up data frame
      WBAT.current <- WBAT.current[WBAT.current$NoPelagic > 0,]
      WBAT.current <- WBAT.current %>% dplyr::select(-thresh_pass,-timediff,-DATE,-TIME,-Latitude,-Longitude,-START_LOG,-Species,-Frequency,-MinDepth,-MaxDepth,-Transcei.,-Threshold,-BottomUpper,-B_CH0)
      idxColDepth <- str_detect(colnames(WBAT.current), 'P_CH')
      colnames(WBAT.current)[idxColDepth] <- as.numeric(sapply(strsplit(colnames(WBAT.current)[idxColDepth],'P_CH'), "[[", 2))
      WBAT.current <- WBAT.current %>% pivot_longer(!datetime & !IDinter & !STOP_LOG & !ScatterPCT & !NoPelagic,names_to = 'depth',values_to = 'SA')
      WBAT.current$depth <- as.numeric(WBAT.current$depth)*WBAT.current$ScatterPCT
      WBAT.current$depthIntegration <- WBAT.current$NoPelagic*WBAT.current$ScatterPCT
      WBAT.current <- WBAT.current[WBAT.current$SA != -1,]
      
      WBAT.current$dataSet  <- dataSet
      WBAT.current$phase    <- phase
      WBAT.current$station  <- station
      WBAT.current$frequency  <- frequency
      WBAT.current$treshold   <- treshold
      WBAT.current$chunk      <- chunk
      
      if(flagFirst){
        WBAT.all <- WBAT.current
        flagFirst <- F
      }else{
        WBAT.all <- rbind(WBAT.all,WBAT.current)
      }
    }
    
    # temp <- subset(WBAT.all,treshold == -50)
    # temp <- subset(temp,depth == 0)
    # 
    # max(temp$datetime,na.rm=T)
    # 
    # ggplot(temp,aes(x=datetime,y=log10(SA)))+
    #   geom_line()
    
    save('WBAT.all',
         file = file.path(dataPath,paste0('WBAT_',myDir,'.RData')))
  } 
}
# 
# load(file.path(resultPath,'WBAT_2023-BSW_278093 BSW2_70khz.RData'))
# WBAT.join <- WBAT.all
# 
# load(file.path(resultPath,'WBAT_2021-BE_P1_grafton_200khz.RData'))
# WBAT.join <- rbind(WBAT.join,WBAT.all)
# 
# WBAT.join <- subset(WBAT.join,depth == 0)
# 
# WBAT.summary <- WBAT.join %>% group_by(treshold,IDinter,frequency,chunk) %>% summarize(SA=mean(SA,na.rm=T),
#                                                                               datetime=first(datetime,na.rm=T),
#                                                                               depthIntegration=first(depthIntegration,na.rm=T))
# 
