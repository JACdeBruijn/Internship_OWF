############################################################################--
## 2_DataFormat - CPOD-WBAT workspace ----
## Script functionality: Creating a working environment for the data analysis of SA and CPOD
############################################################################--

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, icesTAF, suncalc)

rm(list=ls())
getwd()

sourceDir(file.path('.','function'))
figurePath    <- file.path('.','figures')
dataPath      <- file.path('.','data')
resultPath    <- file.path('.','results')

# Loading the META data
WBAT.tab <- file.path(dataPath, 'survey_db.csv') %>%                            # Creating WBAT.tab from survey_db.csv
  read_csv() %>%
  select(1:5, 8, 9) %>%
  mutate(dataSet_station = str_c(dataSet, '_', station))

overview.tab <- file.path(dataPath, 'data_overview.csv') %>%                    # Creating overview.tab from data_overview.csv
  read_csv() %>% 
  mutate(stationSet = str_c(dataSet, '_', station),
         pairingName = str_c(dataSet, '_', pairing)) %>% 
  select(-c('year', 'dataSet', 'station'))

processFlag <- T
filterSA    <- F

############################################################################--
## Filtering or not ----
############################################################################--

if(filterSA){
  saveName <- 'CPOD_WBAT_workspace.filt.RData'
}else{
  saveName <- 'CPOD_WBAT_workspace.RData'
}

############################################################################--
## Read data and plot ----
############################################################################--

if(processFlag){
  load(file.path(dataPath,'CPOD_all.RData'))
                                          
  CPOD.all <- CPOD.all %>% separate(PODid,into=c("POD","dataSet", "station"),sep = "_") # Splitting columns
  CPOD.all$station[CPOD.all$station == '267878'] <- '267838'                    # Waarschijnlijk stond er een foutje in de cijferreeks, maar waarom staan deze cijfers uberhaupt in de naam
  CPOD.all <- subset(CPOD.all,timeIci_str != 'NaT')                             # I think this fixes the earlier issue I had with the CPOD data
  CPOD.all$timeIci_str <- as.POSIXct(CPOD.all$timeIci_str,tz='UTC')             # Omzetten tijd, maar hier gaat dus iets mis in de data

  CPOD.all$dataSet_station <- paste0(CPOD.all$dataSet,'_',CPOD.all$station)     # Combine 2 columns into 1 additional new column 
  
  WBAT.tab <- WBAT.tab[WBAT.tab$SA_exported == 1,]                              # Load all survey data 
  
  flagFirst.all <- T
  for(stationSet in c(unique(WBAT.tab$dataSet_station))){                       # Loop that will do everything for each unique dataSet_station that does also occur in overview.tab
    
    if (grepl('2022-cpower|2022-HKZ|2023-HKN', stationSet)) {
      next                                                                      # Skipping the unneeded data sets
    }
    
    print(stationSet)
    tab.filt <- WBAT.tab[WBAT.tab$dataSet_station == stationSet,]
    
    if(dim(tab.filt)[1] != 0){
      # Read all data in the set of station+data set
      flagFirst <- T
      for(idxDataSet in tab.filt$surveName){                                    # Reading all the data on station and data set for pelegc fish data
        print(idxDataSet)
        
        if (grepl('^2021-BE_cpower$', stationSet)) {                            # Since 2021-BE_cpower has no data it need to be skipped             
          next 
        } else {
          load(file.path(dataPath,paste0('WBAT_',idxDataSet,'.RData')))
        }
        
        # WBAT.all <- WBAT.all[!is.na(WBAT.all$datetime),]      # Here I manage to get all the datetimes, so also the ones that nothing was measured
        WBAT.all$stationSet  <- paste0(WBAT.all$dataSet,'_',WBAT.all$station,'_',WBAT.all$phase)
        WBAT.all$IDinter <- NA
        WBAT.all <- subset(WBAT.all, depth != 0 & SA != 0 & !(treshold >= -59 & treshold <= -51))                          
        # This is a biggie - I just rip out all thresholds and only keep thresholds 50 and 60 which should reduce computational time
        
        # break into intervals
        for(idxTreshold in unique(WBAT.all$treshold)){
          idxFilt <- WBAT.all$treshold == idxTreshold
          WBAT.current <- WBAT.all[idxFilt,]
          
          idxOrder <- order(WBAT.current$datetime)
          thresh <- 60*6
          WBAT.current <- WBAT.current[idxOrder,] %>% 
            mutate(timediff = difftime(datetime, lag(datetime), units = 'secs'))
          
          WBAT.current <- WBAT.current %>%
            mutate(timediff = ifelse(is.na(timediff), 1, timediff)) %>%
            mutate(thresh_pass = timediff > thresh,
                   thresh_pass = as.numeric(thresh_pass)) %>%  # true = 1
            mutate(thresh_pass = ifelse(timediff==1, 1, thresh_pass))
          
          WBAT.current$IDinter_num <- c(cumsum(WBAT.current$thresh_pass))
          WBAT.current$IDinter <- paste0(WBAT.current$stationSet,'_',idxTreshold,'_',WBAT.current$frequency,'_',WBAT.current$IDinter_num)
          
          WBAT.all$IDinter[which(idxFilt)[idxOrder]] <- WBAT.current$IDinter
          
          # Commenting out the filtering step (this is as an additional security in case if the filterSA flag is not working correctly)
          # upper_bound <- quantile(log10(WBAT.current$SA),0.75) #median(log10(WBAT.current$SA)) + 2 * mad(log10(WBAT.current$SA), constant = 1)
          # idxIn <- log10(WBAT.current$SA) > upper_bound # & log10(WBAT.current$SA) < 4.5
          
          if(filterSA){
            WBAT.all <- WBAT.all[-c(which(idxFilt)[idxOrder][!idxIn]),]
          }
        }
        
        WBAT.all.depth0 <- WBAT.all %>% 
          select(-c(depth)) %>% 
          group_by(STOP_LOG,ScatterPCT,NoPelagic,datetime,IDinter,
                   depthIntegration,dataSet,phase,station,frequency,treshold,
                   chunk, stationSet) %>% summarise(SA=sum(SA))
        
        WBAT.all.depth0$depth <- 0
        
        WBAT.all <- rbind(WBAT.all,WBAT.all.depth0)
        WBAT.all <- WBAT.all %>% arrange(treshold,datetime,depth)
        
        WBAT.depth <- subset(WBAT.all,depth != 0) %>% group_by(IDinter,frequency,treshold,datetime) %>% summarize(depthSA=sum(SA*depth)/sum(SA))
        WBAT.depth <- WBAT.depth[order(WBAT.depth$datetime),]
        
        WBAT.all <- subset(WBAT.all,depth == 0)
        if(dim(WBAT.all)[1] != dim(WBAT.depth)[1]){
          print('dimensions are not the same between depth and total SA objects')
        }
        
        WBAT.all <- left_join(WBAT.all,WBAT.depth,by=c('IDinter','frequency','treshold','datetime'))
        
        if(flagFirst){                                                          # Combining the current loop with all the loops that are already done
          WBAT.join <- WBAT.all
          flagFirst <- F
        }else{
          WBAT.join <- rbind(WBAT.join,WBAT.all)
        }
      }
      
      samp.summary <- WBAT.join %>% group_by(stationSet,treshold,IDinter,frequency,phase) %>% summarize(n=n())
      
      #dim(subset(WBAT.join,IDinter %in% subset(samp.summary,n >= sampleSize)$IDinter))
      
      # Sampling the data set if minimal n = 6 bins for the first 11 bins (30 seconds bins)
      # slice_head does take out the randomization caused by only using slice
      WBAT.join <- subset(WBAT.join, IDinter %in% subset(samp.summary, n >= 6)$IDinter) %>%
        group_by(stationSet, treshold, IDinter, frequency, phase) %>%
        slice_head(n = 11)        
      #sample_n(11,replace = FALSE)
      
      # summarize WBAT data per intervals
      WBAT.summary <- WBAT.join %>% 
        group_by(stationSet,treshold,IDinter,frequency,phase) %>% 
        summarize(n = n(),
                  station = unique(station),
                  dataSet = unique(dataSet),
                  SAsd = sd(SA, na.rm=T),
                  SA = mean(SA, na.rm=T),
                  depthSA = mean(depthSA, na.rm=T),
                  datetime = first(datetime),
                  depthIntegration = mean(depthIntegration, na.rm=T))
      
      #temp <- subset(WBAT.summary,frequency == 70 & treshold == -60)
      
      WBAT.summary$stationSet   <- paste0(WBAT.summary$dataSet,'_',WBAT.summary$station)
      
      WBAT.summary <- left_join(WBAT.summary, overview.tab, by = c('stationSet'))
      
      # Add the Sun Position 
      # WBAT.summary$datetime <- as.POSIXct(WBAT.summary$datetime, tz = "UTC")
      mySunPosition <- getSunlightPosition(date = WBAT.summary$datetime,
                                           lat = unique(WBAT.summary$lat),
                                           lon = unique(WBAT.summary$lon))
      WBAT.summary$sunPos_altitude <- mySunPosition$altitude * (180 / pi)
      WBAT.summary$sunPos_azimuth <- mySunPosition$azimuth * (180 / pi) + 180
      WBAT.summary$sunPos_azimuth <- ifelse(WBAT.summary$sunPos_azimuth >= 360,  # Condition
                                            WBAT.summary$sunPos_azimuth - 360,  # If TRUE, subtract 360 
                                            WBAT.summary$sunPos_azimuth)
      
      # The lat and lon are corrected according to the ETN database
      mySunlightTimes <- getSunlightTimes(date = as.Date(WBAT.summary$datetime),
                                          lat = unique(WBAT.summary$lat),
                                          lon = unique(WBAT.summary$lon), tz = "UTC") # hack, lat/lon needs to be inputed for each station
      WBAT.summary$hourSunset    <- hour(mySunlightTimes$sunset) + minute(mySunlightTimes$sunset)/60 + second(mySunlightTimes$sunset)/60/60
      WBAT.summary$hourSunrise   <- hour(mySunlightTimes$sunrise) + minute(mySunlightTimes$sunrise)/60 + second(mySunlightTimes$sunrise)/60/60
      WBAT.summary$sunset <- mySunlightTimes$sunset
      WBAT.summary$sunrise <- mySunlightTimes$sunrise
      
      WBAT.summary$dayNight <- 'night'
      WBAT.summary$dayNight[(WBAT.summary$sunrise <= WBAT.summary$datetime) & (WBAT.summary$datetime <= WBAT.summary$sunset)] <- 'day'
    }
    
    # Here "CPOD_2021-BE_cpower" should still be in WBAT.summary based on the left_join of overview.tab
    
    # exception for HKZ and HKN data sets that don't have CPOD data
    if(dim(tab.filt)[1] != 0){
      if(!(unique(WBAT.summary$dataSet) %in% c('2022-HKZ','2023-HKN'))){
        CPOD.stationSet <- strsplit(stationSet,' ')[[1]][1]
        # summarize CPOD
        CPOD.current <- subset(CPOD.all, dataSet_station == CPOD.stationSet)
        if(CPOD.stationSet == "2023-BSW_274174"){
          CPOD.current$stationSet <- "2023-BSW_274174 BSW1"
        }
        if(CPOD.stationSet == "2023-BSW_278093"){
          CPOD.current$stationSet <- "2023-BSW_278093 BSW2"
        }
        if(CPOD.stationSet == "2023-BSW_267814"){
          CPOD.current$stationSet <- "2023-BSW_267814 BSW3"
        }
        if(CPOD.stationSet == "2023-BSW_267838"){
          CPOD.current$stationSet <- "2023-BSW_267838 BSW4"
        }
        
        CPOD.current$stationName  <- CPOD.current$station
        CPOD.current$type <- unique(WBAT.summary$type)
        CPOD.current$lat <- unique(WBAT.summary$lat)
        CPOD.current$lon <- unique(WBAT.summary$lon)
  
        res.CPOD <- format_CPOD(CPOD.current)
      }
    }else{
      CPOD.stationSet <- strsplit(stationSet,' ')[[1]][1]
      CPOD.current <- subset(CPOD.all,dataSet_station == CPOD.stationSet)
      
      tab.filt <- overview.tab[overview.tab$stationSet == CPOD.stationSet,]
      
      CPOD.current$stationName  <- CPOD.current$station
      CPOD.current$type <- unique(tab.filt$type)
      CPOD.current$lat <- unique(tab.filt$lat)
      CPOD.current$lon <- unique(tab.filt$lon)
      
      res.CPOD <- format_CPOD(CPOD.current)
    }
  
    if(flagFirst.all){
      if(!(unique(WBAT.summary$dataSet) %in% c('2022-HKZ','2023-HKN'))){
        CPOD.all.day <- res.CPOD$CPOD.day
        CPOD.all.hour <- res.CPOD$CPOD.hour
        CPOD.all.min <- res.CPOD$CPOD.minute
      }
      if(dim(tab.filt)[1] != 0){
        WBAT.all.summary  <- WBAT.summary
      }
      # WBAT.sample.all   <- WBAT.sample
      flagFirst.all <- F
    }else{
      if(!(unique(WBAT.summary$dataSet) %in% c('2022-HKZ','2023-HKN'))){
        CPOD.all.day <- rbind(CPOD.all.day,res.CPOD$CPOD.day)
        CPOD.all.hour <- rbind(CPOD.all.hour,res.CPOD$CPOD.hour)
        CPOD.all.min <- rbind(CPOD.all.min,res.CPOD$CPOD.minute)
      }
      if(dim(tab.filt)[1] != 0){
        WBAT.all.summary <- rbind(WBAT.all.summary,WBAT.summary)
      }
      # WBAT.sample.all  <- rbind(WBAT.sample.all,WBAT.sample)
    }
  }
  
  save(WBAT.all.summary,CPOD.all.day,CPOD.all.hour,CPOD.all.min,#WBAT.sample.all
       file = file.path(resultPath,saveName))
}else{
  load(file = file.path(dataPath,saveName))
}