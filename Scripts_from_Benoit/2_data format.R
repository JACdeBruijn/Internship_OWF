###
# Started working through this script - 14/10/24
# Purpose of script: 
###


if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, icesTAF, suncalc)

rm(list=ls())

#setwd('D:/PAM_RABO/finless_neonate_HK')
# setwd('G:/git/APELAFICO_OWF_study/')
#setwd('G:/git/WBAT_APELAFICO')
# data_overview_JB <- read_csv("Data/APELAFICO_data/Data_overview/data_overview.csv")

sourceDir(file.path('.','Function'))

figurePath    <- file.path('.','Figures')
dataPath      <- file.path('.','Data')
resultPath    <- file.path('.','Results')

WBAT.tab <- read.csv(file.path(dataPath,'survey_db.csv'))                       # Creating WBAT.tab from survey_db.csv
WBAT.tab <- WBAT.tab[,c(1:5,8,9)]
WBAT.tab$dataSet_station <- paste0(WBAT.tab$dataSet,'_',WBAT.tab$station)

overview.tab <- read.csv(file.path(dataPath,'data_overview.csv'))               # Creating overview.tab from data_overview.csv
overview.tab$stationSet <- paste0(overview.tab$dataSet,'_',overview.tab$station)
overview.tab$pairingName <- paste0(overview.tab$dataSet,'_',overview.tab$pairing)
overview.tab <- overview.tab %>% select(-c('year','dataSet','station'))

processFlag <- T

############################################################################
## Read data and plot
############################################################################

if(processFlag){
  load(file.path(dataPath,'CPOD_all.RData'))
  
  CPOD.all <- CPOD.all %>% separate(PODid,into=c("POD","dataSet", "station"),sep = "_")
  CPOD.all$station[CPOD.all$station == '267878'] <- '267838'                    # Waarschijnlijk stond er een foutje in de cijferreeks, maar waarom staan deze cijfers uberhaupt in de naam
  CPOD.all$timeIci_str <- as.POSIXct(CPOD.all$timeIci_str,tz='UTC')
  
  CPOD.all$dataSet_station <- paste0(CPOD.all$dataSet,'_',CPOD.all$station)
  CPOD.all$timeIci_num <- as.numeric(CPOD.all$timeIci_str)
  
  
  WBAT.tab <- WBAT.tab[WBAT.tab$SA_exported == 1,]
  
  flagFirst.all <- T
  for(stationSet in unique(WBAT.tab$dataSet_station)){
    print(stationSet)
    tab.filt <- WBAT.tab[WBAT.tab$dataSet_station == stationSet,]
    
    # Read all data in the set of station+data set
    flagFirst <- T
    for(idxDataSet in tab.filt$surveName){
      print(idxDataSet)
      load(file.path(dataPath,paste0('WBAT_',idxDataSet,'.RData')))
      
      WBAT.all <- WBAT.all[!is.na(WBAT.all$datetime),]
      WBAT.all$stationSet  <- paste0(WBAT.all$dataSet,'_',WBAT.all$station,'_',WBAT.all$phase)
      WBAT.all$IDinter <- NA
      
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
        
        WBAT.current$IDinter <- c(cumsum(WBAT.current$thresh_pass))
        WBAT.current$IDinter <- paste0(WBAT.current$stationSet,'_',idxTreshold,'_',WBAT.current$frequency,'_',WBAT.current$IDinter)

        WBAT.all$IDinter[which(idxFilt)[idxOrder]] <- WBAT.current$IDinter
      }

      WBAT.depth <- subset(WBAT.all,depth != 0) %>% group_by(IDinter,frequency,treshold,datetime) %>% summarize(depthSA=sum(SA*depth)/sum(SA))
      WBAT.depth <- WBAT.depth[order(WBAT.depth$datetime),]
      
      WBAT.all <- subset(WBAT.all,depth == 0)
      if(dim(WBAT.all)[1] != dim(WBAT.depth)[1]){
        print('dimensions are not the same between depth and total SA objects')
      }
        
      WBAT.all <- left_join(WBAT.all,WBAT.depth,by=c('IDinter','frequency','treshold','datetime'))
      
      if(flagFirst){
        WBAT.join <- WBAT.all
        flagFirst <- F
      }else{
        WBAT.join <- rbind(WBAT.join,WBAT.all)
      }
    }
    
    samp.summary <- WBAT.join %>% group_by(stationSet,treshold,IDinter,frequency,phase) %>% summarize(n=n())

    #dim(subset(WBAT.join,IDinter %in% subset(samp.summary,n >= sampleSize)$IDinter))
    
    WBAT.join <- subset(WBAT.join,IDinter %in% subset(samp.summary,n >= 6)$IDinter) %>%
                  group_by(stationSet,treshold,IDinter,frequency,phase) %>%
                  slice(sample(n(), min(11, n())))
                  #sample_n(11,replace = FALSE)
    
    # summarize WBAT data per intervals
    WBAT.summary <- WBAT.join %>% group_by(stationSet,treshold,IDinter,frequency,phase) %>% 
      summarize(n=n(),
                station=unique(station),
                dataSet=unique(dataSet),
                SAsd=sd(SA,na.rm=T),
                SA=mean(SA,na.rm=T),
                depthSA=mean(depthSA,na.rm=T),
                datetime=first(datetime),
                depthIntegration=mean(depthIntegration,na.rm=T))

    WBAT.summary$stationSet   <- paste0(WBAT.summary$dataSet,'_',WBAT.summary$station)
    
    WBAT.summary <- left_join(WBAT.summary,overview.tab,by=c('stationSet'))
    
    
    # samp.summary <- WBAT.join %>% group_by(stationSet,treshold,IDinter,frequency,phase) %>% summarize(n=n())
    # 
    # samplingSize <- seq(from=5,to=30,by=1)#c(3:10)
    # nIter <- 1
    # 
    # flagFirstSamples <- T
    # for(idxSampleSize in samplingSize){
    #   for(idxIter in 1:nIter){
    #     if(idxSampleSize <= max(samp.summary$n)){
    #       WBAT.temp <- subset(WBAT.join,IDinter %in% subset(samp.summary,n >= idxSampleSize)$IDinter) %>% 
    #         group_by(stationSet,treshold,IDinter,frequency,phase) %>%
    #         sample_n(idxSampleSize) %>%
    #         summarize(n=n(),
    #                   station=unique(station),
    #                   dataSet=unique(dataSet),
    #                   SA=mean(SA,na.rm=T),
    #                   depthSA=mean(depthSA,na.rm=T),
    #                   datetime=first(datetime),
    #                   depthIntegration=mean(depthIntegration,na.rm=T))
    #       
    #       WBAT.temp$SATotal <- WBAT.summary$SA[match(WBAT.temp$IDinter,WBAT.summary$IDinter)]
    #       WBAT.temp$ratio   <- WBAT.temp$SA/WBAT.temp$SATotal
    #       WBAT.temp$iter    <- idxIter
    #       
    #       if(flagFirstSamples){
    #         WBAT.sample <- WBAT.temp
    #         flagFirstSamples <- F
    #       }else{
    #         WBAT.sample <- rbind(WBAT.sample,WBAT.temp)
    #       }
    #     }
    #   }
    # }
    
    mySunlightTimes <- getSunlightTimes(date = as.Date(WBAT.summary$datetime),
                                        lat = unique(WBAT.summary$lat),
                                        lon = unique(WBAT.summary$lon), tz = "UTC") # hack, lat/lon needs to be inputed for each station
    WBAT.summary$hourSunset    <- hour(mySunlightTimes$sunset)+minute(mySunlightTimes$sunset)/60+second(mySunlightTimes$sunset)/60/60
    WBAT.summary$hourSunrise   <- hour(mySunlightTimes$sunrise)+minute(mySunlightTimes$sunrise)/60+second(mySunlightTimes$sunrise)/60/60
    WBAT.summary$sunset <- mySunlightTimes$sunset
    WBAT.summary$sunrise <- mySunlightTimes$sunrise
    
    WBAT.summary$dayNight <- 'night'
    WBAT.summary$dayNight[(WBAT.summary$sunrise <= WBAT.summary$datetime) & (WBAT.summary$datetime <= WBAT.summary$sunset)] <- 'day'
    
    # exception for HKZ and HKN data sets that don't have CPOD data
    if(!(unique(WBAT.summary$dataSet) %in% c('2022-HKZ','2023-HKN'))){
      CPOD.stationSet <- strsplit(stationSet,' ')[[1]][1]
      # summarize CPOD
      CPOD.current <- subset(CPOD.all,dataSet_station == CPOD.stationSet)
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
  
    if(flagFirst.all){
      if(!(unique(WBAT.summary$dataSet) %in% c('2022-HKZ','2023-HKN'))){
        CPOD.all.day <- res.CPOD$CPOD.day
        CPOD.all.hour <- res.CPOD$CPOD.hour
        CPOD.all.min <- res.CPOD$CPOD.minute
      }
      WBAT.all.summary  <- WBAT.summary
      # WBAT.sample.all   <- WBAT.sample
      flagFirst.all <- F
    }else{
      if(!(unique(WBAT.summary$dataSet) %in% c('2022-HKZ','2023-HKN'))){
        CPOD.all.day <- rbind(CPOD.all.day,res.CPOD$CPOD.day)
        CPOD.all.hour <- rbind(CPOD.all.hour,res.CPOD$CPOD.hour)
        CPOD.all.min <- rbind(CPOD.all.min,res.CPOD$CPOD.minute)
      }
      WBAT.all.summary <- rbind(WBAT.all.summary,WBAT.summary)
      # WBAT.sample.all  <- rbind(WBAT.sample.all,WBAT.sample)
    }
  
    # # make individual plot
    # print(ggplot(subset(WBAT.summary,treshold %in% c(-60,-55,-50)),aes(x=datetime,y=log10(SA),col=as.factor(treshold)))+
    #         geom_line()+
    #         geom_smooth(col='black')+
    #         ggtitle(paste0('WBAT time series - ',stationSet))+
    #         facet_grid(treshold~frequency))
    # 
    # if(!(unique(WBAT.summary$dataSet) %in% c('2022-HKZ','2023-HKN'))){
    #   print(ggplot(res.CPOD$CPOD.day,aes(x=time_day,y=buzz_ratio))+
    #             geom_line()+
    #             geom_smooth(col='black')+
    #             ggtitle(paste0('buzz click ratio - ',stationSet)))
    # }
    # 
    # print(ggplot(subset(WBAT.summary,treshold %in% c(-60,-55,-50)),aes(x=dayNight,y=log10(SA),fill=as.factor(treshold)))+
    #       geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
    #       facet_grid(treshold~frequency)+
    #       ggtitle(paste0('WBAT day/night - ',stationSet)))
    # 
    # if(!(unique(WBAT.summary$dataSet) %in% c('2022-HKZ','2023-HKN'))){
    #   print(ggplot(subset(res.CPOD$CPOD.hour,buzz_pos_minutes!=0),aes(x=dayNight,y=buzz_pos_minutes))+
    #         geom_boxplot()+
    #           ggtitle(paste0('buzz pos min - ',stationSet)))
    # }
  }

  save(WBAT.all.summary,CPOD.all.day,CPOD.all.hour,CPOD.all.min,#WBAT.sample.all
       file = file.path(resultPath,'CPOD_WBAT_workspace.RData'))
}else{
  load(file = file.path(dataPath,'CPOD_WBAT_workspace.RData'))
}

############################################################################
# SA over different intervals
############################################################################

windows()
ggplot(subset(WBAT.sample.all,treshold == -50),aes(x=as.factor(n),y=ratio,fill=as.factor(frequency)))+
  geom_boxplot()+
  ylim(0,1.5)+
  facet_wrap(~dataSet,scales = 'free_x')

############################################################################
# plotting
############################################################################

windows()
print(ggplot(WBAT.all.summary,aes(x=station,y=log10(SA)))+
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  facet_grid(frequency~dataSet,scales = 'free_x')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle('SA all stations'))

windows()
print(ggplot(CPOD.all.day,aes(x=stationName,y=pos_minutes))+
  geom_boxplot()+
  facet_wrap(~dataSet,scales = 'free')+
  ggtitle('CPOD positive minutes per station')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))

windows()
print(ggplot(CPOD.all.day,aes(x=stationName,y=buzz_ratio))+
  geom_boxplot()+
  facet_wrap(~dataSet,scales = 'free_x')+
  ggtitle('CPOD buzz click ratio per station')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))


WBAT.all.summary$time_day <- as.POSIXct(cut(WBAT.all.summary$datetime, breaks = "1 day"),tz='UTC')
CPOD.all.summary$station <- CPOD.all.summary$stationName
CPOD.all.summary$dataSet_station <- paste0(CPOD.all.summary$dataSet,'_',CPOD.all.summary$station)

############################################################################
# combine CPOD and WBAT data
############################################################################

flagFirst <- T
for(stationSet in unique(WBAT.all.summary$dataSet_station)){
  idx.WBAT <- WBAT.all.summary$dataSet_station == stationSet
  
  if(!(unique(WBAT.all.summary$dataSet[idx.WBAT]) %in% c('2022-HKZ','2023-HKN'))){
    CPOD.stationSet <- strsplit(stationSet,' ')[[1]][1]
    CPOD.current <- subset(CPOD.all.summary,dataSet_station == CPOD.stationSet)
    
    WBAT.all.day <- WBAT.all.summary[idx.WBAT,] %>% group_by(time_day,frequency,treshold) %>% summarize(SA=mean(SA,na.rm=T))
    
    df.join <- left_join(WBAT.all.day,CPOD.current,by=c('time_day'))
    df.join$station <- unique(WBAT.all.summary$station[idx.WBAT])
    df.join$dataSet <- unique(WBAT.all.summary$dataSet[idx.WBAT])
    
    if(flagFirst){
      df.join.all <- df.join
      flagFirst <- F
    }else{
      df.join.all <- rbind(df.join.all,df.join)
    }
  }
}

for(idxDataSet in unique(df.join.all$dataSet)){
  if(dim(subset(df.join.all,dataSet == idxDataSet & all_buzz!=0 & frequency == 70 & treshold == -50))[1] != 0){
    print(ggplot(subset(df.join.all,dataSet == idxDataSet & all_buzz!=0 & frequency == 70 & treshold == -50),aes(x=buzz_ratio,y=log10(SA)))+
      geom_point()+
      geom_smooth(method='lm')+
      facet_wrap(~station)+
      ggtitle(paste0('CPOD/WBAT(70) relation - ',idxDataSet)))
  }
  
  if(dim(subset(df.join.all,dataSet == idxDataSet & all_buzz!=0 & frequency == 200 & treshold == -50))[1] != 0){
    print(ggplot(subset(df.join.all,dataSet == idxDataSet & all_buzz!=0 & frequency == 200 & treshold == -50),aes(x=buzz_ratio,y=log10(SA)))+
            geom_point()+
            geom_smooth(method='lm')+
            facet_wrap(~station)+
            ggtitle(paste0('CPOD/WBAT(200) relation - ',idxDataSet)))
  }
}



# 
# fileList <- list.files(file.path(dataPath),pattern="WBAT",full.names = T)
# fileList <- fileList[!file.info(fileList)$isdir]
# 
# for(myFile in fileList){
#   load(myFile)
#   
#   name.split  <- strsplit(myFile,'/')[[1]][3]
#   name.split  <- strsplit(name.split,'.RData')[[1]]
#   name.split  <- strsplit(name.split,'')
#   
#   WBAT.all <- subset(WBAT.all,depth == 0)
#   
#   WBAT.summary <- WBAT.all %>% group_by(treshold,IDinter,frequency,chunk) %>% summarize(SA=mean(SA,na.rm=T),
#                                                                                          datetime=first(datetime,na.rm=T),
#                                                                                          depthIntegration=first(depthIntegration,na.rm=T))
#   
#   ggplot(WBAT.summary,aes(x=datetime,y=log10(SA),col=as.factor(treshold)))+
#     geom_line()+
#     ggtitle()
# }
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
# windows()
# ggplot(subset(WBAT.summary,treshold==-50),aes(x=datetime,y=log10(SA)))+
#   geom_line()
# 
# windows()
# WBAT.join$IDinterUnique <- paste0(WBAT.join$IDinter,'_',WBAT.join$chunk)
# df.plot <- subset(WBAT.join,treshold %in% c(-50))
# ggplot(df.plot,aes(x=datetime,group=IDinterUnique))+
#   stat_summary(aes(y =log10(SA)),fun=mean, geom = "line")
