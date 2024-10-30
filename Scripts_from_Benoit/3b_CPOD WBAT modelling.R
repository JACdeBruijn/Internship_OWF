library(ggplot2)
library(tidyverse)
library(lubridate)
library(icesTAF)
library(mgcViz)

rm(list=ls())

#setwd('D:/PAM_RABO/finless_neonate_HK')
setwd('D:/git/APELAFICO_OWF_study/')
#setwd('G:/git/WBAT_APELAFICO')

sourceDir(file.path('.','function'))

figurePath    <- file.path('.','figures')
dataPath      <- file.path('.','data')
resultPath    <- file.path('.','results')

WBAT.tab <- read.csv(file.path(dataPath,'survey_db.csv'))
WBAT.tab <- WBAT.tab[,c(1:5,8,9)]
WBAT.tab$stationSet <- paste0(WBAT.tab$dataSet,'_',WBAT.tab$station)

load(file = file.path(resultPath,'CPOD_WBAT_workspace.RData'))

WBAT.all.summary$time_day <- as.POSIXct(cut(WBAT.all.summary$datetime, breaks = "1 day"),tz='UTC')
CPOD.all.day$station <- CPOD.all.day$stationName
CPOD.all.day$stationSet <- paste0(CPOD.all.day$dataSet,'_',CPOD.all.day$station)

############################################################################
# combine CPOD and WBAT data
############################################################################

flagFirst <- T
for(stationSet in unique(WBAT.all.summary$stationSet)){
  idx.WBAT <- WBAT.all.summary$stationSet == stationSet
  
  if(!(unique(WBAT.all.summary$dataSet[idx.WBAT]) %in% c('2022-HKZ','2023-HKN'))){
    CPOD.stationSet <- strsplit(stationSet,' ')[[1]][1]
    CPOD.stationSet <- strsplit(CPOD.stationSet,'_')[[1]]
    CPOD.stationSet <- paste(CPOD.stationSet[1:2],collapse = '_')
    CPOD.current <- subset(CPOD.all.day,stationSet == CPOD.stationSet)
    
    WBAT.all.day <- WBAT.all.summary[idx.WBAT,] %>% group_by(time_day,frequency,treshold) %>% summarize(depthIntegration=mean(depthIntegration,na.rm=T),
                                                                                                        depthSA=mean(depthSA,na.rm=T),
                                                                                                        SA=mean(SA,na.rm=T))
    
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

df.join.all$year <- as.factor(year(df.join.all$time_day))
df.join.all$yday <- yday(df.join.all$time_day)
df.join.all$stationName <- as.factor(df.join.all$stationName)

m1_gam <- gamV(pos_minutes ~ s(yday)+s(log10(SA))+stationName,#s(depthIntegration)+
               data = subset(df.join.all,!is.na(all) & frequency == 70 & treshold == -50 & depthIntegration > 5),
               method = 'REML')

plot(m1_gam)

p1 <- plot(ALE(m1_gam, x = "stationName",type='response',center = 2))

plot(ALE(m1_gam, x = "log10(SA)",type='response',center = 2))
