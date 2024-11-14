format_schoolParameters <- function(school_parameters){
  
  school_parameters$timeStart   <- as.POSIXct(school_parameters$timeStart,format="%Y-%m-%dT%H:%M:%OS",tz="UTC")
  school_parameters$timeEnd     <- as.POSIXct(school_parameters$timeEnd,format="%Y-%m-%dT%H:%M:%OS",tz="UTC")
  school_parameters$time_hour   <- as.POSIXct(cut(school_parameters$timeStart, breaks = "1 hour"),tz='UTC')
  school_parameters$time_day    <- as.POSIXct(cut(school_parameters$timeStart, breaks = "1 day"),tz='UTC')
  school_parameters$hour        <- hour(school_parameters$timeStart)
  school_parameters$month       <- month(school_parameters$timeStart)
  school_parameters$year        <- year(school_parameters$timeStart)
  school_parameters$day         <- day(school_parameters$timeStart)
  
  # compute hourly detections
  minHour   <- min(school_parameters$time_hour)
  maxHour   <- max(school_parameters$time_hour)
  
  seqHour <- seq( from=minHour,
                  to=maxHour,by="hour")
  
  school_posHours            <-  school_parameters %>%
    group_by(time_day,time_hour,day,hour,year,dataSet,stationName,freq,phaseName,lat,lon) %>%
    summarize(  count = n())
  
  idxFill <- which(!(seqHour %in% school_posHours$time_hour))
  
  dfFill  <- data.frame(matrix(ncol = dim(school_posHours)[2], nrow = length(idxFill)))
  colnames(dfFill) <- colnames(school_posHours)
  dfFill$time_hour <- seqHour[idxFill]
  
  #school_posHours <- rbind(school_posHours,dfFill)
  school_posHours <- school_posHours[order(school_posHours$time_hour),]
  school_posHours <- school_posHours %>% ungroup %>% fill(dataSet,stationName,phaseName,year,lat,lon,.direction='downup')
  school_posHours$pos_hours    <- 0
  school_posHours$pos_hours[!is.na(school_posHours$count)] <- 1
  school_posHours$count[is.na(school_posHours$count)] <- 0
  school_posHours$hour        <- hour(school_posHours$time_hour)
  school_posHours$month       <- month(school_posHours$time_hour)
  school_posHours$year        <- year(school_posHours$time_hour)
  school_posHours$day         <- day(school_posHours$time_hour)
  school_posHours$time_day    <- as.POSIXct(cut(school_posHours$time_hour, breaks = "1 day"),tz='UTC')
  school_posHours             <- school_posHours[!is.na(school_posHours$freq),]
  
  uniqueStation <- unique(school_posHours$stationName)
  for(idxStation in uniqueStation){
    idxSelStation <- school_posHours$stationName == idxStation
    lat <- unique(school_posHours$lat[idxSelStation])
    lon <- unique(school_posHours$lon[idxSelStation])
    
    mySunlightTimes <- getSunlightTimes(date = as.Date(school_posHours$time_day[idxSelStation]),
                                        lat = lat,
                                        lon = lon, tz = "UTC")
    school_posHours$hourSunset[idxSelStation]    <- hour(mySunlightTimes$sunset)+minute(mySunlightTimes$sunset)/60+second(mySunlightTimes$sunset)/60/60
    school_posHours$hourSunrise[idxSelStation]   <- hour(mySunlightTimes$sunrise)+minute(mySunlightTimes$sunrise)/60+second(mySunlightTimes$sunrise)/60/60
  }
  
  # compute daily detections
  minDay   <- min(school_parameters$time_day)
  maxDay   <- max(school_parameters$time_day)
  
  seqDay <- seq( from=minDay,
                 to=maxDay,by="day")
  
  school_posDay            <-  school_parameters %>%
    group_by(time_day,day,year,dataSet,stationName,freq,phaseName,lat,lon) %>%
    summarize(  count = n())
  
  idxFill <- which(!(as.Date(seqDay) %in% as.Date(school_posDay$time_day)))
  
  dfFill  <- data.frame(matrix(ncol = dim(school_posDay)[2], nrow = length(idxFill)))
  colnames(dfFill) <- colnames(school_posDay)
  dfFill$time_day <- seqDay[idxFill]
  
  if(dim(dfFill)[1] != 0){
    school_posDay <- rbind(school_posDay,dfFill) 
  }
  school_posDay <- school_posDay[order(school_posDay$time_day),]
  school_posDay <- school_posDay %>% ungroup %>% fill(dataSet,stationName,phaseName,year,lat,lon,.direction='downup')
  school_posDay$pos_day    <- 0
  school_posDay$pos_day[!is.na(school_posDay$count)] <- 1
  school_posDay$count[is.na(school_posDay$count)] <- 0
  school_posDay$hour        <- hour(school_posDay$time_day)
  school_posDay$month       <- month(school_posDay$time_day)
  school_posDay$year        <- year(school_posDay$time_day)
  school_posDay$day         <- day(school_posDay$time_day)
  
  school_posDay$pos_hours <- 0
  
  uniquePosDay  <- unique(school_posDay$time_day)
  uniqueStation <- unique(school_posDay$stationName)
  uniqueFreq    <- unique(school_posDay$freq)
  
  for(idxDay in uniquePosDay){
    for(idxStation in uniqueStation){
      for(idxFreq in uniqueFreq){
        school_posDay$pos_hours[school_posDay$time_day == idxDay & 
                                  school_posDay$stationName == idxStation & 
                                  school_posDay$freq == idxFreq]   <- 
          length(which(school_posHours$time_day[school_posHours$pos_hours!=0] == idxDay &
                         school_posHours$stationName[school_posHours$pos_hours!=0] == idxStation & 
                         school_posHours$freq[school_posHours$pos_hours!=0] == idxFreq))
      }
    }
  }
  
  return(list(schoolParameters=school_parameters,
              schoolPosDay=school_posDay,
              schoolPosHours=school_posHours))
}