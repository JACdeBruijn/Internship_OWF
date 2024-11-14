format_SA <- function(SA){
  

  #SA$time <- as.POSIXct(paste0(SA$DATE,'T',SA$TIME),format="%Y%m%dT%H%M%S",tz="UTC")
  SA$time_min     <- as.POSIXct(cut(SA$time, breaks = "1 min"),tz='UTC')
  SA$time_hour    <- as.POSIXct(cut(SA$time, breaks = "1 hour"),tz='UTC')
  SA$time_day     <- as.POSIXct(cut(SA$time, breaks = "1 day"),tz='UTC')
  SA$hour         <- hour(SA$time)
  SA$month        <- month(SA$time)
  SA$year         <- year(SA$time)
  SA$day          <- day(SA$time)
  
  SA <- SA[!is.na(SA$time),]
  
  # compute minute detections
  minMin   <- min(SA$time_min)
  maxMin   <- max(SA$time_min)
  
  seqMin <- seq( from=minMin,
                  to=maxMin,by="min")
  
  SA_min            <-  SA %>% ungroup()%>%
                                select( time_day,
                                        time_min,
                                        time_hour,
                                        day,
                                        hour,
                                        year,
                                        dataSet,
                                        freq,
                                        stationName,
                                        phaseName,
                                        SA_inter,
                                        lat,
                                        lon,
                                        depth,
                                        SA,
                                        STOP_LOG)
  
  SA_min <- SA_min %>% 
            group_by(time_day,time_min,time_hour,day,hour,year,dataSet,stationName,freq,phaseName,lat,lon,depth,SA_inter) %>%
            summarize(SA=mean(SA,na.rm=TRUE),
                      pingN = sum(STOP_LOG))
  
  #windows()
  #ggplot(subset(SA_min,depth != 0), aes(x=time_min, y=depth, fill=log10(SA))) + 
  #  geom_tile()+
  #  scale_fill_viridis()
  
  # compute hourly detections
  minHour   <- min(SA$time_hour)
  maxHour   <- max(SA$time_hour)
  
  seqHour <- seq( from=minHour,
                  to=maxHour,by="hour")
  
  SA_Hours            <-  SA %>% ungroup()%>%
                          select( time_day,
                                  time_hour,
                                  day,
                                  hour,
                                  year,
                                  dataSet,
                                  freq,
                                  stationName,
                                  phaseName,
                                  SA_inter,
                                  lat,
                                  lon,
                                  depth,
                                  SA,
                                  STOP_LOG)
  
  SA_Hours <- SA_Hours %>% 
              group_by(time_day,time_hour,day,hour,year,dataSet,stationName,freq,phaseName,lat,lon,depth,SA_inter) %>%
              summarize(SA=mean(SA,na.rm=TRUE),
                        pingN = sum(STOP_LOG))
  
  #windows()
  #ggplot(subset(SA_Hours,depth != 0), aes(x=time_hour, y=depth, fill=log10(SA))) + 
  #  geom_tile()+
  #  scale_fill_viridis()+
  #  facet_grid(freq~stationName)
  
  uniqueStation <- unique(SA_Hours$stationName)
  for(idxStation in uniqueStation){
    idxSelStation <- SA_Hours$stationName == idxStation
    lat <- unique(SA_Hours$lat[idxSelStation])
    lon <- unique(SA_Hours$lon[idxSelStation])
    
    mySunlightTimes <- getSunlightTimes(date = as.Date(SA_Hours$time_day[idxSelStation]),
                                        lat = lat,
                                        lon = lon, tz = "UTC")
    SA_Hours$hourSunset[idxSelStation]    <- hour(mySunlightTimes$sunset)+minute(mySunlightTimes$sunset)/60+second(mySunlightTimes$sunset)/60/60
    SA_Hours$hourSunrise[idxSelStation]   <- hour(mySunlightTimes$sunrise)+minute(mySunlightTimes$sunrise)/60+second(mySunlightTimes$sunrise)/60/60
  }
  
  SA_Hours$dayNight <- 'night'
  SA_Hours$dayNight[(SA_Hours$sunrise <= SA_Hours$time_hour) & (SA_Hours$time_hour <= SA_Hours$sunset)] <- 'day'
  
  # compute daily detections
  minDay   <- min(SA$time_hour)
  maxDay   <- max(SA$time_hour)
  
  seqHour <- seq( from=minHour,
                  to=maxHour,by="hour")
  
  SA_Hours            <-  SA %>% ungroup()%>%
    select( time_day,
            time_hour,
            day,
            hour,
            year,
            dataSet,
            freq,
            stationName,
            phaseName,
            SA_inter,
            lat,
            lon,
            depth,
            SA,
            STOP_LOG)
  
  SA_Hours <- SA_Hours %>% 
    group_by(time_day,time_hour,day,hour,year,dataSet,stationName,freq,phaseName,lat,lon,depth,SA_inter) %>%
    summarize(SA=mean(SA,na.rm=TRUE),
              pingN = sum(STOP_LOG))
  
  #windows()
  #ggplot(subset(SA_Hours,depth != 0), aes(x=time_hour, y=depth, fill=log10(SA))) + 
  #  geom_tile()+
  #  scale_fill_viridis()+
  #  facet_grid(freq~stationName)
  
  uniqueStation <- unique(SA_Hours$stationName)
  for(idxStation in uniqueStation){
    idxSelStation <- SA_Hours$stationName == idxStation
    lat <- unique(SA_Hours$lat[idxSelStation])
    lon <- unique(SA_Hours$lon[idxSelStation])
    
    mySunlightTimes <- getSunlightTimes(date = as.Date(SA_Hours$time_day[idxSelStation]),
                                        lat = lat,
                                        lon = lon, tz = "UTC")
    SA_Hours$hourSunset[idxSelStation]    <- hour(mySunlightTimes$sunset)+minute(mySunlightTimes$sunset)/60+second(mySunlightTimes$sunset)/60/60
    SA_Hours$hourSunrise[idxSelStation]   <- hour(mySunlightTimes$sunrise)+minute(mySunlightTimes$sunrise)/60+second(mySunlightTimes$sunrise)/60/60
  }
  
  SA_Hours$dayNight <- 'night'
  SA_Hours$dayNight[(SA_Hours$sunrise <= SA_Hours$time_hour) & (SA_Hours$time_hour <= SA_Hours$sunset)] <- 'day'
  
  return(list(SA=SA,
              SA_Hours=SA_Hours,
              SA_min=SA_min))
}
