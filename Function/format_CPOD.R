format_CPOD <- function(CPOD.data){
  
  # ----------------------------------------
  # break down data in minutes
  # ----------------------------------------
  dat_minute <- CPOD.data
  dat_minute$timeIci_str <- as.POSIXct(dat_minute$timeIci_str,format='%Y-%m-%d %H:%M:%S',tz='UTC')
  my.fun <- function(x) as.POSIXct(x, tz = "UTC")
  dat_minute$time_minute    <- unlist(lapply(cut(dat_minute$timeIci_str, breaks = "1 min"),my.fun))
  dat_minute$time_min_num <- dat_minute$time_minute
  
  dat_minute <-   dat_minute %>% 
    group_by(classication,time_minute,stationName,type)%>%
    summarize(  count = n(),
                lat=first(lat),
                lon=first(lon),
                dataSet=first(dataSet))
  
  dat_minute$time_minute    <- as.POSIXct(dat_minute$time_minute,tz='UTC')
  
  dat_minute              <- dat_minute %>% spread(classication, count,fill=0)
  dat_minute$all_buzz     <- dat_minute$buzz
  dat_minute$all_no_buzz  <- dat_minute$inter+dat_minute$other
  dat_minute$all          <- dat_minute$inter+dat_minute$other+dat_minute$buzz
  dat_minute$buzz_ratio   <- dat_minute$all_buzz/dat_minute$all
  
  mySunlightTimes <- getSunlightTimes(date = as.Date(dat_minute$time_minute),
                                      lat = unique(dat_minute$lat),
                                      lon = unique(dat_minute$lon), tz = "UTC") # hack, lat/lon needs to be inputed for each station
  dat_minute$hourSunset    <- hour(mySunlightTimes$sunset)+minute(mySunlightTimes$sunset)/60+second(mySunlightTimes$sunset)/60/60
  dat_minute$hourSunrise   <- hour(mySunlightTimes$sunrise)+minute(mySunlightTimes$sunrise)/60+second(mySunlightTimes$sunrise)/60/60
  dat_minute$sunset <- mySunlightTimes$sunset
  dat_minute$sunrise <- mySunlightTimes$sunrise
  
  dat_minute$dayNight <- 'night'
  dat_minute$dayNight[(dat_minute$sunrise <= dat_minute$time_minute) & (dat_minute$time_minute <= dat_minute$sunset)] <- 'day'
  
  # ----------------------------------------
  # break down data in hours
  # ----------------------------------------
  hour_seq <- seq(  from=as.POSIXct(cut(min(CPOD.data$timeIci_str),breaks='1 day'),tz='UTC'),
                    to=as.POSIXct(cut(max(CPOD.data$timeIci_str),breaks='1 day'),tz='UTC'),
                    by=60*60)
  hour_seq <- as.numeric(hour_seq)
  
  dat_hour <- CPOD.data
  dat_hour$timeIci_str  <- as.POSIXct(dat_hour$timeIci_str,format='%Y-%m-%d %H:%M:%S',tz='UTC')
  my.fun <- function(x) as.POSIXct(x, tz = "UTC")
  dat_hour$time_hour    <- unlist(lapply(cut(dat_hour$timeIci_str, breaks = "1 hour"),my.fun))
  dat_hour$time_hour_num <- dat_hour$time_hour
  
  dat_hour              <-  dat_hour %>% 
                            group_by(classication,time_hour,time_hour_num,stationName,type) %>% 
                            summarize(  count = n(),
                                        lat=first(lat),
                                        lon=first(lon),
                                        dataSet=first(dataSet))
  
  dat_hour$time_hour    <- as.POSIXct(dat_hour$time_hour,tz='UTC')#,origin = "1970-01-01"
  
  dat_hour              <- dat_hour %>% spread(classication, count,fill=0)
  dat_hour$all_buzz     <- dat_hour$buzz
  dat_hour$all_no_buzz  <- dat_hour$inter+dat_hour$other
  dat_hour$all          <- dat_hour$inter+dat_hour$other+dat_hour$buzz
  dat_hour$buzz_ratio   <- dat_hour$all_buzz/dat_hour$all
  dat_hour$pos_minutes  <- 0
  dat_hour$buzz_pos_minutes  <- 0
  dat_hour$day          <- as.POSIXct(format(dat_hour$time_hour,'%Y-%m-%d 00:00:00'),tz='UTC')
  dat_hour$hour         <- hour(dat_hour$time_hour)
  dat_hour$month_year   <- paste0(month(dat_hour$time_hour),'/',year(dat_hour$time_hour))
  
  for(idxHour in 1:(dim(dat_hour)[1])){
    startInter  <- dat_hour$time_hour[idxHour]
    endInter    <- as.POSIXct(as.numeric(dat_hour$time_hour[idxHour])+60*60,origin = "1970-01-01",tz='UTC')
    
    #between(as.POSIXct(as.character(dat_minute$time_minute)), startInter, endInter)
    
    sum(dat_minute$time_minute >= startInter & dat_minute$time_minute < endInter, na.rm = TRUE)
    
    dat_hour$pos_minutes[idxHour] <- sum(dat_minute$time_minute >= startInter & dat_minute$time_minute < endInter, na.rm = TRUE)
    dat_hour$buzz_pos_minutes[idxHour] <- sum(  dat_minute$time_minute >= startInter & 
                                                  dat_minute$time_minute < endInter & 
                                                  dat_minute$all_buzz != 0, 
                                                na.rm = TRUE)
  }
  
  dat_hour <- dat_hour %>% ungroup() %>%
          complete(time_hour_num=hour_seq,fill = list(classication=NA,
                                                  dataSet=unique(dat_hour$dataSet),
                                                  stationName=unique(dat_hour$stationName),
                                                  lat=unique(dat_hour$lat),
                                                  type=unique(dat_hour$type),
                                                  lon=unique(dat_hour$lon)))
  
  dat_hour$time_hour <- as.POSIXct(dat_hour$time_hour_num,tz='UTC')
  #dat_hour$time_hour <- as.POSIXct(dat_hour$time_hour_num,origin = "1970-01-01",tz='UTC')

  
  mySunlightTimes <- getSunlightTimes(date = as.Date(dat_hour$time_hour),
                                      lat = unique(dat_hour$lat),
                                      lon = unique(dat_hour$lon), tz = "UTC") # hack, lat/lon needs to be inputed for each station
  dat_hour$hourSunset    <- hour(mySunlightTimes$sunset)+minute(mySunlightTimes$sunset)/60+second(mySunlightTimes$sunset)/60/60
  dat_hour$hourSunrise   <- hour(mySunlightTimes$sunrise)+minute(mySunlightTimes$sunrise)/60+second(mySunlightTimes$sunrise)/60/60
  dat_hour$sunset <- mySunlightTimes$sunset
  dat_hour$sunrise <- mySunlightTimes$sunrise
  
  dat_hour$dayNight <- 'night'
  dat_hour$dayNight[(dat_hour$sunrise <= dat_hour$time_hour) & (dat_hour$time_hour <= dat_hour$sunset)] <- 'day'
  
  # ----------------------------------------
  # break down data in days
  # ----------------------------------------
  day_seq <- seq(  from=as.POSIXct(cut(min(CPOD.data$timeIci_str),breaks='1 day'),tz='UTC'),
                    to=as.POSIXct(cut(max(CPOD.data$timeIci_str),breaks='1 day'),tz='UTC'),
                    by=60*60*24)
  day_seq <- as.numeric(day_seq)
  
  dat_day <- CPOD.data
  dat_day$timeIci_str   <- as.POSIXct(dat_day$timeIci_str,format='%Y-%m-%d %H:%M:%S',tz='UTC')
  dat_day$time_day      <- as.numeric(as.POSIXct(cut(dat_day$timeIci_str, breaks = "1 day"),tz='UTC'))
  dat_day$time_day_num <- dat_day$time_day
  
  dat_day              <-  dat_day %>% 
                            group_by(classication,time_day,time_day_num,stationName,type) %>% 
                            summarize(  count = n(),
                                        lat=first(lat),
                                        lon=first(lon),
                                        dataSet=first(dataSet))# %>% 
  
  dat_day$time_day    <- as.POSIXct(dat_day$time_day,origin = "1970-01-01",tz='UTC')
  
  dat_day              <- dat_day %>% spread(classication, count,fill=0)
  dat_day$all_buzz     <- dat_day$buzz
  dat_day$all_no_buzz  <- dat_day$inter+dat_day$other
  dat_day$all          <- dat_day$inter+dat_day$other+dat_day$buzz
  dat_day$buzz_ratio   <- dat_day$all_buzz/dat_day$all
  dat_day$pos_minutes  <- 0
  dat_day$buzz_pos_minutes  <- 0
  dat_day$day          <- as.POSIXct(format(dat_day$time_day,'%Y-%m-%d 00:00:00'),tz='UTC')
  dat_day$month_year   <- paste0(month(dat_day$time_day),'/',year(dat_day$time_day))
  
  for(idxDay in 1:(dim(dat_day)[1])){
    startInter  <- dat_day$time_day[idxDay]
    endInter    <- as.POSIXct(as.numeric(dat_day$time_day[idxDay])+60*60*24,origin = "1970-01-01",tz='UTC')
    
    #between(as.POSIXct(as.character(dat_minute$time_minute)), startInter, endInter)
    
    dat_day$pos_minutes[idxDay] <- sum(dat_minute$time_minute >= startInter & dat_minute$time_minute < endInter, na.rm = TRUE)
    dat_day$buzz_pos_minutes[idxDay] <- sum(  dat_minute$time_minute >= startInter & 
                                                  dat_minute$time_minute < endInter & 
                                                  dat_minute$all_buzz != 0, 
                                                na.rm = TRUE)
  }
  
  dat_day <- dat_day %>% ungroup() %>%
    complete(time_day_num=day_seq,fill = list(classication=NA,
                                                dataSet=unique(dat_day$dataSet),
                                                stationName=unique(dat_day$stationName),
                                                type=unique(dat_day$type),
                                                lat=unique(dat_day$lat),
                                                lon=unique(dat_day$lon)))
  dat_day$time_day <- as.POSIXct(dat_day$time_day_num,origin = "1970-01-01",tz='UTC')
  
  
  mySunlightTimes <- getSunlightTimes(date = as.Date(dat_day$time_day),
                                      lat = unique(dat_day$lat),
                                      lon = unique(dat_day$lon), tz = "UTC") # hack, lat/lon needs to be inputed for each station
  dat_day$hourSunset    <- hour(mySunlightTimes$sunset)+minute(mySunlightTimes$sunset)/60+second(mySunlightTimes$sunset)/60/60
  dat_day$hourSunrise   <- hour(mySunlightTimes$sunrise)+minute(mySunlightTimes$sunrise)/60+second(mySunlightTimes$sunrise)/60/60
  dat_day$sunset <- mySunlightTimes$sunset
  dat_day$sunrise <- mySunlightTimes$sunrise
  
  dat_day$dayNight <- 'night'
  dat_day$dayNight[(dat_day$sunrise <= dat_day$time_day) & (dat_day$time_day <= dat_day$sunset)] <- 'day'
  
  
  return(list(CPOD.data=CPOD.data,
              CPOD.hour=dat_hour,
              CPOD.minute=dat_minute,
              CPOD.day=dat_day))
}