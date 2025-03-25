################################################################################--
## Script functionality: Testing the SunAngle and SunPos funtion before integrating it into the 2_creating CPOD_WBAT work space script--
## 08/12/24 
################################################################################--
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, RColorBrewer, icesTAF, see, suncalc, gridExtra,
               grid, RColorBrewer, ggtext)

rm(list=ls())

# sourceDir <- function(directory) {
#   files <- list.files(directory, pattern = "\\.R$", full.names = TRUE)
#   for (file in files) {
#     source(file)
#   }
# }

sourceDir(file.path('.','function'))
figurePath    <- file.path('.','figures')
dataPath      <- file.path('.','data')
resultPath    <- file.path('.','results')

################################################################################--
WBAT.tab <- file.path(dataPath, 'survey_db.csv') %>% 
  read_csv() %>%
  select(1:5, 8, 9) %>%
  mutate(dataSet_station = str_c(dataSet, '_', station))

overview.tab <- file.path(dataPath, 'data_overview.csv') %>% 
  read_csv() %>% 
  mutate(stationSet = str_c(dataSet, '_', station),
         pairingName = str_c(dataSet, '_', pairing)) %>% 
  select(-c('year', 'dataSet', 'station'))

################################################################################--
# Testing the funtion and the script before integrating it in the format data script----
latitude <- 51.40645  # Latitude of Woerden
longitude <- 2.818500  # Longitude of Woerden
date_time <- as.POSIXct("2021-08-30 12:00:00", tz = "UTC")

sun_position <- getSunlightPosition(date = date_time, lat = latitude, lon = longitude)

altitude_degrees <- sun_position$altitude * (180 / pi)
azimuth_degrees <- sun_position$azimuth * (180 / pi) + 180

if (azimuth_degrees >= 0) {
  azimuth_degrees <- azimuth_degrees - 360
}

cat("Sun Altitude:", altitude_degrees, "degrees\n")
cat("Sun Azimuth:", azimuth_degrees, "degrees\n")

# Still testing the script - not for running the final analysis ----
mySunlightTimes <- getSunlightTimes(date = as.Date(WBAT.all$datetime),
                                    lat = unique(WBAT.all$lat),
                                    lon = unique(WBAT.all$lon), tz = "UTC") # hack, lat/lon needs to be inputed for each station
WBAT.all$hourSunset    <- hour(mySunlightTimes$sunset) + minute(mySunlightTimes$sunset)/60 + second(mySunlightTimes$sunset)/60/60
WBAT.all$hourSunrise   <- hour(mySunlightTimes$sunrise) + minute(mySunlightTimes$sunrise)/60 + second(mySunlightTimes$sunrise)/60/60
WBAT.all$sunset <- mySunlightTimes$sunset
WBAT.all$sunrise <- mySunlightTimes$sunrise

WBAT.all$dayNight <- 'night'
WBAT.all$dayNight[(WBAT.all$sunrise <= WBAT.all$datetime) & (WBAT.all$datetime <= WBAT.all$sunset)] <- 'day'
