################################################################################
# Script for plotting - 08/12/24 ----
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

################################################################################
WBAT.tab <- file.path(dataPath, 'survey_db.csv') %>% 
  read_csv() %>%
  select(1:5, 8, 9) %>%
  mutate(dataSet_station = str_c(dataSet, '_', station))

overview.tab <- file.path(dataPath, 'data_overview.csv') %>% 
  read_csv() %>% 
  mutate(stationSet = str_c(dataSet, '_', station),
         pairingName = str_c(dataSet, '_', pairing)) %>% 
  select(-c('year', 'dataSet', 'station'))

################################################################################
# Testing the funtion and the script before integrating it in the format data script
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

# Still testing the script - not for running the final analysis
mySunlightTimes <- getSunlightTimes(date = as.Date(WBAT.all$datetime),
                                    lat = unique(WBAT.all$lat),
                                    lon = unique(WBAT.all$lon), tz = "UTC") # hack, lat/lon needs to be inputed for each station
WBAT.all$hourSunset    <- hour(mySunlightTimes$sunset) + minute(mySunlightTimes$sunset)/60 + second(mySunlightTimes$sunset)/60/60
WBAT.all$hourSunrise   <- hour(mySunlightTimes$sunrise) + minute(mySunlightTimes$sunrise)/60 + second(mySunlightTimes$sunrise)/60/60
WBAT.all$sunset <- mySunlightTimes$sunset
WBAT.all$sunrise <- mySunlightTimes$sunrise

WBAT.all$dayNight <- 'night'
WBAT.all$dayNight[(WBAT.all$sunrise <= WBAT.all$datetime) & (WBAT.all$datetime <= WBAT.all$sunset)] <- 'day'

# From here the script is to be used 
################################################################################
load(file = file.path(resultPath,'CPOD_WBAT_workspace.RData'))
################################################################################

# Making type a factor and removing unneeded columns
WBAT.all.summary <- WBAT.all.summary %>% 
  mutate(type = factor(type, levels = c("control", "OWF"), label = c("Control", "OWF"))) %>% 
  select(-freq, -lat, -lon, -soundtrap, -CPOD, -WBAT_70, -WBAT_200, 
         # -hourSunset, -hourSunrise, -sunset, -sunrise
  ) %>% 
  distinct(IDinter, .keep_all = TRUE)


################################################################################
# Working on WBAT_overview showing SA total ----
################################################################################

# Sub-setting on threshold -50
WBAT_overview <- WBAT.all.summary %>% 
  filter(n >= 8, treshold == -50, frequency == 70) %>% 
  mutate(time = as.numeric(format(datetime, "%H")))

hist(WBAT_overview$sunPos_altitude)

ggplot(WBAT_overview, aes(x = sunPos_altitude, y = log10(SA))) +
  geom_line()

ggplot(subset(WBAT_overview, dayNight == "day" & stationSet == "2021-BE_grafton"), aes(x = sunPos_altitude, y = log10(SA))) +
  geom_line()

summary(WBAT_overview$sunPos_altitude)

# Creating a data set with the day and night based on the sunposition
Sunny <- WBAT_overview %>%
  mutate(dayNight_ele = ifelse(sunPos_altitude >= 0, "Day", "Night"))

# Imputing the phases of the day/sun
Sunny <- Sunny %>%
  group_by(stationSet) %>% 
  arrange(datetime) %>% 
  mutate(phase_sun = case_when(
    sunPos_altitude >= 0 & lead(sunPos_altitude) > sunPos_altitude ~ "Morning",
    sunPos_altitude >= 0 & lead(sunPos_altitude) < sunPos_altitude ~ "Evening",
    sunPos_altitude < 0 & lead(sunPos_altitude) > sunPos_altitude ~ "Dawn",
    sunPos_altitude < 0 & lead(sunPos_altitude) < sunPos_altitude ~ "Night",
      TRUE ~ NA_character_)) %>% 
  filter(!is.na(phase_sun))

# Doing some binning and splitting
Sunny <- Sunny %>%
  group_by(stationSet) %>% 
  mutate(altitude_bin = cut(
    sunPos_altitude, 
    breaks = seq(-80, 80, by = 10), 
    labels = seq(-75, 75, by = 10),
    include.lowest = TRUE),
    altitude_bin = as.numeric(as.character(altitude_bin))) %>% 
  filter(altitude_bin != "-65") %>%                                             # At -65 there are only 5 values
  filter(!(phase_sun == "Morning" & altitude_bin == "65")) %>%                  # At 65 in the morning there are only 4 values
  arrange(stationSet, datetime) %>% 
  ungroup()

# Create a vertical gradient matrix
gradient_colors_morning <- colorRampPalette(c("black", "white"))(100)
gradient_matrix_morning <- matrix(gradient_colors_morning, nrow = 1, ncol = 100)
vertical_gradient_morning <- rasterGrob(image = gradient_matrix_morning, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)

gradient_colors_evening <- colorRampPalette(c("white", "black"))(100)
gradient_matrix_evening <- matrix(gradient_colors_evening, nrow = 1, ncol = 100)
vertical_gradient_evening <- rasterGrob(image = gradient_matrix_evening, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)

# Plotting the first 2 phases 
p1 <- ggplot(subset(Sunny, phase_sun %in% c("Morning", "Dawn")), aes(x = factor(altitude_bin), y = log10(SA), fill = phase_sun)) +
  annotation_custom(vertical_gradient_morning, 
                    xmin = 4.45, xmax = 6.5, 
                    ymin = -Inf, ymax = Inf) +
  annotate("rect", 
           xmin = -Inf, xmax = 4.5, 
           ymin = -Inf, ymax = Inf, 
           fill = "black") +
  geom_vline(xintercept = 6.5, linetype = "dashed") +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-0.2, 4)) +
  labs(x = "Sun altitude around Sunrise (degrees)", y = "Mean log<sub>10</sub>(SA)") +
  scale_fill_manual(values = c("Morning" = "coral", "Dawn" = "lightblue")) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.y = element_markdown()
    )

# Plotting the 3 and the 4 phases
p2 <- ggplot(subset(Sunny, phase_sun %in% c("Evening", "Night")), aes(x = factor(altitude_bin), y = log10(SA), fill = phase_sun)) +
  annotation_custom(vertical_gradient_evening, 
                    xmin = 7.5, xmax = 9.5, 
                    ymin = -Inf, ymax = Inf) +
  annotate("rect", 
           xmin = 9.5, xmax = Inf, 
           ymin = -Inf, ymax = Inf, 
           fill = "black") +
  geom_vline(xintercept = 7.5, linetype = "dashed") +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-0.2, 4)) +
  scale_x_discrete(
    name = "Sun altitude around Sunset (degrees)",
    labels = function(x) ifelse(x == "0", "Sunrise/Sunset", x),
    limits = rev(levels(factor(subset(Sunny, phase_sun %in% c("Evening", "Night"))$altitude_bin)))) +
  scale_fill_manual(values = c("Evening" = "coral", "Night" = "lightblue")) +
  theme_classic() +
  theme(
    legend.position="none",
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),    
    axis.line.y = element_blank())


plot <- grid.arrange(p1, p2, nrow = 1)

ggsave(filename = file.path(figurePath,'Sun altitude around sunset and sunrise with light gradient.png'), plot = plot, width = 15, height = 10)


effe_tellen <- Sunny %>%
  count(altitude_bin) %>% 
  arrange(n)

effe_anderevolgorde <- Sunny %>%
  group_by(phase_sun) %>% 
  count(altitude_bin)

























