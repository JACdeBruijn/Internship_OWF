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
p1_WBAT <- ggplot(subset(Sunny, phase_sun %in% c("Morning", "Dawn")), aes(x = factor(altitude_bin), y = log10(SA), fill = type)) +
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
  labs(x = "Sun altitude at Sunrise (deg)", y = "Mean log<sub>10</sub>(S<sub>A</sub>)") +
  scale_fill_manual(values = c("Control" = "#388fe0", "OWF" = "#ccac3d")) +
  # facet_grid(rows = "type") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.title.y = element_markdown(size = 25),
    axis.text.y = element_text(size = 20, color = "black"),
    # strip.text.y = element_blank()
    )

# Plotting the 3 and the 4 phases
p2_WBAT <- ggplot(subset(Sunny, phase_sun %in% c("Evening", "Night")), aes(x = factor(altitude_bin), y = log10(SA), fill = type)) +
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
    name = "Sun altitude at Sunset (deg)",
    labels = function(x) ifelse(x == "0", "Sunrise/Sunset", x),
    limits = rev(levels(factor(subset(Sunny, phase_sun %in% c("Evening", "Night"))$altitude_bin)))) +
  scale_fill_manual(values = c("Control" = "#388fe0", "OWF" = "#ccac3d")) +
  # facet_grid(rows = "type") +
  theme_minimal() +
  theme(
    legend.position="none",
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),    
    axis.line.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20, color = "black"),
    plot.margin=unit(c(5.5, 5.5, 5.5, -15), "pt")
    )


plot <- grid.arrange(p1, p2, nrow = 1)

station_sets_per_sunPos <- Sunny %>%
  group_by(phase_sun, altitude_bin, type) %>%
  summarise(unique_station_sets = n_distinct(stationSet, na.rm = T), .groups = "drop") %>% 
  mutate(unique_station_sets = if_else(is.na(unique_station_sets), 0L, unique_station_sets))


# Create a plot to show the number of unique station sets per week_num            aes(x = as.factor(week_num), y = unique_station_sets, fill = type)) +
p3_WBAT <-
  ggplot(subset(station_sets_per_sunPos, phase_sun %in% c("Morning", "Dawn")), aes(x = factor(altitude_bin), y = unique_station_sets, fill = type)) +
  annotation_custom(vertical_gradient_morning, 
                    xmin = 4.45, xmax = 6.5, 
                    ymin = -Inf, ymax = Inf) +
  annotate("rect", 
           xmin = -Inf, xmax = 4.5, 
           ymin = -Inf, ymax = Inf, 
           fill = "black") +
  geom_vline(xintercept = 6.5, linetype = "dashed") +
  geom_bar(stat = "identity", position = "dodge", width = .8) +
  scale_fill_manual(values = c("#388fe0", "#ccac3d")) +
  # geom_text(aes(label = unique_station_sets), vjust = .2, size = 4) +
  labs(x = "Sun altitude at Sunrise (deg)", y = "Number of station") +
  theme_pubclean() +
  theme(
    axis.title.x = element_markdown(size = 25),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.title.y = element_markdown(size = 25),
    axis.text.y = element_text(size = 20, color = "black"),
    legend.position = "none"
  )

p4_WBAT <-
  ggplot(subset(station_sets_per_sunPos, phase_sun %in% c("Evening", "Night")), aes(x = factor(altitude_bin), y = unique_station_sets, fill = type)) +
  annotation_custom(vertical_gradient_evening, 
                    xmin = 7.5, xmax = 9.5, 
                    ymin = -Inf, ymax = Inf) +
  annotate("rect", 
           xmin = 9.5, xmax = Inf, 
           ymin = -Inf, ymax = Inf, 
           fill = "black") +
  geom_vline(xintercept = 7.5, linetype = "dashed") +
  geom_bar(stat = "identity", position = "dodge", width = .8) +
  scale_x_discrete(
    name = "Sun altitude at Sunset (deg)",
    labels = function(x) ifelse(x == "0", "Sunrise/Sunset", x),
    limits = rev(levels(factor(subset(station_sets_per_sunPos, phase_sun %in% c("Evening", "Night"))$altitude_bin)))) +
  scale_fill_manual(values = c("#388fe0", "#ccac3d")) +
  # geom_text(aes(label = unique_station_sets), vjust = .2, size = 4) +
  theme_pubclean() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),    
    axis.line.y = element_blank(),
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 20, color = "black"),
    plot.margin=unit(c(5.5, 5.5, 5.5, -15), "pt")
  )

lay_WBAT_sun <- rbind(c(1, 2),
                      c(1, 2),
                      c(3, 4))

WBAT_sun <-
  grid.arrange(p1_WBAT, p2_WBAT, p3_WBAT, p4_WBAT, 
               layout_matrix = lay_WBAT_sun)

lay_PODandWBAT_sunny <- rbind(c(1),
                              c(2))

WBAT_sun_andCPOD <-
  grid.arrange(WBAT_sun, CPOD_sun, 
               widths = c(1),
               layout_matrix = lay_PODandWBAT_sunny)

# Commented out so that I do not accidentally enable it
ggsave(filename = file.path(figurePath,'Combining the CPOD and WBAT for sunny.png'), plot = WBAT_sun_andCPOD, width = 14, height = 16)









# Commented out so that I do not accidentally enable it
ggsave(filename = file.path(figurePath,'Sun altitude around sunset and sunrise with light gradient_3_draft.png'), plot = WBAT_sun, width = 14, height = 10)

# Controling the numbers
effe_tellen <- Sunny %>%
  count(altitude_bin) %>% 
  arrange(n)

effe_anderevolgorde <- Sunny %>%
  group_by(phase_sun) %>% 
  count(altitude_bin)

























