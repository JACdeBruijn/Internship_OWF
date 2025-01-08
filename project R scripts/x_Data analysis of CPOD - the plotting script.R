################################################################################-
# Script for plotting CPOD data ----
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

################################################################################-
# Loading the data ----
WBAT.tab <- file.path(dataPath, 'survey_db.csv') %>% 
  read_csv() %>%
  select(1:5, 8, 9) %>%
  mutate(dataSet_station = str_c(dataSet, '_', station))

overview.tab <- file.path(dataPath, 'data_overview.csv') %>% 
  read_csv() %>% 
  mutate(stationSet = str_c(dataSet, '_', station),
         pairingName = str_c(dataSet, '_', pairing)) %>% 
  select(-c('year', 'dataSet', 'station'))

load(file = file.path(resultPath,'CPOD_WBAT_workspace.RData'))

################################################################################- 
# Adjust CPOD data frames----
CPOD.all.day <- CPOD.all.day %>%                                                # Fixing names and adding META info
  mutate(station = stationName,
         stationSet = str_c(dataSet, '_', station)) %>%
  mutate(stationSet = case_when(
    stationSet == "2023-BSW_274174" ~ "2023-BSW_274174 BSW1",
    stationSet == "2023-BSW_278093" ~ "2023-BSW_278093 BSW2",
    stationSet == "2023-BSW_267814" ~ "2023-BSW_267814 BSW3",
    stationSet == "2023-BSW_267838" ~ "2023-BSW_267838 BSW4",
    TRUE ~ stationSet)) %>%
  select(-lat, -lon) %>%
  left_join(overview.tab, by = "stationSet") %>% 
  rename(type = type.x)
  
CPOD.all.day <- CPOD.all.day %>%
  mutate(type = factor(type, levels = c("control", "OWF"), label = c("Control", "OWF"))) %>%
  select(time_day_num, time_day, date_start, date_end, stationName, stationSet, 
         pairing, pairingName, everything(),
         -day, -month_year, -station, 
         # -hourSunrise, -hourSunset, -sunset, -sunrise, 
         -freq, -lat, -lon, -soundtrap, -CPOD, -WBAT_70, -WBAT_200, -type.y)

CPOD_all_day_noNA <- CPOD.all.day %>% 
  mutate(across(c(buzz, inter, other, all_buzz, all_no_buzz, all, buzz_ratio, pos_minutes, buzz_pos_minutes), 
                ~ replace_na(.x, 0))) # Mutate all the NA values of the columns to ZERO 

CPOD.all.hour <- CPOD.all.hour %>%
  mutate(station = stationName,
         stationSet = str_c(dataSet, '_', station)) %>%
  mutate(stationSet = case_when(
    stationSet == "2023-BSW_274174" ~ "2023-BSW_274174 BSW1",
    stationSet == "2023-BSW_278093" ~ "2023-BSW_278093 BSW2",
    stationSet == "2023-BSW_267814" ~ "2023-BSW_267814 BSW3",
    stationSet == "2023-BSW_267838" ~ "2023-BSW_267838 BSW4",
    TRUE ~ stationSet)) %>%
  select(-lat, -lon) %>%
  left_join(overview.tab, by = "stationSet") %>% 
  rename(type = type.x)

CPOD.all.hour <- CPOD.all.hour %>%
  mutate(type = factor(type, levels = c("control", "OWF"), label = c("Control", "OWF"))) %>%
  select(time_hour_num, time_hour, date_start, date_end, stationName, stationSet, 
         pairing, pairingName, everything(),
         -day, -month_year, -station,
         # -hourSunrise, -hourSunset, -sunset, -sunrise, 
         -freq, -lat, -lon, -soundtrap, -CPOD, -WBAT_70, -WBAT_200, -type.y) 

CPOD_all_hour_noNA <- CPOD.all.hour %>% 
  mutate(across(c(buzz, inter, other, all_buzz, all_no_buzz, all, buzz_ratio, pos_minutes, buzz_pos_minutes), 
                ~ replace_na(.x, 0))) # Mutate all the NA values of the columns to ZERO 

CPOD.all.min <- CPOD.all.min %>%
  mutate(station = stationName,
         stationSet = str_c(dataSet, '_', station)) %>%
  mutate(stationSet = case_when(
    stationSet == "2023-BSW_274174" ~ "2023-BSW_274174 BSW1",
    stationSet == "2023-BSW_278093" ~ "2023-BSW_278093 BSW2",
    stationSet == "2023-BSW_267814" ~ "2023-BSW_267814 BSW3",
    stationSet == "2023-BSW_267838" ~ "2023-BSW_267838 BSW4",
    TRUE ~ stationSet)) %>%
  select(-lat, -lon) %>%
  left_join(overview.tab, by = "stationSet") %>% 
  rename(type = type.x)

CPOD.all.min <- CPOD.all.min %>%   
  mutate(type = factor(type, levels = c("control", "OWF"), label = c("Control", "OWF"))) %>%
  select(time_minute, date_start, date_end, stationName, stationSet, 
         pairing, pairingName, everything(),
         # -hourSunrise, -hourSunset, -sunset, -sunrise, 
         -station, -freq, -lat, -lon, -soundtrap, -CPOD, -WBAT_70, -WBAT_200, -type.y) 

CPOD_all_min_noNA <- CPOD.all.min %>% 
  mutate(across(c(buzz, inter, other, all_buzz, all_no_buzz, all, buzz_ratio), 
                ~ replace_na(.x, 0))) # Mutate all the NA values of the columns to ZERO 

################################################################################-
# Working on CPOD overview ----

hist(CPOD.all.hour$pos_minutes)
hist(CPOD_all_hour_noNA$pos_minutes)

hist(CPOD.all.hour$buzz)
hist(CPOD.all.hour$buzz_pos_minutes)
hist(CPOD.all.hour$buzz_ratio)
str(CPOD.all.hour)

ggplot(CPOD.all.hour, aes(x = type, y = pos_minutes), fill = type) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 10)) +
  facet_grid(~ pairingName)

ggplot(CPOD.all.day, aes(x = type, y = pos_minutes), fill = type) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 200)) +
  facet_grid(~ pairingName)

























# Plotting the overview of CPOD based on day data

CPOD_all_day <- CPOD.all.day %>% 
  group_by(stationSet, pairingName, type) %>% 
  summarise(mean_pos_min = mean(pos_minutes, na.rm = TRUE),
            mean_buzz_pos_min = mean(buzz_pos_minutes, na.rm = TRUE))

ggplot(subset(CPOD_all_day, stationSet == "2021-BE_belwind"), aes(x = pairingName, y = mean_pos_min)) +
  geom_boxplot() +
  facet_grid(~pairingName) +
  theme_classic()
  
ggplot(CPOD_all_day, aes(x = type, y = mean_pos_min)) +
    geom_boxplot()
# +
#   facet_grid(~pairingName) +
#   theme_classic()

CPOD_all_hour <- CPOD.all.hour %>% 
  group_by(stationSet, pairingName, pairing, type) %>% 
  summarise(mean_pos_hour = mean(pos_minutes, na.rm = TRUE),
            mean_buzz_pos_hour = mean(buzz_pos_minutes, na.rm = TRUE))

ggplot(CPOD_all_hour, aes(x = type, y = mean_pos_hour)) +
  geom_boxplot() +
  theme_classic() 
  # facet_grid(~pairingName)

unique(CPOD.all.day$stationSet)                                                 # 2021-BE_cpower is now added to the CPOD data!
unique(WBAT.all.summary$stationSet)
unique(CPOD.all$PODid)

ggplot(CPOD_all_day, aes(x = type, y = mean_pos_min, group = pairingName, color = factor(pairingName))) +
  geom_point(stat = "summary", fun = mean, size = 3, aes(color = NULL)) +
  geom_path(aes(group = pairingName), linetype = "dashed", linewidth = 0.5, color = "red")
  
  
  
(ggplot(CPOD.all.hour, aes(x = type, y = pos_minutes, fill = dayNight))) +
  geom_boxplot() +
  labs(x = "Day or Night", y = "") +
  theme_minimal() 



# Sub-setting on threshold -50 & mutating hour
CPOD_min <- CPOD.all.min %>%
  mutate(minuut = as.numeric(format(time_minute, "%M")))


hist(CPOD_min$minuut)






