### Loading packages ###
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, leaflet, sf)

rm(list=ls())

### Loading data ####
head(CPOD.all.day)


### Wrangling data ####
CPOD.all.day %>% 
  group_by(stationName, type) %>% 
  summarise(buzz = sum(buzz, na.rm = T)) %>% 
  arrange(buzz)
      
CPOD.all.day %>% 
  group_by(time_day, buzz_pos_minutes, stationName) %>% 
  ggplot(aes(x = time_day, y = buzz_pos_minutes, color = stationName)) +
  geom_point() 
