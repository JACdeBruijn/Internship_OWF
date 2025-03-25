################################################################################-
# Combining the to data sets to see effect of WBAT on HP - 31/01/25 ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, mgcv, RColorBrewer, mgcv, ggstar, gratia, visualize, infer, ggtext, lattice, nlme, lmtest)

rm(list=ls())

sourceDir(file.path('.','function'))
figurePath    <- file.path('.','figures')
dataPath      <- file.path('.','data')
resultPath    <- file.path('.','results')

################################################################################-
# Loading in the data ----
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

############################################################################-
# Prepping the SA data ----
WBAT.all.summary <- WBAT.all.summary %>%
  distinct(IDinter, .keep_all = TRUE)

WBAT_sample <- WBAT.all.summary %>% 
  filter(treshold %in% c("-50"),
         frequency %in% c("70", "200"),
         n >= 8) %>% 
  mutate(day = as.Date(datetime, "%Y-%m-%d", tz = "UTC"),
         ToD = as.numeric(format(datetime, format = "%H")),
         day_ToD = paste(format(as.Date(datetime, tz = "UTC"), "%Y-%m-%d"), 
                         format(datetime, format = "%H"), 
                         sep = " "))
