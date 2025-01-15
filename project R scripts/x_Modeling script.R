################################################################################-
# Modeling SA - 14/01/25 ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, mgcv)

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

# Removing duplicate rows that are there for some reason
WBAT.all.summary <- WBAT.all.summary %>%
  distinct(IDinter, .keep_all = TRUE)

################################################################################-
# Working on the WBAT data ----
################################################################################-

# Wrangling the data - removing columns; filtering threshold, frequency and n; mutating columns
WBAT_modeling <- WBAT.all.summary %>% 
    select(-phase, -station, -dataSet, -SAsd, -depthSA, -depthIntegration, -freq, 
         -date_start, -date_end, -soundtrap, -CPOD, -WBAT_70, -WBAT_200,
         -pairing, -sunPos_azimuth, -hourSunset, -hourSunrise, -sunset,
         -sunrise) %>% 
  filter(treshold %in% c("-50"),
         frequency %in% c("70"),
         n >= 8) %>% 
  mutate(stationSet = factor(stationSet),
         type = factor(type, levels = c("control", "OWF"), label = c("Control", "OWF")),
         year = as.factor(format(datetime, format = "%y")),
         month = as.numeric(format(datetime, format = "%m")),
         week_num = as.numeric(format(datetime, format = "%W")),
         ToD = as.numeric(format(datetime, format = "%H")),
         log10_SA = log10(SA))

# Plotting SA normal
ggplot(WBAT_modeling, aes(x = (SA))) + geom_histogram()

# Plotting SA in the log 
ggplot(WBAT_modeling, aes(x = log10(SA))) + geom_histogram()



################################################################################-
# Making the first and simplest models

# Model a simplest model
gam_1 <- gam(SA ~ type,
             data = WBAT_modeling, method = "REML")


# Making the model more complicated
gam_2 <- gam(SA ~ type,
             data = WBAT_modeling, method = "REML",
             family = Gamma(link = 'log'))

# Model the log of the simplest model
gam_3 <- gam(log10_SA ~ type,
             data = WBAT_modeling, method = "REML")

# Checking the model performance 
AIC(gam_1, gam_2, gam_3)

# looking at the summary of the model
summary(gam_2)



################################################################################-
# Trying to make the models more complex
# Some diagnostics 
str(WBAT_modeling)
sapply(WBAT_modeling, function(x) length(unique(x))) # If the number of unique values is less than k, you need to reduce k for that term.
sapply(WBAT_modeling, function(x) sum(is.na(x)))  # Check for NAs in the dataset

# First complexer model
gam_4 <- bam(log10_SA ~ 
               type +
               s(ToD, k = 10) +
               s(week_num, k = 12) +
               s(month, k = 8) +
               s(lat, lon, k = 100, bs = "ds", m = c(1, 0.5)) +
               s(stationSet, bs = "re") +   
               s(year, bs = "re"),
             data = WBAT_modeling, method = "fREML", 
             nthreads = c(4, 1), discrete = TRUE)

# second complexer model
gam_5 <- bam(log10_SA ~ 
               type +
               s(ToD, k = 10) +                     
               s(week_num, k = 10) +                
               s(month, bs = "cc", k = 6) +         
               ti(ToD, by = type, k = 10) + 
               ti(week_num, by = type, k = 10) +
               ti(month, by = type, k = 6) +
               ti(ToD, week_num, k = c(10, 10)) + 
               s(stationSet, bs = "re") +           
               s(year, bs = "re"),
             data = WBAT_modeling, method = "fREML", 
             nthreads = c(4, 1), discrete = TRUE)

# third completer model
gam_6 <- bam(log10_SA ~ 
               type +
               s(ToD, k = 10, bs = "cc") +                     
               s(week_num, k = 10) +                
               ti(ToD, by = type, k = 10) + 
               ti(week_num, by = type, k = 10) +
               ti(ToD, week_num, k = c(10, 10)) + 
               stationSet +           
               year,
             data = WBAT_modeling, method = "fREML", 
             nthreads = c(4, 1), discrete = TRUE)

# without the interactions again
gam_7 <- bam(log10_SA ~ 
               type +                                                # a small effect of type
               s(ToD, k = 10, bs = "cc") +                           # a large effect of Time of Day scale
               s(week_num, k = 12) +                                 # a large effect of week_num scale
               s(month, k = 8) +                                     # a small effect of month scale
               s(lat, lon, k = 10, bs = "ds", m = c(1, 0.5)) +       # Little effect of spatial aspect
               s(stationSet, bs = "re") +                            # Some effect of stationSet 
               s(year, bs = "re"),                                   # Year has little effect
             data = WBAT_modeling, method = "fREML", 
             nthreads = c(4, 1), discrete = TRUE)

# With factor type interactions to see the different interaction options - testing and going wild
gam_8 <- bam(log10_SA ~ 
               s(ToD, type, k = 10, bs = "fs") +                        
               s(week_num, type, k = 10, bs = "fs") +                               
               s(month, type, k = 6, bs = "fs") +                              
               s(lat, lon, k = 10, bs = "ds", m = c(1, 0.5)) +
               s(stationSet, bs = "re") +                           
               s(year, bs = "re"),                                 
             data = WBAT_modeling, method = "fREML", 
             nthreads = c(4, 1), discrete = TRUE, select = TRUE)

gam_9 <- bam(log10_SA ~ 
               type +
               s(ToD, k = 10, bs = "cc") +
               # s(sunPos_altitude, k = 10) +
               s(week_num, k = 12) +
               s(month, k = 8) +
               s(lat, lon, k = 100, bs = "ds", m = c(1, 0.5)) +
               s(stationSet, bs = "re") +   
               s(year, bs = "re"),
             data = WBAT_modeling, method = "fREML", 
             nthreads = c(4, 1), discrete = TRUE)

# Checking the model performance 
AIC(gam_3, gam_4, gam_5, gam_6, gam_7, gam_8, gam_9)
AIC(gam_7)

# looking at the summary of the model
summary(gam_9)
gam.check(gam_7)
plot(gam_9)


################################################################################-
# Working on the CPOD data ----
################################################################################-

# Prepairing the CPOD data for the model
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
  rename(type = type.x) %>% 
  mutate(type = factor(type, levels = c("control", "OWF"), label = c("Control", "OWF")),
         across(c(buzz_pos_minutes, buzz_ratio), ~ if_else(.x == 0, NA, .x)))

CPOD_all_hour <- CPOD.all.hour %>% 
  mutate(across(c(buzz, inter, other, all_buzz, all_no_buzz, all, pos_minutes, buzz_pos_minutes), 
                ~ replace_na(.x, 0))) %>%  # Mutate all the NA values of the columns to ZERO 
  mutate(pph = if_else(pos_minutes == 0, 0, 1),
         ppbuzzh = if_else(buzz_pos_minutes == 0, 0, 1))

# Writing it to a seperate vector; with mutating additonal things and deselecting serveral columns 
CPOD_modeling <- CPOD_all_hour %>% 
  mutate(stationSet = factor(stationSet),
         year = as.factor(format(time_hour, format = "%y")),
         month = as.numeric(format(time_hour, format = "%m")),
         week_num = as.numeric(format(time_hour, format = "%W")),
         DoY = as.numeric(format(time_hour, format = "%j")),
         ToD = as.numeric(format(time_hour, format = "%H")),
         PODtype = "CPOD",
         PODtype = if_else(stationSet == "2024-BE_grafton", "FPOD", PODtype),
         PODtype = factor(PODtype, levels = c("CPOD", "FPOD"))) %>% 
  select(-time_hour_num, -time_hour, -dataSet, -buzz, -inter, -other, -all_buzz, -all,
         -all_no_buzz, -day, -hour, -month_year, -sunPos_azimuth, -hourSunset, 
         -hourSunrise, -sunset, -sunrise, -station, -freq, -date_start, -date_end,
         -soundtrap, -CPOD, -WBAT_70, -WBAT_200, -pairing, -type.y, -pairingName)


# Plotting CPOD in the pph
ggplot(CPOD_modeling, aes(x = pph)) + geom_histogram()

# Plotting CPOD in the pos_minutes 
ggplot(CPOD_modeling, aes(x = pos_minutes)) + geom_histogram()

################################################################################-
# Making the first and simplest models
# I have a bionomonal data set!!!!!

# Model a simplest model
GPOD_1 <- gam(pph ~ type,
             data = CPOD_modeling, method = "REML")


# Making the model more complicated
GPOD_2 <- gam(pph ~ type,
              data = CPOD_modeling, method = "REML",
              family = binomial(link = "logit"))

# Checking the model performance 
AIC(GPOD_1, GPOD_2)

# looking at the summary of the model
summary(GPOD_1)
gam.check(GPOD_1)
plot(GPOD_1)

str(CPOD_modeling)
sapply(CPOD_modeling, function(x) length(unique(x))) # If the number of unique values is less than k, you need to reduce k for that term.
sapply(CPOD_modeling, function(x) sum(is.na(x))) 

################################################################################-
# First complexer model
GPOD_3 <- bam(pph ~ 
                type +
                PODtype +
                s(ToD, k = 10) +
                s(week_num, k = 12) +
                s(month, k = 8) +
                s(lat, lon, k = 10, bs = "ds", m = c(1, 0.5)) +
                s(stationSet, bs = "re") +   
                s(year, bs = "re"),
              data = CPOD_modeling, method = "fREML", 
              nthreads = c(4, 1), discrete = TRUE,
              family = binomial(link = "logit"))

# First complexer model
GPOD_4 <- bam(pph ~ 
               type +
               dayNight +
               PODtype +
               s(ToD, k = 10) +
               s(DoY, k = 10) +
               s(week_num, k = 12) +
               s(month, k = 8) +
               s(lat, lon, k = 10, bs = "ds", m = c(1, 0.5)) +
               s(stationSet, bs = "re") +   
               s(year, bs = "re"),
             data = CPOD_modeling, method = "fREML", 
             nthreads = c(4, 1), discrete = TRUE,
             family = binomial(link = "logit"))

# First complexer model
GPOD_5 <- bam(pph ~ 
                type +
                PODtype +
                s(ToD, k = 10, bs = "cc") +
                s(week_num, k = 12) +
                s(month, k = 8) +
                s(lat, lon, k = 10, bs = "ds", m = c(1, 0.5)) +
                s(stationSet, bs = "re") +   
                s(year, bs = "re"),
              data = CPOD_modeling, method = "fREML", 
              nthreads = c(4, 1), discrete = TRUE,
              family = binomial(link = "logit"))

# First complexer model
GPOD_6 <- bam(pph ~ 
                type +
                PODtype +
                dayNight +
                s(ToD, k = 10, bs = "cc") +
                s(DoY, k = 10) +
                s(stationSet, bs = "re") +   
                s(year, bs = "re"),
              data = CPOD_modeling, method = "fREML", 
              nthreads = c(4, 1), discrete = TRUE,
              family = binomial(link = "logit"))

AIC(GPOD_1, GPOD_2, GPOD_3, GPOD_4, GPOD_5)
summary(GPOD_3)
gam.check(GPOD_3)
plot(GPOD_3)


################################################################################-
GPOD_pos1 <- bam(pos_minutes ~
                   type +
                   PODtype +
                   s(ToD, k = 10, bs = "cc") +
                   s(week_num, k = 12) +
                   s(month, k = 8) +
                   s(lat, lon, k = 10, bs = "ds", m = c(1, 0.5)) +
                   s(stationSet, bs = "re") +   
                   s(year, bs = "re"),
                 data = CPOD_modeling, method = "fREML", 
                 nthreads = c(4, 1), discrete = TRUE,
                 family = poisson(log))













   
