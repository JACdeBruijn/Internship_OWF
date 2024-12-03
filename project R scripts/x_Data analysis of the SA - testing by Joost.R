if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, progress, icesTAF, timetk)

rm(list=ls())

#setwd('D:/PAM_RABO/finless_neonate_HK')
# setwd('G:/git/APELAFICO_OWF_study/')
#setwd('G:/git/WBAT_APELAFICO')
getwd()

sourceDir(file.path('.','function'))

figurePath    <- file.path('.','figures')
dataPath      <- file.path('.','data')
resultPath    <- file.path('.','results')

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
load(file = file.path(resultPath,'CPOD_WBAT_workspace.RData'))
################################################################################

WBAT.all.summary <- WBAT.all.summary %>% 
  group_by(frequency, station, phase, dataSet) %>% 
  mutate(depthMaxR = depthIntegration / max(depthIntegration))

################################################################################
# Subsetting on -50dB
################################################################################

WBAT.current <- subset(WBAT.all.summary, pairing != -1)
WBAT.pairwise <- subset(WBAT.all.summary, pairing != -1 & depthMaxR > 0.25)
WBAT.pairwise <- subset(WBAT.all.summary, pairing != -1 & n >= 8 & depthMaxR > 0.25 & treshold == -50)#& frequency == 70 

hist(WBAT.all.summary$depthIntegration)
hist(WBAT.current$depthMaxR)
hist(WBAT.pairwise$depthMaxR)


test <- WBAT.all.summary %>% 
  filter(n == 20) 

unique(test$stationSet)

################################################################################
# Working with the n = 11 slice
################################################################################
# give all a stationSet
WBAT.all.summary <- WBAT.all.summary %>% 
  mutate(stationSet = str_c(dataSet, '_', station))

################################################################################
# count the n of rows in WBAT.all
# all.summary <- WBAT.all %>% 
#   group_by(stationSet, treshold, IDinter, frequency, phase) %>% 
#   summarize(n=n())

# Make a summary 
WBAT.all_sum <- WBAT.all.summary %>% 
  group_by(stationSet, treshold, IDinter, frequency, phase) %>% 
  summarize(n=n(),
            station = unique(station),
            dataSet = unique(dataSet),
            SAsd = sd(SA, na.rm=T),
            SA = mean(SA, na.rm=T),
            datetime = first(datetime),
            depthIntegration = mean(depthIntegration, na.rm=T),
            type = type
            )

WBAT.all_sum <- WBAT.all_sum %>% 
  filter(treshold == -50)

subset(WBAT.all_sum, treshold == -50) %>% 
  group_by(stationSet, type) %>% 
  summarize(meanSA = mean(SA))

summary(WBAT.all_sum$n)

################################################################################
# count the n of rows in WBAT.join
join.summary <- WBAT.all %>% 
  group_by(stationSet, treshold, IDinter, frequency, phase) %>% 
  summarize(n=n())

#sample_n(11,replace = FALSE)
WBAT.join <- subset(WBAT.all, IDinter %in% subset(join.summary, n >= 6)$IDinter) %>%
  group_by(stationSet, treshold, IDinter, frequency, phase) %>%
  slice(sample(n(), min(11, n())))

# summarize WBAT data per intervals
WBAT.join.summary <- WBAT.join %>% 
  group_by(stationSet, treshold, IDinter, frequency, phase) %>% 
  summarize(n=n(),
            station=unique(station),
            dataSet=unique(dataSet),
            SAsd=sd(SA,na.rm=T),
            SA=mean(SA,na.rm=T),
            datetime=first(datetime),
            depthIntegration=mean(depthIntegration,na.rm=T))

WBAT.join.summary <- WBAT.join.summary %>% 
  filter(treshold == -50)

subset(WBAT.join.summary, treshold == -50) %>% 
  group_by(stationSet) %>% 
  summarize(meanSA = mean(SA))

################################################################################

ggplot(WBAT.join.summary, aes(x = datetime, y = SA)) +
  geom_line() +
  ggtitle('join')

ggplot(WBAT.all_sum, aes(x = datetime, y = log10(SA))) +
  geom_line() +
  ggtitle('all')





WBAT.all %>%
  group_by(stationSet, treshold, IDinter, frequency, phase) %>%
  filter(treshold == -50) %>% 
  summarize(datetime = first(datetime))
  

nrow(WBAT.all)  
nrow(WBAT.all.summary)




WBAT.all.summary <- WBAT.all %>%
  group_by(stationSet, treshold, IDinter, frequency, phase) %>%
  summarize(
    n = n(),
    station = unique(station),
    dataSet = unique(dataSet),
    SAsd = sd(SA, na.rm = TRUE),
    SA = mean(SA, na.rm = TRUE),
    datetime_min = min(datetime),
    datetime_max = max(datetime),
    depthIntegration = mean(depthIntegration, na.rm = TRUE)
  ) %>%
  filter(treshold == -50)

WBAT.all %>%
  group_by(IDinter) %>%
  summarize(unique_datetimes = n_distinct(datetime))

WBAT.all %>%
  filter(IDinter == 1) %>%
  summarize(unique_datetimes = n_distinct(datetime))

print(n = "inf", WBAT.all %>%
                     filter(IDinter == 1) %>%
                     distinct(datetime) %>% 
                     arrange(datetime))

################################################################################
# Trying to correct the IDinter fout

WBAT.correct.ID <- WBAT.all %>% 
  filter(treshold == -50)

print(n = "inf", WBAT.correct.ID %>%
        filter(IDinter == 1) %>%
        distinct(datetime) %>% 
        arrange(datetime))

ggplot(WBAT.correct.ID, aes(x = datetime, y = IDinter)) +
  geom_line()


################################################################################
# Looking in to the CPOD_WBAT_workspace how is it build and how is the 11 slice effecting it

WBAT.test <- WBAT.all.summary %>% 
  filter(treshold == -50,
         stationSet == "2021-BE_grafton",
         frequency == "70")

ggplot(WBAT.test, aes(x = datetime, y = log10(SA), color = as.factor(phase))) +
  geom_point() +
  scale_color_manual(values = c("P1" = "red", "P2" = "blue")) +
  ggtitle('all') +
  theme_minimal()

ggplot(subset(WBAT.test,
              datetime > as.POSIXct("2021-08-16 0:05:29", tz = "UTC") & 
                datetime < as.POSIXct("2021-08-16 18:05:29", tz = "UTC")),
       aes(x = datetime, y = SA, color = as.factor(phase))) +
  geom_point() +
  scale_color_manual(values = c("P1" = "red", "P2" = "blue")) +
  ggtitle('all') +
  theme_minimal()


p1_dates <- WBAT.test %>% filter(phase == 1) %>% pull(datetime)
p2_dates <- WBAT.test %>% filter(phase == 2) %>% pull(datetime)

# Find overlapping datetimes
common_dates <- intersect(p1_dates, p2_dates)





table(WBAT.all.summary$n)




