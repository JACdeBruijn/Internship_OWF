if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, progress, icesTAF, timetk)

rm(list=ls())

getwd()

sourceDir(file.path('.','function'))

figurePath    <- file.path('.','figures')
dataPath      <- file.path('.','data')
resultPath    <- file.path('.','results')

# Loading the META data
WBAT.tab <- file.path(dataPath, 'survey_db.csv') %>% 
  read_csv() %>%
  select(1:5, 8, 9) %>%
  mutate(dataSet_station = str_c(dataSet, '_', station))

overview.tab <- file.path(dataPath, 'data_overview.csv') %>% 
  read_csv() %>% 
  mutate(stationSet = str_c(dataSet, '_', station),
         pairingName = str_c(dataSet, '_', pairing)) %>% 
  select(-c('year', 'dataSet', 'station'))

# Loading the R workspace with CPOD and WBAT 
load(file = file.path(resultPath,'CPOD_WBAT_workspace.RData'))

# Data wrangling on removing the unneeded data sets 
# WBAT.all.summary <- WBAT.all.summary %>%                                      # This has been filtered out in script 2_data_format
#   filter(!dataSet %in% c("2022-cpower", "2022-HKZ", "2023-HKN"))

WBAT_50dB_tres70 <- WBAT.all.summary %>% 
  filter(treshold == "-50", frequency == "70")

# I need to figure out how the 0's are effecting the data 

ggplot(WBAT_50dB_tres70, aes(x = log10(SA))) +
  geom_histogram(binwidth = .1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of SA Values at -50dB for 70kHz",
       x = "log10(SA)",
       y = "Frequency") +
  theme_minimal() +
  facet_grid(pairingName ~ type)


WBAT_50dB_tres70 %>% 
  group_by(stationSet, type) %>% 
  summarise(mean_SA = mean(SA, na.rm = T))

ggplot(subset(WBAT_50dB_tres70, WBAT_50dB_tres70$pairingName == "2024-BE_1"), aes(x = SA)) +
  geom_histogram(binwidth = .00000001, fill = "skyblue", color = "black") +
  labs(title = "Distribution of SA Values at -50dB for 70kHz",
       x = "log10(SA)",
       y = "Frequency") +
  theme_minimal() +
  facet_grid(pairingName ~ type)

summary(WBAT_50dB_tres70$SA)

hist(WBAT_50dB_tres70$SA,
     main = "Distribution of SA Values",
     xlab = "SA",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")


# Plotting the WBAT data 

print(ggplot(WBAT.all.summary,aes(x=station,y=log10(SA)))+
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
        facet_grid(frequency~dataSet,scales = 'free_x')+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle('SA all stations'))


ggplot(WBAT.all.summary, aes(x = station, y = log10(SA))) +
  geom_boxplot()+
  labs(
    title = "Hourly log SA over time per pair",
    x = "Datetime",
    y = "log10(SA)",
    color = "Time of Day") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.5, linetype = "dotted")) +
  facet_grid(pairingName ~ type)


ggplot(WBAT.all.summary, aes(x = as.factor(time), y = log10(SA))) +
  geom_boxplot()+
  labs(
    title = "Hourly log SA over time per pair",
    x = "Datetime",
    y = "log10(SA)",
    color = "Time of Day"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.5, linetype = "dotted")
  ) +
  facet_grid(pairingName ~ type)
