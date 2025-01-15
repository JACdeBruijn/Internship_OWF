################################################################################-
# Combining the to data sets and plotting them - 15/01/25 ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, mgcv, RColorBrewer, mgcv)

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
         frequency %in% c("70"),
         n >= 8) %>% 
  mutate(day = as.Date(datetime, "%Y-%m-%d", tz = "UTC"),
         ToD = as.numeric(format(datetime, format = "%H")),
         day_ToD = paste(format(as.Date(datetime, tz = "UTC"), "%Y-%m-%d"), 
                         format(datetime, format = "%H"), 
                         sep = " "))

############################################################################-
# Prepping the CPOD data ----
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
  select(time_hour, stationSet, pairingName, stationName, dataSet, type, 
         lat, lon, -day, everything(),
         -freq, -date_start, -date_end, -soundtrap, -CPOD, 
         -WBAT_70, -WBAT_200, -pairing, -type.y, -station)

CPOD_sample <- CPOD.all.hour %>% 
  mutate(day = as.Date(time_hour, "%Y-%m-%d", tz = "UTC"),
         ToD = as.numeric(format(time_hour, format = "%H")),
         day_ToD = paste(format(as.Date(time_hour, tz = "UTC"), "%Y-%m-%d"), 
                         format(time_hour, format = "%H"), 
                         sep = " ")) %>% 
  mutate(across(c(buzz, inter, other, all_buzz, all_no_buzz, all, pos_minutes, buzz_pos_minutes), 
                       ~ replace_na(.x, 0))) %>%  # Mutate all the NA values of the columns to ZERO 
  mutate(pph = if_else(pos_minutes == 0, 0, 1))

############################################################################-
# Combining data on day_ToD ----
flagFirst <- T
for(stationSet in unique(WBAT_sample$stationSet)){
  idx.WBAT <- WBAT_sample$stationSet == stationSet
  
  if(!(unique(WBAT_sample$dataSet[idx.WBAT]) %in% c('2022-HKZ','2023-HKN'))){
    CPOD.stationSet <- stationSet
    CPOD.current <- subset(CPOD_sample, stationSet == CPOD.stationSet)
    
    WBAT.all.day <- WBAT_sample[idx.WBAT,] %>% 
      group_by(day_ToD) %>% 
      summarize(depthIntegration = mean(depthIntegration, na.rm = T),
                depthSA = mean(depthSA, na.rm = T),
                SA = mean(SA, na.rm = T))
    
    df.join <- left_join(WBAT.all.day, CPOD.current, by = c('day_ToD'))
    df.join$station <- unique(WBAT_sample$station[idx.WBAT])
    df.join$dataSet <- unique(WBAT_sample$dataSet[idx.WBAT])
    
    if(flagFirst){
      df.join.all <- df.join
      flagFirst <- F
    }else{
      df.join.all <- rbind(df.join.all, df.join)
    }
  }
}

############################################################################-
# Cleaning and mutating some things to joined data ----
joined_WBAT_CPOD <- df.join.all %>% 
  filter(!is.na(type)) %>%                                                  # The are some WBAT rows for which there is no CPOD data. These are removed
  select(time_hour, stationSet, pairingName, stationName, dataSet, type, 
         lat, lon, everything(),
         -depthIntegration, -depthSA, -time_hour_num, -buzz, -inter,
         -other, -all_buzz, -all_no_buzz, -all, -hour, -month_year,
         -sunPos_azimuth, -hourSunset, -hourSunrise, -sunset, -sunrise) %>% 
  mutate(stationSet = factor(stationSet),
         type = factor(type, levels = c("control", "OWF"), label = c("Control", "OWF")),
         year = as.factor(format(time_hour, format = "%y")),
         month = as.numeric(format(time_hour, format = "%m")),
         week_num = as.numeric(format(time_hour, format = "%W")),
         day = as.Date(time_hour, "%Y-%m-%d", tz = "UTC"),
         ToD = as.numeric(format(time_hour, format = "%H")),
         log10_SA = log10(SA),
         PODtype = "CPOD",
         PODtype = if_else(stationSet == "2024-BE_grafton", "FPOD", PODtype),
         PODtype = factor(PODtype, levels = c("CPOD", "FPOD")))

################################################################################-
# Combining the WBAT - CPOD plot ----

joined_WBAT_CPOD_sum <- joined_WBAT_CPOD %>%
  mutate(pph = factor(pph, levels = c("0", "1"), label = c("Absent", "Present"))) %>%
  group_by(stationSet, pairingName, type, pph) %>% 
  summarise(mean_logSA = mean(log10_SA, na.rm = T))

joined_WBAT_CPOD_sum <- joined_WBAT_CPOD_sum %>% 
  group_by(stationSet) %>%
  summarise(slope = (mean_logSA[pph == "Present"] - mean_logSA[pph == "Absent"]) / (as.numeric(factor(pph))[pph == "Present"] - as.numeric(factor(pph))[pph == "Absent"]),
            .groups = "drop") %>%
  left_join(joined_WBAT_CPOD_sum, join_by(stationSet)) %>% 
  mutate(line_color = ifelse(slope > 0, "lightblue", "coral"))


interaction_WBAT_CPOD <-
  ggplot(joined_WBAT_CPOD_sum, aes(x = pph, y = mean_logSA)) +
  geom_line(aes(group = stationSet, color = line_color), 
            linewidth = 1.25, lineend = "round", show.legend = F) +
  geom_point(aes(color = type 
                 # shape = type
  ), size = 6) +
  # scale_color_manual(values = all_colors) +
  # scale_shape_manual(values = c("OWF" = 16, "control" = 17)) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "Total HP hours", y = "Mean log<sub>10</sub>(SA)") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 30),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.title.y = element_markdown(size = 30),
    axis.text.y = element_text(size = 20, color = "black"),
    legend.position = "inside",
    legend.position.inside = c(.9, .5),
    legend.background = element_rect(fill = "white", size = .5),
    legend.margin = margin(3, 3, 3, 3),
    legend.key.size = unit(30, "pt")
  )

ggsave(filename = file.path(figurePath, "Interaction between PF and HP.png"), plot = interaction_WBAT_CPOD, width = 15, height = 10)



################################################################################-
# Some modeling

WC_1 <- gam(log10_SA ~ 
              type +
              pph,
             data = joined_WBAT_CPOD, method = "REML")

# Checking the model performance 
AIC(WC_1)

# looking at the summary of the model
summary(WC_1)

# First complexer model
WC_2 <- bam(log10_SA ~ 
               type +
               pph +
               PODtype +
               s(ToD, k = 10, bs = "cc") +
               s(week_num, k = 12) +
               s(month, k = 8) +
               s(lat, lon, k = 10, bs = "ds", m = c(1, 0.5)) +
               s(stationSet, bs = "re") +   
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, method = "fREML", 
             nthreads = c(4, 1), discrete = TRUE)


# Checking the model performance 
AIC(WC_1, WC_2)
summary(WC_2)
gam.check(WC_2)
plot(WC_2)








