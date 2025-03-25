################################################################################-
# Combining the to data sets and plotting them - 15/01/25 ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, mgcv, RColorBrewer, 
               mgcv, ggstar, gratia, visualize, infer, 
               ggtext, lattice, nlme, lmtest)

rm(list=ls())

sourceDir <- function(directory) {
  files <- list.files(directory, pattern = "\\.R$", full.names = TRUE)
  for (file in files) {
    source(file)
  }
}

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
  mutate(pairingName = factor(pairingName),
         dataSet = factor(dataSet),
         stationSet = factor(stationSet),
         stationName = factor(stationName),
         type = factor(type, levels = c("control", "OWF"), label = c("Control", "OWF")),
         year = as.numeric(format(time_hour, format = "%y")),
         month = as.numeric(format(time_hour, format = "%m")),
         week_num = as.numeric(format(time_hour, format = "%W")),
         day = as.Date(time_hour, "%Y-%m-%d", tz = "UTC"),
         DoY = as.numeric(format(time_hour, format = "%j")),
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

joined_WBAT_CPOD_sum <- joined_WBAT_CPOD_sum %>%
  mutate(pairingName = factor(pairingName, 
                              levels = c("2021-BE_1", "2021-BE_2", "2023-BSW_1", "2023-BSW_2", "2023-BE_1", "2023-BE_2", "2024-BE_1"), 
                              label = c("July-Aug/21 - Birk-/Belw-",
                                        "July-Aug/21 - Graf-/Cpow-",
                                        "May-June/23 - Bors 1/Bors 4",
                                        "May-June/23 - Bors 2/Bors 3",
                                        "July-Sept/23 - Gard-/Belw-",
                                        "July-Sept/23 - Graf-/Cpow-",
                                        "Oct-Dec/23 - Graf-/Cpow-")))

manual_shapes_joined <- c(1, 5, 13, 15, 23, 28, 22)[seq_len(7)]

interaction_WBAT_CPOD <-
  ggplot(joined_WBAT_CPOD_sum, aes(x = pph, y = mean_logSA)) +
  geom_line(aes(group = stationSet, color = line_color), 
            linewidth = 3, 
            lineend = "round", 
            show.legend = F) +
  geom_star(aes(color = type, 
                starshape = pairingName,
                fill = type), 
            # fill = NA,
            starstroke = 1.50,
            size = 8,
            show.legend = F) +
  # geom_point(aes(color = type,
                 # shape = pairingName
  # ), size = 6) +
  scale_fill_manual(values = c("#388fe0", "#ccac3d")) +
  scale_color_manual(values = c("black", "black", "grey", "black")) +
  scale_starshape_manual(values = manual_shapes_joined,
                         name = "Pairings - Period & Location") +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "Harbour porpoise state", y = "Area Backscattering Coefficient log<sub>10</sub>(s<sub>a</sub>)") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.title.y = element_markdown(size = 25),
    axis.text.y = element_text(size = 20, color = "black"),
    legend.position = "inside",
    legend.position.inside = c(.9, .5),
    legend.background = element_rect(fill = "white", size = .5),
    legend.margin = margin(3, 3, 3, 3),
    legend.key.size = unit(40, "pt")
  )

ggsave(filename = file.path(figurePath, "Interaction between PF and HP2_draft_nocolour.png"), plot = interaction_WBAT_CPOD, width = 14, height = 10)



################################################################################-
# Last changes to make it suitable for modeling:
joined_WBAT_CPOD <- joined_WBAT_CPOD %>% 
  mutate(pph = factor(pph, levels = c("0", "1"), label = c("Absent", "Present")))




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
               # PODtype +
               s(ToD, k = 10) +
               # s(DoY, k = 10) +
               s(week_num, k = 12) +
               # s(month, k = 8) +
               s(lat, lon, k = 10, bs = "ds", m = c(1, 0.5)) +
               s(stationSet, bs = "re") +   
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, method = "fREML", 
             nthreads = c(4, 1), discrete = TRUE)


WC_7 <- bam(log10_SA ~ 
              type +
              pph +
              s(week_num, k = 15) +
              s(sunPos_altitude, k = 10) +
              s(stationSet, bs = "re") +
              s(year, bs = "re") +
              s(dataSet, bs = "re"),
            data = joined_WBAT_CPOD, method = "fREML", 
            nthreads = c(4, 1), discrete = TRUE)

AIC(WC_1, WC_2, WC_3, WC_4, WC_5)
summary(WC_7)
gam.check(WC_7)
plot(WC_7)


str(joined_WBAT_CPOD)


# First complexer model
WC_3 <- bam(pph ~ 
              type +
              log10_SA +
              PODtype +
              s(ToD, k = 10, bs = "cc") +
              s(week_num, k = 12) +
              s(month, k = 8) +
              s(lat, lon, k = 10, bs = "ds", m = c(1, 0.5)) +
              s(stationSet, bs = "re") +   
              s(year, bs = "re"),
            data = joined_WBAT_CPOD, method = "fREML", 
            nthreads = c(4, 1), discrete = TRUE,
            family = binomial(link = "logit"))

# After sending the email work
WC_4 <- bam(log10_SA ~ 
              type +
              s(week_num, by = type, k = 12) +
              pph +         
              s(week_num, by = pph, k = 12) +
              s(week_num, stationSet, bs = "fs", k = 12) +
              s(lat, lon, k = 10, bs = "ds", m = c(1, 0.5)) +
              s(year, bs = "re"),
            data = joined_WBAT_CPOD, 
            method = "fREML", 
            nthreads = c(4, 1), 
            discrete = TRUE)

WC_5 <- bam(log10_SA ~ 
              type +
              s(sunPos_altitude) 
              ,
            data = joined_WBAT_CPOD, 
            method = "fREML", 
            nthreads = c(4, 1), 
            discrete = TRUE)
              
              
              type +
              s(week_num, by = type, k = 12) +
              pph +         
              s(week_num, by = pph, k = 12) +
              s(week_num, stationSet, bs = "fs", k = 12) +
              s(lat, lon, k = 10, bs = "ds", m = c(1, 0.5)) +
              s(year, bs = "re")



k.check(WC_4)


# 
# WC_5 <- bam(log10_SA ~ 
#               s(week_num, type, bs = "fs", k = 12) +
#               s(stationName, bs = "re") +
#               s(year, bs = "re"),
#             data = joined_WBAT_CPOD, 
#             method = "fREML", 
#             nthreads = c(4, 1), 
#             discrete = TRUE)

# Checking the model performance 
AIC(WC_1, WC_2, WC_3, WC_4, WC_5)
summary(WC_4)
gam.check(WC_4)
plot(WC_2)





# Looking into the autocorrolation of the variables
residuals_gam <- residuals(WC_2, type = "response")
acf(residuals_gam, main = "ACF of GAM Residuals")

acf(residuals_gam ~ joined_WBAT_CPOD$time_hour, main = "Temporal ACF of Residuals")

pacf(residuals_gam, main = "PACF of GAM Residuals")


time_series <- ts(residuals_gam, frequency = 1)  # Adjust frequency as needed
acf(time_series, main = "Temporal ACF of Residuals")

library(lmtest)
dwtest(WC_2)


# First try into seeing if my data is even normaly distributed
# ks.test(joined_WBAT_CPOD$log10_SA, "pnorm", mean = mean(joined_WBAT_CPOD$log10_SA), sd = sd(joined_WBAT_CPOD$log10_SA))
# 
# joined_WBAT_CPOD %>%
#   ggplot(aes(x = log10_SA)) +
#   geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7) +
#   facet_wrap(~type) +
#   theme_minimal()


################################################################################-
# Working on the script after Jeroen his script idea to correct for auto correlation 


joined_WBAT_CPOD$time_numeric <- as.numeric(difftime(
  joined_WBAT_CPOD$time_hour, 
  min(joined_WBAT_CPOD$time_hour), 
  units = "hours"))


WC_10 <- gamm(log10_SA ~
               type +
               s(time_numeric) + 
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "REML", 
             discrete = TRUE)

WC_11 <- gamm(log10_SA ~
               type +
               s(time_numeric, by = stationSet) +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "REML", 
             discrete = TRUE)

WC_12 <- gamm(log10_SA ~
                type +
                s(year, bs = "re"),
              correlation = corAR1(form = ~ time_numeric | stationSet),
              data = joined_WBAT_CPOD, 
              method = "REML", 
              discrete = TRUE)

summary(WC_10$gam)
summary(WC_10$lme)
summary(WC_11)
summary(WC_12)

# Residual diagnostics
acf(residuals(WC_10$lme, type = "normalized"), main = "ACF - Model 0")
acf(residuals(WC_11$lme, type = "normalized"), main = "ACF - Model 1")
acf(residuals(WC_12$lme, type = "normalized"), main = "ACF - Model 2")

pacf(residuals(WC_10$lme, type = "normalized"), main = "PACF - Model 0")
pacf(residuals(WC_11$lme, type = "normalized"), main = "PACF - Model 1")
pacf(residuals(WC_12$lme, type = "normalized"), main = "PACF - Model 2")

# Durbin-Watson Test
dwtest(residuals(WC_10$lme, type = "normalized") ~ 1)
dwtest(residuals(WC_11$lme, type = "normalized") ~ 1)
dwtest(residuals(WC_12$lme, type = "normalized") ~ 1)

# Residual plots
plot(data$time_numeric, residuals(WC_10$lme, type = "normalized"), type = "l", main = "Residuals - Model 0", ylab = "Residuals")
plot(data$time_numeric, residuals(WC_11$lme, type = "normalized"), type = "l", main = "Residuals - Model 1", ylab = "Residuals")
plot(data$time_numeric, residuals(WC_12$lme, type = "normalized"), type = "l", main = "Residuals - Model 2", ylab = "Residuals")

# Compare models using AIC
AIC(WC_10$lme, WC_11$lme, WC_12$lme)

# Compare models using ANOVA (nested models only)
anova(WC_10$lme, WC_11$lme, WC_12$lme)




WC_13 <- bam(log10_SA ~
                type +
                s(time_numeric, stationSet, bs = "fs", k = 10) +
                s(year, bs = "re"),
              data = joined_WBAT_CPOD, 
              method = "fREML", 
              nthreads = c(4, 1),
              discrete = TRUE)

WC_14 <- bam(log10_SA ~
               type +
               stationSet +
               s(time_numeric, by = stationSet, k = 10) +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_15 <- bam(log10_SA ~
               type +
               pph +
               s(DoY, by = type, k = 30) +
               s(DoY, by = pph, k = 25) +
               s(week_num, k = 25) +
               s(sunPos_altitude, k = 10) +
               s(DoY, stationSet, bs = "fs", k = 25) +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_16 <- bam(log10_SA ~
               type +
               pph +
               stationSet +
               s(week_num, k = 15) +
               s(sunPos_altitude, k = 10) +
               s(DoY, by = stationSet, k = 25) +
               s(year, bs = "re") +
               s(dataSet, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_17 <- bam(log10_SA ~ 
               type +
               pph +
               s(week_num, by = type, k = 10) +
               s(sunPos_altitude, k = 10) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_18 <- bam(log10_SA ~
               type +
               pph +
               s(week_num, k = 15) +
               s(sunPos_altitude, k = 10) +
               s(DoY, by = dataSet, k = 10) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_19 <- bam(log10_SA ~
               type +
               pph +
               s(week_num, k = 10) +
               s(sunPos_altitude, k = 10) +
               s(DoY, by = pairingName, k = 14) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_20 <- bam(log10_SA ~ # is working better then WC_21
               type +
               pph +
               s(week_num, k = 12) +
               s(sunPos_altitude, k = 15) +
               s(DoY, pairingName, bs = "fs", k = 30) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_20.2 <- bam(SA ~ 
               type +
               pph +
               s(week_num, k = 12) +
               s(sunPos_altitude, k = 15) +
               s(DoY, pairingName, bs = "fs", k = 30) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE,
             family = gaussian(link = "log"))

WC_21 <- bam(log10_SA ~
               type +
               pph +
               pairingName +
               s(week_num, k = 12) +
               s(sunPos_altitude, k = 15) +
               s(DoY, by = pairingName, k = 25) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

gam.check(WC_15)
gam.check(WC_17)
gam.check(WC_18)
gam.check(WC_19)
gam.check(WC_20.2)
gam.check(WC_21)

plot(WC_17)
plot(WC_18)
plot(WC_19)
plot(WC_20.2)
plot(WC_21)

summary(WC_13)
summary(WC_14)
summary(WC_15)
summary(WC_16)
summary(WC_17)
summary(WC_18)
summary(WC_19)
summary(WC_20.2)
summary(WC_21)

# Residual diagnostics
acf(residuals(WC_17, type = "response"), main = "ACF of GAM Residuals")
acf(residuals(WC_18, type = "response"), main = "ACF of GAM Residuals")
acf(residuals(WC_19, type = "response"), main = "ACF of GAM Residuals")
acf(residuals(WC_20, type = "response"), main = "ACF of GAM Residuals")
acf(residuals(WC_21, type = "response"), main = "ACF of GAM Residuals")

pacf(residuals(WC_17, type = "response"), main = "PACF - Model 0")
pacf(residuals(WC_18, type = "response"), main = "PACF - Model 1")
pacf(residuals(WC_19, type = "response"), main = "PACF - Model 0")
pacf(residuals(WC_16, type = "response"), main = "PACF - Model 0")
pacf(residuals(WC_20, type = "response"), main = "PACF - Model 0")

# Durbin-Watson Test
dwtest(residuals(WC_17, type = "response") ~ 1)
dwtest(residuals(WC_18, type = "response") ~ 1)
dwtest(residuals(WC_19, type = "response") ~ 1)
dwtest(residuals(WC_16, type = "response") ~ 1)
dwtest(residuals(WC_20, type = "response") ~ 1)

# Compare models using AIC
AIC(WC_16, WC_17, WC_18, WC_19, WC_20, WC_21)

# Compare models using ANOVA (nested models only)
anova(WC_16, WC_17, WC_18, WC_19, WC_20)



# Work on sunday

WC_30 <- bam(log10_SA ~ # is working better then WC_21
               type +
               pph +
               s(week_num, k = 12) +
               s(sunPos_altitude, k = 15) +
               s(DoY, pairingName, bs = "fs", k = 30) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_31 <- bam(log10_SA ~ 
                 type +
                 pph +
                 s(week_num, k = 12) +
                 s(sunPos_altitude, k = 15) +
                 s(DoY, pairingName, bs = "fs", k = 15) +
                 s(stationSet, bs = "re") +
                 s(year, bs = "re"),
               data = joined_WBAT_CPOD, 
               method = "fREML", 
               nthreads = c(4, 1),
               discrete = TRUE)

WC_32 <- bam(log10_SA ~ 
               type +
               pph +
               stationSet +
               s(week_num, k = 14) +
               s(sunPos_altitude, k = 12) +
               s(DoY, pairingName, bs = "fs", k = 15) +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_33.2 <- bam(log10_SA ~ 
               type +
               pph +
               s(week_num, k = 18) +
               s(sunPos_altitude, k = 12) +
               # s(DoY, pairingName, bs = "fs", k = 15) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_33 <- bam(log10_SA ~ 
               type +
               pph +
               s(week_num, k = 14) +
               s(sunPos_altitude, k = 12) +
               # s(DoY, pairingName, bs = "fs", k = 15) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

rho <- acf(residuals(WC_33), lag.max = 1, plot = FALSE)$acf[2]

WC_34 <- bam(log10_SA ~ 
               type +
               pph +
               s(week_num, k = 14) +
               s(sunPos_altitude, k = 12) +
               s(DoY, pairingName, bs = "fs", k = 15) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE,
             acf = rho)

WC_35 <- bam(log10_SA ~ 
               type +
               pph +
               s(week_num, k = 14) +
               s(sunPos_altitude, k = 12) +
               s(DoY, pairingName, bs = "fs", k = 20) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

sapply(joined_WBAT_CPOD, function(x) length(unique(x)))
unique(joined_WBAT_CPOD$week_num)

gam.check(WC_30)
gam.check(WC_31)
gam.check(WC_32)
gam.check(WC_33)
gam.check(WC_33.2, rep = 500)
gam.check(WC_35)

appraise(WC_33.2, method = "simulate")

plot(WC_30)
plot(WC_31)
plot(WC_32)
plot(WC_33)
plot(WC_33.2, rep = 500)
plot(WC_35)

summary(WC_30)
summary(WC_31)
summary(WC_32)
summary(WC_33)
summary(WC_33.2)
summary(WC_34)
summary(WC_35)

# Residual diagnostics
acf(residuals(WC_30, type = "response"), main = "ACF of GAM Residuals")
acf(residuals(WC_31, type = "response"), main = "ACF of GAM Residuals")
acf(residuals(WC_32, type = "response"), main = "ACF of GAM Residuals")
acf(residuals(WC_33.2, type = "response"), main = "ACF of GAM Residuals")
acf(residuals(WC_35, type = "response"), main = "ACF of GAM Residuals")

pacf(residuals(WC_30, type = "response"), main = "PACF - Model 0")
pacf(residuals(WC_31, type = "response"), main = "PACF - Model 1")
pacf(residuals(WC_32, type = "response"), main = "PACF - Model 0")
pacf(residuals(WC_33, type = "response"), main = "PACF - Model 0")
pacf(residuals(WC_35, type = "response"), main = "PACF - Model 0")

# Durbin-Watson Test
dwtest(residuals(WC_30, type = "response") ~ 1)
dwtest(residuals(WC_31, type = "response") ~ 1)
dwtest(residuals(WC_32, type = "response") ~ 1)
dwtest(residuals(WC_33, type = "response") ~ 1)
dwtest(residuals(WC_35, type = "response") ~ 1)

# Compare models using AIC
AIC(WC_30, WC_31, WC_32, WC_33, WC_35)
AIC(WC_30, WC_33)
# Compare models using ANOVA (nested models only)
anova(WC_30, WC_31, WC_32, WC_33, WC_35)






my_knots <- list(week_num = c(18, 24, 27, 35, 40, 48, 50))

WC_40 <- bam(log10_SA ~ 
                 type +
                 pph +
                 s(week_num, k = 18) +
                 s(sunPos_altitude, k = 12) +
                 # s(DoY, pairingName, bs = "fs", k = 15) +
                 s(stationSet, bs = "re") +
                 s(year, bs = "re"),
               data = joined_WBAT_CPOD, 
               method = "fREML", 
               nthreads = c(4, 1),
               discrete = TRUE,
               knots = my_knots)

gam.check(WC_40, pages = 1)

appraise(WC_40, method = "simulate")

plot(WC_40)
visualize(WC_40)

summary(WC_40)

hist(joined_WBAT_CPOD$log10_SA)
hist(joined_WBAT_CPOD$SA)



###############################################################################-
# After starting with writing 
# What will happen if I take pph or pos_minutes as the responsive variable and plot log10(SA) against it


WC_50 <- bam(pph ~ 
               type +
               s(log10_SA, k = 10) +
               s(week_num, k = 14) +
               s(sunPos_altitude, k = 12) +
               # s(DoY, pairingName, bs = "fs", k = 15) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE,
             family = binomial(logit))

summary(WC_50)
gam.check(WC_50)
plot(WC_50)

WC_33 <- bam(log10_SA ~ 
               type +
               pph +
               s(week_num, k = 14) +
               s(sunPos_altitude, k = 12) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_51 <- bam(log10_SA ~ 
               type +
               pph +
               pairingName +
               s(week_num, by = pairingName, k = 14) +
               s(sunPos_altitude, k = 12) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_52 <- bam(log10_SA ~ 
               type +
               pph +
               s(week_num, pairingName, bs = "fs", k = 14) +
               s(sunPos_altitude, k = 12) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

summary(WC_33)
summary(WC_51)
summary(WC_52)

gam.check(WC_33)
gam.check(WC_51)
gam.check(WC_52)

plot(WC_33)
plot(WC_51)
plot(WC_52)

# Residual diagnostics
acf(residuals(WC_33, type = "response"), main = "ACF of GAM Residuals")
acf(residuals(WC_51, type = "response"), main = "ACF of GAM Residuals")
acf(residuals(WC_52, type = "response"), main = "ACF of GAM Residuals")

pacf(residuals(WC_33, type = "response"), main = "PACF - Model 0")
pacf(residuals(WC_51, type = "response"), main = "PACF - Model 1")
pacf(residuals(WC_52, type = "response"), main = "PACF - Model 1")

# Durbin-Watson Test
dwtest(residuals(WC_33, type = "response") ~ 1)
dwtest(residuals(WC_51, type = "response") ~ 1)
dwtest(residuals(WC_52, type = "response") ~ 1)

# Compare models using AIC
AIC(WC_33, WC_51, WC_52)

# Compare models using ANOVA (nested models only)
anova(WC_33, WC_51, WC_52)




WC_60 <- bam(log10_SA ~ 
               type +
               pph +
               s(sunPos_altitude, k = 10),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_60 <- bam(log10_SA ~ 
               type +
               pph +
               s(sunPos_altitude, k = 10) +
               s(dist_to_struc, type, bs = "fs"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_60 <- bam(log10_SA ~ 
               type +
               s(week_num, k = 14) +
               s(sunPos_altitude, k = 10) +
               s(pairingName, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_61 <- bam(log10_SA ~ 
               s(week_num, k = 14) +
               s(sunPos_altitude, k = 10) +
               s(dist_to_struc, type, bs = "fs"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_61 <- bam(log10_SA ~ 
               type +
               s(sunPos_altitude, k = 12) +
               s(week_num, k = 14) +
               ti(sunPos_altitude, week_num) +
               s(dist_to_struc, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_61 <- bam(log10_SA ~ 
               type +
               s(sunPos_altitude, k = 12) +
               s(week_num, k = 15) +
               s(stationSet, bs = "re") +
               s(dist_to_struc, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)


summary(WC_60)
summary(WC_61)

gam.check(WC_60)
gam.check(WC_61)

plot(WC_60)
plot(WC_61)

AIC(WC_60, WC_61, WC_33)






CPOD_71 <- bam(pph ~ 
               type +
               s(log10_SA, by = type) +
               s(sunPos_altitude, by = type) +
               s(week_num, by = type) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE,
             family = binomial())


summary(CPOD_71)
summary(CPOD_72)

gam.check(CPOD_71)
gam.check(CPOD_72)

plot(CPOD_71)
plot(CPOD_72)

AIC(CPOD_71, CPOD_72, CPOD_71)



CPOD_72 <- bam(pph ~ 
                 type + 
                 s(ToD, by = type) +
                 s(dist_to_struc, by = type),
               data = joined_WBAT_CPOD, 
               method = "fREML", 
               nthreads = c(4, 1),
               discrete = TRUE,
               family = binomial())







joined_WBAT_CPOD$dist_to_struc_km <- joined_WBAT_CPOD$dist_to_struc / 1000

library(lme4)       # For GLMM with lmer and glmer
library(glmmTMB) 

glmm_model <- glmer(pph ~ dist_to_struc_km + type + (1 | stationSet) + (1 | year), 
                    data = joined_WBAT_CPOD, 
                    family = binomial(link = "logit")) 
summary(glmm_model)

car::vif(glmm_model)

ggplot(joined_WBAT_CPOD, aes(x = dist_to_struc, y = pph)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +
  labs(title = "Effect of Distance from Offshore Structures on PPH",
       x = "Distance to Offshore Structure (m)",
       y = "Predicted Probability of PPH") +
  theme_minimal()



joined_WBAT_CPOD_test <- joined_WBAT_CPOD %>%
  mutate(pph = as.numeric(as.character(pph))) %>%
  group_by(stationSet, day, dist_to_struc, type, year) %>%
  summarise(
    total_pph = sum(pph, na.rm = TRUE),  # Sum detections per station and day
    DPH = (total_pph / 24) * 100  # Convert to proportion
  )

hist(joined_WBAT_CPOD_test$DPH_prop)

joined_WBAT_CPOD_test$DPH_prop <- joined_WBAT_CPOD_test$total_pph / 24  # Convert to proportion (0-1)
joined_WBAT_CPOD_test <- joined_WBAT_CPOD_test %>%
  mutate(DPH_prop = (total_pph + 0.01) / (24 + 0.02))

summary(joined_WBAT_CPOD_test$DPH_prop)
hist(joined_WBAT_CPOD_test$DPH_prop, main="DPH Proportion Distribution", xlab="DPH Proportion")
joined_WBAT_CPOD_test <- joined_WBAT_CPOD_test %>%
  mutate(DPH_prop = pmin(pmax((total_pph + 0.01) / (24 + 0.02), 0.001), 0.999))  # Clamp values between 0.001 and 0.999


glm_beta <- glmmTMB(DPH_prop ~ dist_to_struc + type + (1 | stationSet) + (1 | year), 
                    data = joined_WBAT_CPOD_test, 
                    family = beta_family(),
                    control = glmmTMBControl(optimizer = "bobyqa"))
  

sum(is.na(joined_WBAT_CPOD_test$dist_to_struc))  # Check missing values in dist_to_struc
sum(is.na(joined_WBAT_CPOD_test$type))          # Check missing values in type
sum(is.na(joined_WBAT_CPOD_test$stationSet))    # Check missing values in stationSet
sum(is.na(joined_WBAT_CPOD_test$year)) 

glm_beta_simple <- glmmTMB(DPH_prop ~ dist_to_struc + type, 
                           data = joined_WBAT_CPOD_test, 
                           family = beta_family())

summary(glm_beta_simple)

glm_beta_full <- glmmTMB(DPH_prop ~ type,
                         data = joined_WBAT_CPOD_test, 
                         family = beta_family())

summary(glm_beta_full)





dist_range <- data.frame(dist_to_struc = seq(min(joined_WBAT_CPOD_test$dist_to_struc), 
                                             max(joined_WBAT_CPOD_test$dist_to_struc), 
                                             length.out = 100),
                         type = "OWF")  # Assume type is "OWF", or choose another value

# Get predicted DPH_prop for each value of 'dist_to_struc'
dist_range$predicted_DPH_prop <- predict(glm_beta_simple, newdata = dist_range, type = "response")

# Plot
ggplot(dist_range, aes(x = dist_to_struc, y = predicted_DPH_prop)) +
  geom_line() +
  labs(title = "Effect of Distance to Structure on DPH Proportion",
       x = "Distance to Structure", y = "Predicted DPH Proportion") +
  theme_minimal()



type_effect <- data.frame(dist_to_struc = mean(joined_WBAT_CPOD_test$dist_to_struc), 
                          type = factor(c("OWF", "Control"), levels = levels(joined_WBAT_CPOD_test$type)))

# Get predicted values for each level of 'type'
type_effect$predicted_DPH_prop <- predict(glm_beta_simple, newdata = type_effect, type = "response")

# Plot
ggplot(type_effect, aes(x = type, y = predicted_DPH_prop, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Effect of Type on DPH Proportion",
       x = "Type", y = "Predicted DPH Proportion") +
  theme_minimal()






prediction_data <- data.frame(
  dist_to_struc = rep(seq(min(joined_WBAT_CPOD_test$dist_to_struc), 
                          max(joined_WBAT_CPOD_test$dist_to_struc), 
                          length.out = 100)),  # Adjust the length to get a smooth line
                      type = rep(c("Control", "OWF"), each = 100))
  
  # Make sure 'type' is a factor and has the same levels as in the original data
  prediction_data$type <- factor(prediction_data$type, levels = levels(joined_WBAT_CPOD_test$type))
  
  # Get predicted values for DPH_prop
  prediction_data$predicted_DPH_prop <- predict(glm_beta_simple, newdata = prediction_data, type = "response")
  
  # Plot the results
  library(ggplot2)
  
  ggplot(prediction_data, aes(x = dist_to_struc, y = predicted_DPH_prop, color = type)) +
    geom_line(size = 1) +  # Line plot for predicted DPH_prop
    labs(title = "Effect of Distance from Structure on DPH Prop",
         x = "Distance to Structure",
         y = "Predicted DPH Proportion",
         color = "Type") +
    theme_minimal()

  
  

  
  
  
################################################################################- 
# modeling the presence to sun angle and distance to structure 
  
CPOD_71 <- bam(pph ~ 
               type +
               s(sunPos_altitude, k = 12) +
               s(week_num, k = 15),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE,
             family = binomial())

CPOD_72 <- bam(pph ~ 
                 type +
                 s(sunPos_altitude, k = 12) +
                 s(week_num, k = 15) +
                 s(stationSet, bs = "re") +
                 s(year, bs = "re"),
               data = joined_WBAT_CPOD, 
               method = "fREML", 
               nthreads = c(4, 1),
               discrete = TRUE,
               family = binomial())

CPOD_74 <- bam(pph ~ 
                 type +
                 s(log10_SA, k = 10) +
                 s(sunPos_altitude, k = 14) +
                 s(week_num, k = 14) +
                 s(DoY, pairingName, bs = "fs", k = 40),
               data = joined_WBAT_CPOD, 
               method = "fREML", 
               nthreads = c(4, 1),
               discrete = TRUE,
               family = binomial())

###############################################################################
# The model I will be using for the CPOD value ----
CPOD_73_final <- bam(pph ~ 
                 type +
                 s(log10_SA, k = 10) +
                 s(sunPos_altitude, k = 12) +
                 s(week_num, k = 12) +
                 s(pairingName, bs = "re"),
               data = joined_WBAT_CPOD, 
               method = "fREML", 
               nthreads = c(4, 1),
               discrete = TRUE,
               family = binomial())

# for temporal night/day pattern
CPOD_74_final <- bam(pph ~ 
                 type +
                 s(log10_SA, k = 10) +
                 s(sunPos_altitude, by = type, k = 12) +
                 s(week_num, k = 12) +
                 s(pairingName, bs = "re"),
               data = joined_WBAT_CPOD, 
               method = "fREML", 
               nthreads = c(4, 1),
               discrete = TRUE,
               family = binomial())

# for temporal week_num pattern
CPOD_75_final <- bam(pph ~ 
                 type +
                 s(log10_SA, k = 10) +
                 s(sunPos_altitude, k = 12) +
                 s(week_num, by = type, k = 12) +
                 s(pairingName, bs = "re"),
               data = joined_WBAT_CPOD, 
               method = "fREML", 
               nthreads = c(4, 1),
               discrete = TRUE,
               family = binomial())

summary(CPOD_73_final)
summary(CPOD_74_final)
summary(CPOD_75_final)

gam.check(CPOD_73_final)
gam.check(CPOD_74_final)
gam.check(CPOD_75_final)

plot(CPOD_73_final, pages = 1)
plot(CPOD_74_final, pages = 1)
plot(CPOD_75_final, pages = 1)

AIC(CPOD_73_final, CPOD_74_final, CPOD_75_final)





WC_61 <- bam(log10_SA ~ 
               type +
               s(sunPos_altitude, k = 12) +
               s(week_num, k = 15),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_62 <- bam(log10_SA ~ 
               type +
               s(sunPos_altitude, k = 12) +
               s(week_num, k = 15) +
               s(stationSet, bs = "re") +
               s(year, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

###############################################################################
# The model I will be using for the log10(SA) value ----
WC_63_final <- bam(log10_SA ~ 
               type +
               pph +
               s(sunPos_altitude, k = 12) +
               s(week_num, k = 12) +
               s(pairingName, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

# for temporal effect of type in day/night 
WC_64_final <- bam(log10_SA ~ 
               type +
               pph +
               s(sunPos_altitude, by = type, k = 12) +
               s(week_num, k = 12) +
               s(pairingName, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

# for temporal effect of type in week_num
WC_65_final <- bam(log10_SA ~ 
               type +
               pph +
               s(sunPos_altitude, k = 12) +
               s(week_num, by = type, k = 12) +
               s(pairingName, bs = "re"),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)


summary(WC_63_final)
summary(WC_64_final)
summary(WC_65_final)

gam.check(WC_63_final)
gam.check(WC_64_final)
gam.check(WC_65_final)

plot(WC_63_final, pages = 1)
plot(WC_64_final, pages = 1)
plot(WC_65_final, pages = 1)

AIC(WC_63_final, WC_64_final, WC_65_final)





a# Nadat ik de mail naar jeroen heb gevonden even wat dingen testen
WC_62 <- bam(log10_SA ~ 
               type +
               pph +
               pairingName +
               s(sunPos_altitude, by = pairingName, k = 12) +
               s(week_num, by = pairingName, k = 12),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)

WC_64 <- bam(log10_SA ~ 
               type +
               pph +
               s(sunPos_altitude, k = 12) +
               s(week_num, pairingName, bs = "fs", k = 12),
             data = joined_WBAT_CPOD, 
             method = "fREML", 
             nthreads = c(4, 1),
             discrete = TRUE)





summary(WC_61)
summary(WC_62)
summary(WC_63)
summary(WC_64)

gam.check(WC_61)
gam.check(WC_62)
gam.check(WC_63)
gam.check(WC_64, pages = 1)

plot(WC_61)
plot(WC_62, pages = 1)
plot(WC_63, pages = 1)
plot(WC_64, pages = 1)

AIC(WC_61, WC_62, WC_63, WC_64)


acf(residuals(WC_63, type = "response"), main = "ACF of GAM Residuals")

pacf(residuals(WC_63, type = "response"), main = "PACF - Model 0")

# Durbin-Watson Test
dwtest(residuals(WC_63, type = "response") ~ 1)






library(glmmTMB)
library(performance)
library(ggplot2)

model <- glmmTMB(log10_SA ~ type + pph + sunPos_altitude + week_num + 
                   (1 + type | stationSet) + (1 | year), 
                 data = joined_WBAT_CPOD, 
                 family = gaussian(link = "identity"))

# Model summary
summary(model)
check_model(model)




