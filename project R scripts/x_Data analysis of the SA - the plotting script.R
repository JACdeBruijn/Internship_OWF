################################################################################
# Script for plotting - 08/12/24
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, RColorBrewer, icesTAF, see)

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

load(file = file.path(resultPath,'CPOD_WBAT_workspace.RData'))

################################################################################
# adjust CPOD data frames (should be done at data generation...)
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
  filter(pairing != -1)

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
  filter(pairing != -1)

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
  filter(pairing != -1)

# WBAT.all.summary %>%                                                            # Checking the depthIntegration per stationSet to see if the imputation of depthMaxR makes sense
#   group_by(stationSet, pairingName) %>%
#   summarize(mean_depth = mean(depthIntegration), .groups = "drop")

WBAT.all.summary <- WBAT.all.summary %>% 
  mutate(type = factor(type, levels = c("OWF", "control")))

# WBAT.all.summary <- WBAT.all.summary %>%                                        # Adding depthMaxR
#   group_by(frequency, station, phase, dataSet) %>% 
#   mutate(depthMaxR = depthIntegration / max(depthIntegration)) 

################################################################################
# Working on WBAT_overview showing SA total
################################################################################

# Sub-setting on threshold -50
WBAT_overview <- WBAT.all.summary %>%
  filter(pairing != -1, 
         n >= 8, 
         # depthMaxR > 0.00, 
         treshold == -50) %>% 
  select(-freq, -lat, -lon, -soundtrap, -CPOD, -WBAT_70, -WBAT_200)

hist(WBAT.all.summary$n)

str(WBAT_overview)

# Overview - VIOLIN
taf.png(file.path(figurePath, paste0("WBAT overview Mean log10(SA) grouped on type (75 cut out) - 70khz.png")))
print(ggplot(subset(WBAT_overview, frequency == 70), aes(x = type, y = log10(SA), fill = type))) +
  geom_violin(position = "dodge", width = 0.5, outlier.alpha = 0.1) + 
  stat_summary(aes(x = type, label = round(..y.., 2)),
               fun = mean, geom = "text", color = "black", size = 4, 
               hjust = 0.5, vjust = 0) +
  coord_cartesian(ylim = c(0, 4)) +
  labs(x = "Type", y = "mean log10(SA)") +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
  ggtitle('WBAT overview - grouped on Type - 70 kHz')
dev.off()

# Overview - BOXPLOT
taf.png(file.path(figurePath, paste0("WBAT overview Mean log10(SA) grouped on type - 70khz.png")))
print(ggplot(subset(WBAT_overview, frequency == 70), aes(x = type, y = log10(SA), fill = type))) +
  geom_boxplot(position = "dodge", width = 0.5, outlier.alpha = 0.1) + 
  stat_summary(aes(x = type, label = round(..y.., 2)),
               fun = mean, geom = "text", color = "black", size = 4, 
               hjust = 0.5, vjust = 0) +
  coord_cartesian(ylim = c(0, 4)) +
  labs(x = "Type", y = "mean log10(SA)") +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
  ggtitle('WBAT overview - grouped on Type - 70 kHz')
dev.off()

# Overview paired
taf.png(file.path(figurePath, paste0("WBAT overview - grouped on Type with pairwise comparison - 70khz.png")))
ggplot(subset(WBAT_overview, frequency == 70 & pairingName != "2021-BE_2"), 
             aes(x = type, y = log10(SA), group = pairingName, color = factor(pairingName))) +
  geom_point(stat = "summary", fun = mean, size = 3, aes(color = NULL)) +  
  geom_path(aes(group = pairingName), linetype = "dashed", size = 0.5, color = "red") +  
  coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "Type", y = "Mean log10(SA)") +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle('WBAT overview - grouped on Type with pairwise comparison - 70 kHz')
dev.off()

WBAT_overview_sum <- WBAT_overview %>%
  group_by(stationSet, pairingName, type, pairing, frequency) %>% 
  summarise(mean_SA = mean(log10(SA), na.rm = T)) 

ggplot(subset(WBAT_overview_sum, frequency == 70 & pairingName != "2021-BE_2"), 
       aes(x = type, y = mean_SA)) +
  geom_point(aes(shape = type, color = pairingName), size = 4) +
  scale_shape_manual(values = c(16, 17)) +
  geom_line(aes(group = pairingName), linetype = 'solid', linewidth = .75, lineend = "round") +
  labs(x = "Location type", y = "Mean log10(SA)") +
  theme_minimal()
  


################################################################################ Testing

# Summarizing mean_SA, mutate slope & give colours
WBAT_overview_sum <- WBAT_overview %>%
  group_by(stationSet, pairingName, type, pairing, frequency) %>% 
  summarise(mean_SA = mean(log10(SA), na.rm = T),
            SAsd = sd(log10(SA), na.rm = T),
            sem_SA = sd(log10(SA), na.rm = TRUE) / sqrt(n())) 

WBAT_overview_sum <- WBAT_overview_sum %>% 
  group_by(pairingName, frequency) %>%
  summarise(slope = (mean_SA[type == "control"] - mean_SA[type == "OWF"]) / (as.numeric(factor(type))[type == "control"] - as.numeric(factor(type))[type == "OWF"]),
            .groups = "drop") %>%
  left_join(WBAT_overview_sum, join_by(pairingName, frequency)) %>% 
  mutate(line_color = ifelse(slope > 0, "blue", "red"))

pairing_colors <- setNames(RColorBrewer::brewer.pal(n = length(unique(WBAT_overview_sum$pairingName)), "Set1"),
                           unique(WBAT_overview_sum$pairingName))
line_colors <- c("red" = "red", "blue" = "blue")
all_colors <- c(pairing_colors, line_colors)

# Plotting only the slope ######################################################
ggplot(subset(WBAT_overview_sum, frequency == 70 & pairingName != "2021-BE_2"), 
       aes(x = type, y = mean_SA)) +
  geom_line(aes(group = pairingName, color = line_color), linewidth = 0.75, linetype = "solid")

# plotting mean and slope ######################################## - This is working!!!!!
ggplot(subset(WBAT_overview_sum, frequency == 70 & pairingName != "2021-BE_2"), 
       aes(x = type, y = mean_SA)) +
  geom_violinhalf(data = subset(WBAT_overview, frequency == 70), flip = c(1, 3),
                  aes(x = type, y = log10(SA), fill = type)) +
  geom_line(aes(group = pairingName, color = line_color), 
            linewidth = 1, lineend = "round") +
  geom_point(aes(color = pairingName), size = 5) +
  scale_color_manual(values = all_colors) +
  coord_cartesian(ylim = c(0, 3)) +
  labs(x = "Location type", y = "Mean log10(SA)") +
  theme_minimal()

# Plotting the mean and slope with violin distance ####################################### - This is working!!!!!
p <- ggplot(subset(WBAT_overview_sum, frequency == 70), aes(x = type, y = mean_SA)) +
  geom_line(aes(group = pairingName, color = line_color), 
            linewidth = 1, lineend = "round") +
  geom_point(aes(color = pairingName), size = 5) +
  scale_color_manual(values = all_colors) +
  coord_cartesian(ylim = c(0, 3)) +
  labs(x = "Type", y = "Mean log10(SA)", title = "Nudged Points by Type") +
  theme_minimal()

data_list <- split(subset(WBAT_overview, frequency == 70), subset(WBAT_overview, frequency == 70)$type)

for (type in names(data_list)) {
  if (type == "control") {
    # For "control", plot on one side
    p <- p +
      geom_violinhalf(
        data = data_list[[type]],
        mapping = aes(x = type, y = log10(SA), fill = type), flip = F,
        position = position_nudge(x = 0.1))
    
  } else if (type == "OWF") {
    p <- p +
      geom_violinhalf(
        data = data_list[[type]],
        mapping = aes(x = type, y = log10(SA), fill = type), flip = T,
        position = position_nudge(x = -0.1))
  }
}

print(p)



################################################################################ Testing


test <- ggplot(subset(WBAT_overview_sum, frequency == 70), aes(x = type, y = mean_SA)) +
  geom_line(aes(group = pairingName, color = line_color), 
            linewidth = 1.25, lineend = "round") +
  geom_point(aes(color = pairingName), size = 6
             # position = position_jitter(width = 0.04, height = 0)
             ) +
  # geom_errorbar(aes(ymin = mean_SA - sem_SA, ymax = mean_SA + sem_SA, color = 'black'), width = 0.05,
  #               position = position_jitter(width = 0.04, height = 0)
  #               ) +
  scale_color_manual(values = all_colors) +
  coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "Type", y = "Mean log10(SA)") +
  theme_minimal()

data_list <- split(subset(WBAT_overview, frequency == 70), subset(WBAT_overview, frequency == 70)$type)

for (type in names(data_list)) {
  if (type == "control") {
    # For "control", plot on one side
    test <- test +
      geom_violinhalf(
        data = data_list[[type]],
        mapping = aes(x = type, y = log10(SA), fill = type), flip = F,
        position = position_nudge(x = 0.1)) +
      geom_boxplot(
        data = data_list[[type]],
        mapping = aes(x = type, y = log10(SA)),
        position = position_nudge(x = 0.15),
        width = 0.1,
        color = "black",
        fill = NA,
        linetype = "solid") +
      stat_summary(
        data = data_list[[type]], mapping = aes(x = type, y = log10(SA)),
        fun = mean, geom = "point", color = "black", size = 3, shape = 17,  
        position = position_nudge(x = 0.15))
    
  } else if (type == "OWF") {
    test <- test +
      geom_violinhalf(
        data = data_list[[type]],
        mapping = aes(x = type, y = log10(SA), fill = type), flip = T,
        position = position_nudge(x = -0.1)) +
      geom_boxplot(
        data = data_list[[type]],
        mapping = aes(x = type, y = log10(SA)),
        position = position_nudge(x = -0.15),
        width = 0.1,
        color = "black",
        fill = NA,
        linetype = "solid") +
      stat_summary(
        data = data_list[[type]], mapping = aes(x = type, y = log10(SA)),
        fun = mean, geom = "point", color = "black", size = 3, shape = 17,  
        position = position_nudge(x = -0.15))
  }
}

print(test)

taf.png(file.path(figurePath, paste0("WBAT overall2 - pairwise comparison - 70khz.png")))
print(test)
dev.off()






################################################################################
# Working on WBAT_season showing SA over the seasons
################################################################################

# Sub-setting on threshold -50 & mutating season
WBAT_season <- WBAT.all.summary %>%
  filter(pairing != -1, n >= 8, 
         # depthMaxR > 0.25, 
         treshold == -50) %>% 
  mutate(season = case_when(
    dataSet == "2021-BE" ~ "summer",
    dataSet == "2023-BE" ~ "spring",
    dataSet == "2023-BSW" ~ "summer",
    dataSet == "2024-BE" ~ "autumn"),
    season = factor(season, levels = c("spring", "summer", "autumn")))

str(WBAT_season)

# Overview of seasons
taf.png(file.path(figurePath, paste0("WBAT season - grouped on Type - 70khz.png")))
ggplot(subset(WBAT_season, frequency == 70), aes(x = season, y = log10(SA), fill = type)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 4)) +
  labs(x = "Type", y = "Mean log10(SA)") +
  theme_minimal()
  # theme(legend.position = "none")
dev.off()



# Summarizing mean_SA, mutate slope & give colours
WBAT_season_sum <- WBAT_season %>%
  group_by(stationSet, pairingName, type, pairing, frequency, season) %>% 
  summarise(mean_SA = mean(log10(SA), na.rm = T),
            SAsd = sd(log10(SA), na.rm = T),
            sem_SA = sd(log10(SA), na.rm = TRUE) / sqrt(n())) 

WBAT_season_sum <- WBAT_season_sum %>% 
  group_by(pairingName, frequency) %>%
  summarise(slope = (mean_SA[type == "control"] - mean_SA[type == "OWF"]) / (as.numeric(factor(type))[type == "control"] - as.numeric(factor(type))[type == "OWF"]),
            .groups = "drop") %>%
  left_join(WBAT_season_sum, join_by(pairingName, frequency)) %>% 
  mutate(line_color = ifelse(slope > 0, "blue", "red"))

pairing_colors_season <- setNames(RColorBrewer::brewer.pal(n = length(unique(WBAT_season_sum$pairingName)), "Set1"),
                           unique(WBAT_season_sum$pairingName))
line_colors_season <- c("red" = "red", "blue" = "blue")
all_colors_season <- c(pairing_colors_season, line_colors)

# Plotting grouped on season


# Season per pairing
taf.png(file.path(figurePath, paste0("WBAT season - grouped on Type with pairwise comparison - 70khz.png")))

ggplot(subset(WBAT_season_sum, frequency == 70), aes(x = type, y = mean_SA)) +
  geom_line(aes(group = pairingName, color = line_color), 
            linewidth = 1.25, lineend = "round") +
  geom_point(aes(color = pairingName), size = 6) +
  scale_color_manual(values = all_colors_season) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "Type", y = "Mean log10(SA)") +
  theme_minimal() +
  facet_wrap( ~ season)

dev.off()

################################################################################
# Working on WBAT_weekday showing SA over the different weekdays and workdays/weekenddays
################################################################################

# Sub-setting on threshold -50 & mutating season
WBAT_weekday <- WBAT.all.summary %>%
  filter(pairing != -1, n >= 8, 
         # depthMaxR > 0.25, 
         treshold == -50) %>% 
  mutate(week_day = wday(datetime, label = TRUE, abbr = FALSE) %>% 
           as.character()) %>%
  mutate(week_day = factor(week_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  mutate(weekend_boys = case_when(
         week_day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "Work day",
         week_day %in% c("Saturday", "Sunday") ~ "Weekend day")) %>% 
  mutate(weekend_boys = factor(weekend_boys, levels = c("Work day", "Weekend day")))

str(WBAT_weekday)

taf.png(file.path(figurePath, paste0("WBAT weekday - grouped on Type - 70khz.png")))  
ggplot(subset(WBAT_weekday, frequency == 70), aes(x = week_day, y = log10(SA), fill = type)) +
  geom_boxplot() +
  labs(x = "Day of the week", y = "log10(SA)") +
  coord_cartesian(ylim = c(0.1, 3.5)) +
  theme_minimal() +
  theme(# panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dotted"))
dev.off()

taf.png(file.path(figurePath, paste0("WBAT weekend_day - grouped on Type  - 70khz.png")))
ggplot(subset(WBAT_weekday, frequency == 70), aes(x = weekend_boys, y = log10(SA), fill = type)) +
  geom_boxplot() +
  labs(x = "Work or weekend day", y = "log10(SA)") +
  coord_cartesian(ylim = c(0.1, 3.5)) +
  theme_minimal() +
  theme(# panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "dotted"))
dev.off()

################################################################################
# Working on WBAT_day/night showing SA day part
################################################################################

# Sub-setting on threshold -50 & mutating hour
WBAT_dayNight <- WBAT.all.summary %>%
  filter(pairing != -1, n >= 8, 
         # depthMaxR > 0.25, 
         treshold == -50) 

taf.png(file.path(figurePath, paste0("WBAT dayNight - 70khz.png")))
print(ggplot(subset(WBAT_dayNight, frequency == 70 & pairingName != "2021-BE_2"), 
             aes(x = type, y = log10(SA), fill = dayNight))) +
  geom_boxplot() +
  # stat_summary(aes(x = type, label = round(..y.., 2)),
  #              fun = mean, geom = "text", color = "black", size = 4, 
  #              hjust = -.5, vjust = 0) +
  labs(x = "Day or Night", y = "log10(SA)") +
  coord_cartesian(ylim = c(0.1, 4)) +
  theme_minimal() +
  facet_grid(~ pairingName)
dev.off()





# Testing ----------
WBAT_dayNight_sum <- WBAT_dayNight %>%
  group_by(stationSet, pairingName, pairing, frequency, dayNight, type) %>% 
  summarise(mean_SA = mean(log10(SA), na.rm = T),
            SAsd = sd(log10(SA), na.rm = T),
            sem_SA = sd(log10(SA), na.rm = TRUE) / sqrt(n())) 

WBAT_dayNight_sum <- WBAT_dayNight_sum %>% 
  group_by(stationSet, frequency) %>%
  summarise(slope = (mean_SA[dayNight == "night"] - mean_SA[dayNight == "day"]) / (as.numeric(factor(dayNight))[dayNight == "night"] - as.numeric(factor(dayNight))[dayNight == "day"]),
            .groups = "drop") %>%
  left_join(WBAT_dayNight_sum, join_by(stationSet, frequency)) %>% 
  mutate(line_color = ifelse(slope > 0, "blue", "red"))

custom_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
pairing_colors_dayNight <- setNames(custom_palette(length(unique(WBAT_dayNight_sum$stationSet))), unique(WBAT_dayNight_sum$stationSet))

line_colors_dayNight <- c("red" = "red", "blue" = "blue")
all_colors_dayNight <- c(pairing_colors_dayNight, line_colors)


taf.png(file.path(figurePath, paste0("WBAT dayNight pairwise comparison - 70khz.png")))
ggplot(subset(WBAT_dayNight_sum, frequency == 70), aes(x = dayNight, y = mean_SA)) +
  geom_line(aes(group = stationSet, color = line_color), 
            linewidth = 1.25, lineend = "round") +
  geom_point(aes(color = stationSet), size = 6) +
  scale_color_manual(values = all_colors_dayNight) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "dayNight", y = "Mean log10(SA)") +
  theme_minimal() 
dev.off()

ggplot(subset(WBAT_dayNight_sum, frequency == 70 & type == "OWF"), aes(x = dayNight, y = mean_SA)) +
  geom_line(aes(group = stationSet, color = line_color), 
            linewidth = 1.25, lineend = "round") +
  geom_point(aes(color = stationSet), size = 6) +
  scale_color_manual(values = all_colors_dayNight) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "dayNight", y = "Mean log10(SA)") +
  theme_minimal() 

ggplot(subset(WBAT_dayNight_sum, frequency == 70 & type == "control"), aes(x = dayNight, y = mean_SA)) +
  geom_line(aes(group = stationSet, color = line_color), 
            linewidth = 1.25, lineend = "round") +
  geom_point(aes(color = stationSet), size = 6) +
  scale_color_manual(values = all_colors_dayNight) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "dayNight", y = "Mean log10(SA)") +
  theme_minimal() 

ggplot(subset(WBAT_dayNight_sum, frequency == 70), aes(x = dayNight, y = mean_SA)) +
  geom_line(aes(group = stationSet, color = line_color), 
            linewidth = 1.25, lineend = "round") +
  geom_point(aes(color = stationSet), size = 6) +
  scale_color_manual(values = all_colors_dayNight) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "dayNight", y = "Mean log10(SA)") +
  theme_minimal() +
  facet_grid( ~ type)


# Creating it on type 

WBAT_dayNight_sum$type <- factor(WBAT_dayNight_sum$type, levels = c("OWF", "control"))

pairing_colors_dayNight <- setNames(c("lightcoral", "lightblue"), unique(WBAT_season_sum$type))
line_colors_dayNight <- c("red" = "red", "blue" = "blue")
all_colors_dayNight <- c(pairing_colors_dayNight, line_colors_dayNight)

taf.png(file.path(figurePath, paste0("WBAT dayNight on type - 70khz.png")))
ggplot(subset(WBAT_dayNight_sum, frequency == 70), aes(x = dayNight, y = mean_SA)) +
  geom_line(aes(group = stationSet, color = line_color), 
            linewidth = 1.25, lineend = "round") +
  geom_point(aes(color = type 
                 # shape = type
                 ), size = 6) +
  scale_color_manual(values = all_colors_dayNight) +
  # scale_shape_manual(values = c("OWF" = 16, "control" = 17)) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "dayNight", y = "Mean log10(SA)") +
  theme_minimal() 
dev.off()





################################################################################
# Working on WBAT_hour showing SA per hour
################################################################################

# Sub-setting on threshold -50 & mutating hour
WBAT_hour <- WBAT.all.summary %>%
  filter(pairing != -1, n >= 8, 
         # depthMaxR > 0.00, 
         treshold == -50) %>% 
  mutate(time = as.numeric(format(datetime, "%H")))

str(WBAT_hour)

# Plotting time 
taf.png(file.path(figurePath, paste0("WBAT weekday - with 2024 (.00 drop) - 70khz.png")))
print(ggplot(subset(WBAT_hour, frequency == 70), aes(x = as.factor(time), y = log10(SA), fill = type))) +
  geom_boxplot() +
  # stat_summary(aes(x = time, label = round(..y.., 2)),
  #              fun = mean, geom = "text", color = "black", size = 4, 
  #              hjust = -.5, vjust = 0) +
  labs(x = "Hour of the day (UTC)", y = "log10(SA)") +
  coord_cartesian(ylim = c(0.1, 3)) +
  # ylab(0.5, 3.5) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  facet_grid( ~type) +
  ggtitle('WBAT hour - with 2024 (.00 drop) - 70khz.png')
dev.off()

# Plotting time without the extremes of 2024-BE_1
taf.png(file.path(figurePath, paste0("WBAT hour - without 2024 (.00 drop) - 70khz.png")))
print(ggplot(subset(WBAT_hour, frequency == 70 & pairingName != "2024-BE_1"), aes(x = as.factor(time), y = log10(SA), fill = type))) +
  geom_boxplot() +
  labs(x = "Time in hours", y = "log10(SA)") +
  coord_cartesian(ylim = c(0.1, 3)) +
  # ylab(0.5, 3.5) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  facet_grid( ~type) +
  ggtitle('WBAT hour - without 2024 (.00 drop) - 70khz.png')
dev.off()













