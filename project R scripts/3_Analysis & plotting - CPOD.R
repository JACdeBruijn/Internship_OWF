################################################################################-
## 3_Analysis & plotting - CPOD ----
# Script for plotting CPOD data --
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, RColorBrewer, icesTAF, see, suncalc, gridExtra,
               grid, RColorBrewer, ggtext, egg, ggpubr, ggstar, nlme)

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
# Loading the data ----
##############################################################################--
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
# Adjust the CPOD per --DAY-- and replace NA to 0
##############################################################################--
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

CPOD_all_day <- CPOD.all.day %>% 
  mutate(across(c(buzz, inter, other, all_buzz, all_no_buzz, all, buzz_ratio, pos_minutes, buzz_pos_minutes), 
                ~ replace_na(.x, 0))) # Mutate all the NA values of the columns to ZERO 

# Adjust the CPOD per --HOUR-- and replace NA to 0
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
         -freq, -lat, -lon, -soundtrap, -CPOD, -WBAT_70, -WBAT_200, -type.y) %>% 
  mutate(across(c(buzz_pos_minutes, buzz_ratio), ~ if_else(.x == 0, NA, .x)))

CPOD_all_hour <- CPOD.all.hour %>% 
  mutate(across(c(buzz, inter, other, all_buzz, all_no_buzz, all, pos_minutes, buzz_pos_minutes), 
                ~ replace_na(.x, 0))) %>%  # Mutate all the NA values of the columns to ZERO 
  mutate(pph = if_else(pos_minutes == 0, 0, 1),
         ppbuzzh = if_else(buzz_pos_minutes == 0, 0, 1))

# Adjust the CPOD per --MINUTE-- and replace NA to 0
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

CPOD_all_min <- CPOD.all.min %>% 
  mutate(across(c(buzz, inter, other, all_buzz, all_no_buzz, all), 
                ~ replace_na(.x, 0))) # Mutate all the NA values of the columns to ZERO 

################################################################################-
# Looking into the includation of the NA values or not as this will largely influence the data ----
# I should include the 0 values and replace the NA values. The machine was on and therefore they are true zero's. 
##############################################################################--

# first visuals
hist(CPOD.all.hour$pos_minutes)
hist(CPOD_all_hour$pos_minutes)

hist(CPOD.all.hour$buzz)
hist(CPOD.all.hour$buzz_pos_minutes)
hist(CPOD.all.hour$buzz_ratio)
str(CPOD.all.hour)

# on hour grouped
ggplot(CPOD.all.hour, aes(x = type, y = (pos_minutes)/60), fill = type) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, .25))

ggplot(CPOD_all_hour, aes(x = type, y = (pos_minutes)/60), fill = type) +
  geom_boxplot()

# on hour facet
ggplot(CPOD.all.hour, aes(x = type, y = (pos_minutes)/60), fill = type) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, .25)) +
  facet_grid(~ pairingName)

ggplot(CPOD_all_hour, aes(x = type, y = (pos_minutes)/60), fill = type) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, .25)) +
  facet_grid(~ pairingName)

# on day
ggplot(CPOD.all.day, aes(x = type, y = pos_minutes), fill = type) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 200)) +
  facet_grid(~ pairingName)

################################################################################-
# Making the overview CPOD ----
##############################################################################--

# summarizing the CPOD_hour data
CPOD_all_hour_sum <- CPOD_all_hour %>%
  group_by(stationSet, pairingName, type, pairing) %>% 
  summarise(mean_pph = mean(pph, na.rm = T),
            sd_pph = sd(pph, na.rm = T),
            sem_pph = sd(pph, na.rm = TRUE) / sqrt(n())) 

# plot for line diagram
ggplot(CPOD_all_hour_sum, aes(x = type, y = mean_pph)) +
  geom_point(aes(shape = type, color = pairingName), size = 4) +
  scale_shape_manual(values = c(16, 17)) +
  geom_line(aes(group = pairingName), linetype = 'solid', linewidth = .75, lineend = "round") +
  labs(x = "Location type", y = "mean pph") +
  theme_minimal()

# Calculating the slope to calculate the color that the lines need to have
CPOD_all_hour_sum <- CPOD_all_hour_sum %>% 
  group_by(pairingName) %>%
  summarise(slope = (mean_pph[type == "Control"] - mean_pph[type == "OWF"]) / (as.numeric(factor(type))[type == "Control"] - as.numeric(factor(type))[type == "OWF"]),
            .groups = "drop") %>%
  left_join(CPOD_all_hour_sum, join_by(pairingName)) %>% 
  mutate(line_color = ifelse(slope > 0, "#e4d49a", "#91c1ee"))

CPOD_all_hour_sum <- CPOD_all_hour_sum %>%
  mutate(pairingName = factor(pairingName, 
                              levels = c("2021-BE_1", "2021-BE_2", "2023-BSW_1", "2023-BSW_2", "2023-BE_1", "2023-BE_2", "2024-BE_1"), 
                              label = c("July-Aug/21 - Birk-/Belw-",
                                        "July-Aug/21 - Graf-/Cpow-",
                                        "May-June/23 - Bors 1/Bors 4",
                                        "May-June/23 - Bors 2/Bors 3",
                                        "July-Sept/23 - Gard-/Belw-",
                                        "July-Sept/23 - Graf-/Cpow-",
                                        "Oct-Dec/23 - Graf-/Cpow-")))

CPOD_all_hour_prop <- CPOD_all_hour %>%
  mutate(pph = factor(pph, levels = c("0", "1"), label = c("Absent", "Present"))) %>% 
  group_by(type, pph) %>%
  summarize(count = n()) %>%
  mutate(proportion = count / sum(count))

CPOD_manual_shapes <- c(1, 5, 13, 15, 23, 28, 22)[seq_len(7)]
line_colors <- c("#e4d49a" = "#e4d49a", "#91c1ee" = "#91c1ee")

# Define the color mapping for type
pph_color_map <- c("Present.OWF" = "#ccac3d", "Absent.OWF" = "grey", "Present.Control" = "#388fe0", "Absent.Control" = "grey")

# Plot the overview 
C4 <- ggplot(CPOD_all_hour_sum, aes(x = type, y = mean_pph)) +
  geom_line(aes(group = pairingName, color = line_color), 
            linewidth = 3, 
            linetype = "solid",
            lineend = "round",
            show.legend = F) +
  scale_color_manual(values = line_colors) +
  geom_star(aes(starshape = pairingName), 
            color = "black", 
            fill = NA,
            starstroke = 1.5,
            size = 8) +
  scale_starshape_manual(values = CPOD_manual_shapes,
                         name = "Pairings - Period & Location") +
  # scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Location type", y = "Detection Positive Hours") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 25),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.title.y = element_markdown(size = 25),
    axis.text.y = element_text(size = 20, color = "black"),
    legend.position = "inside",
    legend.position.inside = c(0.5, 1),
    legend.background = element_rect(fill = NA, size = .5),
    legend.margin = margin(2, 2, 2, 3),
    legend.key.size = unit(40, "pt"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.spacing.y = unit(0, "cm")
  )
  
data_list <- split(CPOD_all_hour_prop, CPOD_all_hour_prop$type)

for (type in names(data_list)) {
  if (type == "OWF") {
    C4 <- C4 +
      geom_bar(
        data = data_list[[type]],
        mapping = aes(x = as.numeric(as.factor(type)) + 0.3, y = proportion, fill = interaction(pph, type)),
        stat = "identity", 
        position = "fill",
        width = 0.3,
        show.legend = F) +
      geom_text(
        data = data_list[[type]],
        mapping = aes(x = type, y = proportion, label = scales::percent(proportion)),
        position = position_nudge(x = 0.3),
        color = "black", size = 6)
    
  } else if (type == "Control") {
    C4 <- C4 +
      geom_bar(
        data = data_list[[type]],
        mapping = aes(x = as.numeric(as.factor(type)) - 0.3, y = proportion, fill = interaction(pph, type)),
        stat = "identity", 
        position = "fill",
        width = 0.3,
        show.legend = F) +
      geom_text(
        data = data_list[[type]],
        mapping = aes(x = type, y = proportion, label = scales::percent(proportion)),
        position = position_nudge(x = -0.3),
        color = "black", size = 6)
  }
}

# apply the color scale for the type (OWF and Control)
C4 <- C4 + scale_fill_manual(values = pph_color_map)

print(C4)

ggsave(filename = file.path(figurePath, 'CPOD overall pairwise comparsion3_draft.png'), plot = C4, width = 14, height = 10)


##############################################################################--
# Making the season CPOD ----
##############################################################################--

CPOD_all_hour_season <- CPOD_all_hour %>%
  mutate(season = case_when(
    dataSet == "2021-BE" ~ "Summer",
    dataSet == "2023-BE" ~ "Spring",
    dataSet == "2023-BSW" ~ "Summer",
    dataSet == "2024-BE" ~ "Autumn"),
    season = factor(season, levels = c("Spring", "Summer", "Autumn")))

CPOD_all_hour_season_sum <- CPOD_all_hour_season %>%
  group_by(stationSet, pairingName, type, pairing, season) %>% 
  summarise(mean_pph = mean(pph, na.rm = T),
            SA_pph = sd(pph, na.rm = T))

CPOD_all_hour_season_sum <- CPOD_all_hour_season_sum %>% 
  group_by(pairingName) %>%
  summarise(slope = (mean_pph[type == "Control"] - 
                       mean_pph[type == "OWF"]) / 
              (as.numeric(factor(type))[type == "Control"] - 
                 as.numeric(factor(type))[type == "OWF"]),
            .groups = "drop") %>%
  left_join(CPOD_all_hour_season_sum, join_by(pairingName)) %>% 
  mutate(line_color = ifelse(slope > 0, "#e4d49a", "#91c1ee"))

CPOD_line_colors_season <- c("#e4d49a" = "#e4d49a", "#91c1ee" = "#91c1ee")

CPOD_season <-
  ggplot(CPOD_all_hour_season_sum, aes(x = type, y = mean_pph)) +
  geom_line(aes(group = pairingName, color = line_color), 
            linewidth = 1.25, lineend = "round",
            show.legend = F) +
  geom_point(aes(color = type), size = 8) +
  # geom_star(aes(starshape = pairingName, color = type), 
  #             # color = , 
  #             # fill = type,
  #             starstroke = 1,
  #             size = 8) +
  # scale_starshape_manual(values = WBAT_manual_shapes,
  #                          name = "Period & Location per pair") +
  scale_color_manual(values = c("#388fe0", "#ccac3d", "#91c1ee", "#e4d49a")) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  facet_grid(~season) +
  labs(x = "Type", y = "Hours with HP presence ratio") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 30),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.title.y = element_markdown(size = 30),
    axis.text.y = element_text(size = 20, color = "black"),
    legend.position = "inside",
    legend.position.inside = c(0.5, .8),
    legend.background = element_rect(fill = "white", size = .5),
    legend.margin = margin(3, 3, 3, 3),
    legend.key.size = unit(30, "pt")
  )

ggsave(filename = file.path(figurePath, 'CPOD season pairwise comparsion.png'), plot = CPOD_season, width = 15, height = 10)
  


################################################################################-
# Making the weeknumber plots for CPOD ----
##############################################################################--

CPOD_all_hour_weeknumber <- CPOD_all_hour %>%
  mutate(week_num = isoweek(time_hour)) 

CPOD_all_hour_weeknumber <- CPOD_all_hour_weeknumber %>% 
  complete(week_num = seq(min(week_num, na.rm = T), max(week_num, na.rm = T), by = 1)) %>% 
  filter(!week_num %in% c("49", "50"))                                          # Week 49 and 50 only have data during control and not for the OWF, therefore I removed them

unique(CPOD_all_hour_weeknumber$week_num)

CPOD_all_hour_weeknumber_prop <- CPOD_all_hour_weeknumber %>% 
  mutate(pph = factor(pph, levels = c("0", "1"), labels = c("Absent", "Present"))) %>%
  group_by(week_num, type, pph) %>% 
  summarise(count = n()) %>% 
  mutate(proportion = count / sum(count)) %>% 
  filter(!pph %in% c("Absent"))

CPOD1_weeknum <-
  ggplot(CPOD_all_hour_weeknumber_prop, aes(x = as.factor(week_num), y = proportion, fill = type)) +
  annotate("rect", 
           xmin = .5, xmax = 8.5, 
           ymin = -Inf, ymax = Inf, 
           fill = "#92c84c", alpha = .2) +
  annotate("rect", 
           xmin = 8.5, xmax = 21.5, 
           ymin = -Inf, ymax = Inf, 
           fill = "#f47155", alpha = .2) +
  annotate("rect", 
           xmin = 21.5, xmax = 31.5, 
           ymin = -Inf, ymax = Inf, 
           fill = "#f7b354", alpha = .2) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#388fe0", "#ccac3d")) +
  scale_x_discrete(limits = factor(sort(unique(CPOD_all_hour_weeknumber$week_num)))) +
  labs(x = "Location type", y = "Detection Positive Hours") +
  # geom_text(aes(label = count), vjust = .2, size = 4) +
  theme_pubclean() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.title.y = element_markdown(size = 25),
    axis.text.y = element_text(size = 20, color = "black"),
    legend.position = "none"
    )

station_sets_per_week <- CPOD_all_hour_weeknumber %>%
  group_by(week_num, type) %>%
  summarise(unique_station_sets = n_distinct(stationSet, na.rm = T), .groups = "drop") %>% 
  mutate(unique_station_sets = if_else(is.na(unique_station_sets), 0L, unique_station_sets))

# Create a plot to show the number of unique station sets per week_num
CPOD2_weeknum <-
  ggplot(station_sets_per_week, aes(x = as.factor(week_num), y = unique_station_sets, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", width = .8) +
  scale_fill_manual(values = c("#388fe0", "#ccac3d")) +
  # geom_text(aes(label = unique_station_sets), vjust = .2, size = 4) +
  labs(x = "Week Number", y = "Number of stations") +
  theme_pubclean() +
  theme(
    axis.title.x = element_markdown(size = 25),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.title.y = element_markdown(size = 25),
    axis.text.y = element_text(size = 20, color = "black"),
    legend.position = "none"
    )

lay <- rbind(c(1),
             c(1),
             c(2))

CPOD_weeknum <-
  grid.arrange(CPOD1_weeknum, CPOD2_weeknum, 
               widths = c(1),
               layout_matrix = lay)

ggsave(filename = file.path(figurePath, 'CPOD weeknum_draft.png'), plot = CPOD_weeknum, width = 14, height = 10)




################################################################################-
# Making the dayNights plots for CPOD ----
##############################################################################--

CPOD_dayNight_sum <- CPOD_all_hour %>%
  mutate(dayNight = factor(dayNight, levels = c("day", "night"), label = c("Day", "Night"))) %>% 
  group_by(stationSet, pairingName, pairing, dayNight, type) %>% 
  summarise(mean_pph = mean(pph, na.rm = T))

CPOD_dayNight_sum <- CPOD_dayNight_sum %>% 
  group_by(stationSet) %>%
  summarise(slope = (mean_pph[dayNight == "Night"] - mean_pph[dayNight == "Day"]) / (as.numeric(factor(dayNight))[dayNight == "Night"] - as.numeric(factor(dayNight))[dayNight == "Day"]),
            .groups = "drop") %>%
  left_join(CPOD_dayNight_sum, join_by(stationSet)) %>% 
  mutate(line_color = ifelse(slope > 0, "black", "coral"))



#WBAT dayNight on type - 70khz.png
CPOD_dayNight <-
  ggplot(CPOD_dayNight_sum, aes(x = dayNight, y = mean_pph)) +
  geom_line(aes(group = stationSet, color = line_color), 
            linewidth = 1.25, lineend = "round", show.legend = F) +
  geom_point(aes(color = type 
                 # shape = type
  ), size = 6) +
  scale_color_manual(values = all_colors_dayNight) +
  # scale_shape_manual(values = c("OWF" = 16, "control" = 17)) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "Day or Night", y = "Hours with HP presence ratio - pph") +
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

ggsave(filename = file.path(figurePath, "CPOD dayNight.png"), plot = CPOD_dayNight, width = 15, height = 10)




################################################################################-
# Making the sunangle plots for CPOD ----
##############################################################################--
CPOD_all_hour_sun <- CPOD_all_hour %>% 
  mutate(time = as.numeric(format(time_hour, "%H")))

# Creating a data set with the day and night based on the sunposition
CPOD_sunny <- CPOD_all_hour_sun %>%
  mutate(dayNight_ele = ifelse(sunPos_altitude >= 0, "Day", "Night"))

# Imputing the phases of the day/sun
CPOD_sunny <- CPOD_sunny %>%
  group_by(stationSet) %>% 
  arrange(time_hour) %>% 
  mutate(phase_sun = case_when(
    sunPos_altitude >= 0 & lead(sunPos_altitude) > sunPos_altitude ~ "Morning",
    sunPos_altitude >= 0 & lead(sunPos_altitude) < sunPos_altitude ~ "Evening",
    sunPos_altitude < 0 & lead(sunPos_altitude) > sunPos_altitude ~ "Dawn",
    sunPos_altitude < 0 & lead(sunPos_altitude) < sunPos_altitude ~ "Night",
    TRUE ~ NA_character_)) %>% 
  filter(!is.na(phase_sun))

# Doing some binning and splitting
CPOD_sunny <- CPOD_sunny %>%
  group_by(stationSet) %>% 
  mutate(altitude_bin = cut(
    sunPos_altitude, 
    breaks = seq(-80, 80, by = 10), 
    labels = seq(-75, 75, by = 10),
    include.lowest = TRUE),
    altitude_bin = as.numeric(as.character(altitude_bin))) %>% 
  filter(altitude_bin != "-65") %>%                                             # At -65 there are only 18 values
  mutate(phase_sun = if_else(phase_sun == "Morning" & altitude_bin == 65, "Evening", phase_sun)) %>% 
  # filter(!(phase_sun == "Morning" & altitude_bin == "65")) %>%                  # At 65 in the morning there are only 4 values
  arrange(stationSet, time_hour) %>% 
  ungroup()

CPOD_sunny_prop <- CPOD_sunny %>%
  mutate(pph = factor(pph, levels = c("0", "1"), labels = c("Absent", "Present"))) %>% 
  group_by(phase_sun, altitude_bin, type, pph) %>%
  summarize(count = n()) %>%  
  mutate(proportion = count / sum(count)) %>% 
  filter(!pph %in% c("Absent"))



# Testing 

# CPOD_sunny_prop_test <- CPOD_sunny %>%
#   mutate(pph = factor(pph, levels = c("0", "1"), labels = c("Absent", "Present"))) %>% 
#   group_by(phase_sun, altitude_bin, stationSet) %>%
#   summarize(count = n()) %>%  
#   mutate(proportion = count / sum(count)) %>% 
#   view()






# Create a vertical gradient matrix
gradient_colors_morning <- colorRampPalette(c("black", "white"))(100)
gradient_matrix_morning <- matrix(gradient_colors_morning, nrow = 1, ncol = 100)
vertical_gradient_morning <- rasterGrob(image = gradient_matrix_morning, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)

gradient_colors_evening <- colorRampPalette(c("white", "black"))(100)
gradient_matrix_evening <- matrix(gradient_colors_evening, nrow = 1, ncol = 100)
vertical_gradient_evening <- rasterGrob(image = gradient_matrix_evening, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)

# Plotting the first 2 phases 
p1_CPOD <- ggplot(subset(CPOD_sunny_prop, phase_sun %in% c("Morning", "Dawn")), aes(x = factor(altitude_bin), y = proportion, fill = type)) +
  annotation_custom(vertical_gradient_morning, 
                    xmin = 4.45, xmax = 6.5, 
                    ymin = -Inf, ymax = Inf) +
  annotate("rect", 
           xmin = -Inf, xmax = 4.5, 
           ymin = -Inf, ymax = Inf, 
           fill = "black") +
  geom_vline(xintercept = 6.5, linetype = "dashed") +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(x = "Sun altitude at Sunrise (deg)", y = "Detection Positive Minutes") +
  scale_fill_manual(values = c("Control" = "#388fe0", "OWF" = "#ccac3d")) +
  coord_cartesian(ylim = c(0, 1)) +
  # facet_grid(rows = "type") +
  theme_pubclean() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.title.y = element_markdown(size = 25),
    axis.text.y = element_text(size = 20, color = "black"),
    # strip.text.y = element_blank()
  )

# Plotting the 3 and the 4 phases
p2_CPOD <- ggplot(subset(CPOD_sunny_prop, phase_sun %in% c("Evening", "Night")), aes(x = factor(altitude_bin), y = proportion, fill = type)) +
  annotation_custom(vertical_gradient_evening, 
                    xmin = 7.5, xmax = 9.5, 
                    ymin = -Inf, ymax = Inf) +
  annotate("rect", 
           xmin = 9.5, xmax = Inf, 
           ymin = -Inf, ymax = Inf, 
           fill = "black") +
  geom_vline(xintercept = 7.5, linetype = "dashed") +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  scale_x_discrete(
    name = "Sun altitude at Sunset (deg)",
    labels = function(x) ifelse(x == "0", "Sunrise/Sunset", x),
    limits = rev(levels(factor(subset(CPOD_sunny_prop, phase_sun %in% c("Evening", "Night"))$altitude_bin)))) +
  scale_fill_manual(values = c("Control" = "#388fe0", "OWF" = "#ccac3d")) +
  coord_cartesian(ylim = c(0, 1)) +
  # facet_grid(rows = "type") +
  theme_pubclean() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),    
    axis.line.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20, color = "black"),
    plot.margin=unit(c(5.5, 5.5, 5.5, -15), "pt")
  )



station_sets_per_sunPos <- CPOD_sunny %>%
  group_by(phase_sun, altitude_bin, type) %>%
  summarise(unique_station_sets = n_distinct(stationSet, na.rm = T), .groups = "drop") %>% 
  mutate(unique_station_sets = if_else(is.na(unique_station_sets), 0L, unique_station_sets))

# Create a plot to show the number of unique station sets per week_num            aes(x = as.factor(week_num), y = unique_station_sets, fill = type)) +
p3_CPOD <-
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
  
p4_CPOD <-
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

lay_CPOD_sun <- rbind(c(1, 2),
             c(1, 2),
             c(3, 4))

CPOD_sun <-
  grid.arrange(p1_CPOD, p2_CPOD, p3_CPOD, p4_CPOD, 
               layout_matrix = lay_CPOD_sun)


# Commented out so that I do not accidentally enable it
ggsave(filename = file.path(figurePath,'CPOD _ Sun altitude around sunset and sunrise with light gradient_better3_draft.png'), plot = CPOD_sun, width = 14, height = 10)





##############################################################################--
# Notes and thoughts from here ----
# Looking into ppm/h--
##############################################################################--
total_hours <- CPOD_all_hour %>%
  group_by(stationSet, pairingName, type) %>% 
  summarise(
    total_hours = as.numeric(difftime(max(time_hour), min(time_hour), units = "hours"))) 

CPOD_all_hour_test <- CPOD.all.hour %>% 
  mutate(across(c(buzz, inter, other, all_buzz, all_no_buzz, all, pos_minutes, buzz_pos_minutes), 
                ~ replace_na(.x, 0))) %>%  # Mutate all the NA values of the columns to ZERO 
  mutate(hour = if_else(pos_minutes == 0, 1, 1))

total_hours_check <- CPOD_all_hour_test %>% 
  group_by(stationSet, pairingName, type) %>% 
  summarise(total_hours_check = sum(hour, na.rm = T))

ggplot(CPOD_all_hour, aes(x = time_hour, y = 1)) + 
  geom_line() + 
  facet_wrap(~stationSet, scales = "free_x") + 
  labs(
    title = "Measurement Gaps by Station Set", x = "Time (time_hour)", y = "Measurement Indicator") + 
  theme_minimal()

total_hours %>% 
  left_join(total_hours_check, join_by(stationSet, pairingName, type)) %>% 
  summarise(diff = total_hours - total_hours_check)

# Total hours check is calculated based on the total number of hours in the dataset 
# While total hours is based on the first and last date, so I am gone take the total hours check hours

CPOD_all_hour_test_sum <- CPOD_all_hour_test %>% 
  group_by(stationSet, pairingName, type) %>% 
  summarise(ppm_total = sum(pos_minutes, na.rm = T),
            total_hours_check = sum(hour/60*50, na.rm = T),
            total_minutes_check = total_hours_check*60, na.rm = T) %>% 
  mutate(ppm_per_hour = ppm_total / total_hours_check,
         ppm_per_min = ppm_total / total_minutes_check)

test3 <- ggplot(CPOD_all_hour_test_sum, aes(x = type, y = ppm_per_hour, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#388fe0", "#ccac3d")) +
  labs(title = "porpoise positive minutes/h-1 over the stationSets by type", 
       x = "type", 
       y = "porpoise positive minutes/h-1") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  facet_grid( ~pairingName)

ggsave(filename = file.path(figurePath, "porpoise positive minutes_h-1 over the stationSets by type.png"), plot = test3, width = 15, height = 10)

# summarized on the mean of the pairing Names on type
CPOD_all_hour_test_sum_mean <- CPOD_all_hour_test_sum %>% 
  group_by(type) %>% 
  summarise(mean_ppm = mean(ppm_per_hour),
            sd_ppm = sd(ppm_per_hour))

test4 <- ggplot(CPOD_all_hour_test_sum_mean, aes(x = type, y = mean_ppm, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#388fe0", "#ccac3d")) +
  geom_errorbar(aes(ymin = (mean_ppm - sd_ppm), ymax = (mean_ppm + sd_ppm)), 
    width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Mean ppm/h-1 split on type", 
       x = "type", 
       y = "Mean porpoise positive minutes/h-1") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() 

ggsave(filename = file.path(figurePath, 'Mean ppm_h-1 split on type.png'), plot = test4, width = 15, height = 10)
#

# testing it with the zero's removed!!!!!!!!!!!!!!!11
CPOD.all.hour.test.sum <- CPOD.all.hour %>% 
  group_by(stationSet, pairingName, type) %>% 
  summarise(ppm_total = sum(pos_minutes, na.rm = T),
            total_hours_check = sum(hour/60*50, na.rm = T),
            total_minutes_check = total_hours_check*60, na.rm = T) %>% 
  mutate(ppm_per_hour = ppm_total / total_hours_check,
         ppm_per_min = ppm_total / total_minutes_check)

ggplot(CPOD.all.hour.test.sum, aes(x = type, y = ppm_per_hour, fill = type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#388fe0", "#ccac3d")) +
  labs(title = "porpoise positive minutes/h-1 over the stationSets by type", 
       x = "type", 
       y = "porpoise positive minutes") +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  facet_grid( ~pairingName)


ggplot(CPOD_all_hour_noNA, aes(x = type, y = (pos_minutes)/60, fill = type)) +
  geom_bar(stat = "identity") +
  facet_grid(~pairingName)

ggplot(CPOD.all.hour, aes(x = type, y = buzz_ratio, fill = buzz_ratio)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Type", y = "buzz?ratio") +
  facet_grid(~pairingName)


SUM <- CPOD_all_hour_noNA %>%
  mutate(pph = factor(pph, levels = c("0", "1"), label = c("Absent", "Present"))) %>% 
  group_by(type, pph) %>%
  summarize(count = n()) %>%
  mutate(proportion = count / sum(count))
  
ggplot(SUM, aes(x = type, y = proportion, fill = factor(pph))) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    aes(label = scales::percent(proportion)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 6
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Type",
    y = "Proportion",
    fill = "Presence/Absence",
    title = "Proportion of Presence (1) and Absence (0) by Type"
  ) +
  theme_minimal()


SUM <- CPOD_all_hour_noNA %>%
  mutate(ppbh = factor(ppbh, levels = c("0", "1"), label = c("Absent", "Present"))) %>% 
  group_by(type, ppbh) %>%
  summarize(count = n()) %>%
  mutate(proportion = count / sum(count))

ggplot(SUM, aes(x = type, y = proportion, fill = factor(ppbh))) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    aes(label = scales::percent(proportion)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 6
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Type",
    y = "Proportion",
    fill = "Presence/Absence",
    title = "Proportion of Presence (1) and Absence (0) by Type"
  ) +
  theme_minimal()


SUM <- CPOD_all_hour_noNA %>%
  group_by(type, buzz_ratio) %>%
  summarize(count = n()) %>%
  mutate(proportion = count / sum(count))

ggplot(SUM, aes(x = type, y = proportion, fill = buzz_ratio)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    aes(label = scales::percent(proportion)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 6
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Type",
    y = "Proportion",
    fill = "Presence/Absence",
    title = "Proportion of Presence (1) and Absence (0) by Type"
  ) +
  theme_minimal()



# Testing idea of Midas
summary_data <- CPOD_all_hour_noNA %>%
  group_by(type, pos_minutes) %>%
  summarize(count = n(), .groups = "drop")

# Plot the summarized data
ggplot(summary_data, aes(x = pos_minutes, y = count, color = type, group = type)) +
  geom_line(linewidth = 1) +
  scale_x_log10()


gggeom_bar()ggplot(CPOD_all_hour_noNA, aes(x = type, y = (pos_minutes)/50, fill = type)) +
  geom_violin(position = "dodge", width = 0.5, outlier.shape = NA) + 
  stat_summary(aes(x = type, label = round(..y.., 2)),
               fun = mean, geom = "text", color = "black", size = 4, 
               hjust = 0.5, vjust = 0) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(x = "Type", y = "Fraction positive minutes/hour") +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black", vjust = 0.5, hjust = 0.5)) +
  ggtitle('CPOD overview - on type - FracPres/hour')
  # facet_grid(~ pairingName)

ggplot(CPOD_all_hour_no0s, aes(x = type, y = (buzz_pos_minutes), fill = type)) +
  geom_boxplot(position = "dodge", width = 0.5, outlier.shape = NA) + 
  stat_summary(aes(x = type, label = round(..y.., 2)),
               fun = mean, geom = "text", color = "black", size = 4, 
               hjust = 0.5, vjust = 0) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(x = "Type", y = "Fraction buzz minutes/hour") +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black", vjust = 0.5, hjust = 0.5)) +
  ggtitle('CPOD overview - on type - Fracbuzz/hour') +
  facet_grid(~ pairingName)

ggplot(CPOD_all_hour_noNA, aes(x = type, y = (buzz_ratio), fill = type)) +
  geom_violin(position = "dodge", width = 0.5, outlier.shape = NA) + 
  stat_summary(aes(x = type, label = round(..y.., 2)),
               fun = mean, geom = "text", color = "black", size = 4, 
               hjust = 0.5, vjust = 0) +
  coord_cartesian(ylim = c(0, .25)) +
  labs(x = "Type", y = "BuzzRatio") +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black", vjust = 0.5, hjust = 0.5)) +
  ggtitle('CPOD overview - on type - BuzzRatio') 


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

CPOD_weg <- ggplot(CPOD_min, aes(x = minuut)) +
  geom_bar() +
  labs(x = "Minute in the hour", y = "Total occurrences of HP") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 30),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.title.y = element_markdown(size = 30),
    axis.text.y = element_text(size = 20, color = "black"))

hist(CPOD_min$minuut)

ggsave(filename = file.path(figurePath, "CPOD_deterance.png"), plot = CPOD_weg, width = 15, height = 10)

################################################################################-
# Visualizing the sampling dates for Jeroen ----
##############################################################################--

CPOD_all_min

CPOD_all_min_summary <- CPOD_all_min %>%
  group_by(stationSet) %>%
  summarise(
    first_sampling = min(time_minute),
    last_sampling = max(time_minute))

