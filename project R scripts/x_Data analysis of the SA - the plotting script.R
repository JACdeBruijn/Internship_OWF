################################################################################
# Script for plotting - 08/12/24 ----
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, RColorBrewer, icesTAF, see, gridExtra,
               grid, ggstar, ggtext)

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

# Making type a factor and removing unneeded columns
WBAT.all.summary <- WBAT.all.summary %>% 
  distinct(IDinter, .keep_all = TRUE) %>% 
  mutate(type = factor(type, levels = c("control", "OWF"), label = c("Control", "OWF"))) %>% 
  select(-freq, 
         # -lat, -lon, 
         -soundtrap, -CPOD, -WBAT_70, -WBAT_200, 
         # -hourSunset, -hourSunrise, -sunset, -sunrise
         )

################################################################################
# Mutating the depthMaxR in the data set 
# ---- update: this is not used in the final data analysis anymore as the imputation is not based on correct things

# WBAT.all.summary %>%                                                            # Checking the depthIntegration per stationSet to see if the imputation of depthMaxR makes sense
#   group_by(stationSet, pairingName) %>%
#   summarize(mean_depth = mean(depthIntegration), .groups = "drop")

# WBAT.all.summary <- WBAT.all.summary %>%                                        # Adding depthMaxR
#   group_by(frequency, station, phase, dataSet) %>% 
#   mutate(depthMaxR = depthIntegration / max(depthIntegration)) 


################################################################################
# Working on WBAT_overview showing SA total ----
################################################################################

# Sub-setting on threshold -50
WBAT_overview <- WBAT.all.summary %>% 
  filter(n >= 8, treshold == -50) 

# Some basic visualizations 
hist(WBAT.all.summary$n)
hist(WBAT_overview$n)
str(WBAT.all.summary)
str(WBAT_overview)

# Overview - VIOLIN
taf.png(file.path(figurePath, paste0("WBAT overview Mean log10(SA) grouped on type - 70khz.png")))
ggplot(subset(WBAT_overview, frequency == 70), aes(x = type, y = log10(SA), fill = type)) +
  geom_violin(position = "dodge", width = 0.5, outlier.alpha = 0.1) + 
  stat_summary(aes(x = type, label = round(..y.., 2)),
               fun = mean, geom = "text", color = "black", size = 4, 
               hjust = 0.5, vjust = 0) +
  coord_cartesian(ylim = c(0, 4)) +
  labs(x = "Type", y = "mean log10(SA)") +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(color = "black"),
    axis.text.x = element_text(color = "black", vjust = 0.5, hjust = 0.5)) +
  ggtitle('WBAT overview - grouped on Type - 70 kHz')
dev.off()

# Overview - BOXPLOT
taf.png(file.path(figurePath, paste0("WBAT overview Mean log10(SA) grouped on type - 70khz.png")))
ggplot(subset(WBAT_overview, frequency == 70), 
       aes(x = type, y = log10(SA), fill = type)) +
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

# Summarizing mean_SA, mutate slope & give colors
WBAT_overview_sum <- WBAT_overview %>%
  group_by(stationSet, pairingName, type, frequency) %>% 
  summarise(mean_SA = mean(log10(SA), na.rm = T),
            SAsd = sd(log10(SA), na.rm = T),
            sem_SA = sd(log10(SA), na.rm = TRUE) / sqrt(n()))
            # Q1 = quantile(log10(SA)[2]),
            # Q3 = quantile(log10(SA)[4]))
            
quantile(log10(WBAT_overview$SA))[3]
        
# Testing the simple 
# wilcox.test(SA ~ type, data = WBAT_overview, exact = FALSE)
# model <- lmer(log10(SA) ~ type + (1 | stationSet), data = WBAT_overview)
# summary(model)

WBAT_overview_sum <- WBAT_overview_sum %>% 
  group_by(pairingName, frequency) %>%
  summarise(slope = (mean_SA[type == "Control"] - mean_SA[type == "OWF"]) / (as.numeric(factor(type))[type == "Control"] - as.numeric(factor(type))[type == "OWF"]),
            .groups = "drop") %>%
  left_join(WBAT_overview_sum, join_by(pairingName, frequency)) %>% 
  mutate(line_color = ifelse(slope > 0, "#e4d49a", "#91c1ee"))




### Testing for black but different colours update =---
manual_shapes <- c(1, 13, 15, 23, 28, 22)[seq_len(6)]

ggplot(subset(WBAT_overview_sum, frequency == 70 & pairingName != "2021-BE_2"), 
       aes(x = type, y = mean_SA)) +
  geom_line(aes(group = pairingName, color = line_color), linewidth = 0.75, linetype = "solid") +
  geom_star(aes(starshape = pairingName), color = "black", fill = "black", size = 6) +
  scale_starshape_manual(values = manual_shapes)



#ccac3d for offshore windfarm
#darker line color #796620 for farm
#lighter line color #e4d49a for farm

#388fe0 for Shipwereck
#darker line color #154e84 for wreck
#lighter line color for #91c1ee wreck





pairing_colors <- setNames(RColorBrewer::brewer.pal(n = length(unique(WBAT_overview_sum$pairingName)), "Set1"),
                           unique(WBAT_overview_sum$pairingName))
line_colors <- c("#e4d49a" = "#e4d49a", "#91c1ee" = "#91c1ee")
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
  if (type == "Control") {
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


# Editing the names of the pairingName
WBAT_overview_sum <- WBAT_overview_sum %>%
  mutate(pairingName = factor(pairingName, 
                              levels = c("2021-BE_1", "2023-BSW_1", "2023-BSW_2", "2023-BE_1", "2023-BE_2", "2024-BE_1"), 
                              label = c("July-Aug/21 - Birk-/Belw-",
                                        "May-June/23 - Bors 1/Bors 4",
                                        "May-June/23 - Bors 2/Bors 3",
                                        "July-Sept/23 - Gard-/Belw-",
                                        "July-Sept/23 - Graf-/Cpow-",
                                        "Oct-Dec/23 - Graf-/Cpow-")))

WBAT_manual_shapes <- c(1, 13, 15, 23, 28, 22)[seq_len(6)]

################################################################################ Testing
test <- ggplot(subset(WBAT_overview_sum, frequency == 70), aes(x = type, y = mean_SA)) +
  geom_line(aes(group = pairingName, color = line_color), 
            linewidth = 3, lineend = "round",
            show.legend = F) +
  scale_color_manual(values = all_colors) +
  geom_star(aes(starshape = pairingName), 
            color = "black", 
            fill = NA,
            starstroke = 1.5,
            size = 8) +
  scale_starshape_manual(values = WBAT_manual_shapes,
                         name = "Pairings - Period & Location") +
  coord_cartesian(ylim = c(0, 4)) +
  # scale_y_log10() +
  labs(x = "Location type", y = "Area Backscattering Coefficient log<sub>10</sub>(s<sub>a</sub>)") +
  theme_minimal() +
  theme(
    # axis.title.x = element_text(size = 25),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.title.y = element_markdown(size = 25),
    axis.text.y = element_text(size = 20, color = "black"),
    legend.position = "none",
    # legend.position.inside = c(0.5, .75),
    # legend.background = element_rect(fill = NA, size = .5),
    # legend.margin = margin(2, 2, 2, 3),
    # legend.key.size = unit(40, "pt"),
    # legend.text = element_text(size = 15),
    # legend.title = element_text(size = 18),
    # legend.spacing.y = unit(.1, "cm")
  )

data_list <- split(subset(WBAT_overview, frequency == 70), subset(WBAT_overview, frequency == 70)$type)

for (type in names(data_list)) {
  if (type == "OWF") {
    # For "OWF", plot on one side
    test <- test +
      geom_violinhalf(
        data = data_list[[type]],
        mapping = aes(x = type, y = log10(SA)), 
        fill = "#ccac3d",
        width = 0.5, 
        flip = F,
        linewidth = NA,
        position = position_nudge(x = 0.10),
        show.legend = F) +
      geom_boxplot(
        data = data_list[[type]],
        mapping = aes(x = type, y = log10(SA)),
        position = position_nudge(x = 0.15),
        outlier.shape = NA,
        width = 0.1,
        color = "black",
        fill = NA,
        linetype = "solid") +
      stat_summary(
        data = data_list[[type]], mapping = aes(x = type, y = log10(SA)),
        fun = mean, geom = "point", color = "black", size = 6, shape = 17,  
        position = position_nudge(x = 0.15))
    
  } else if (type == "Control") {
    test <- test +
      geom_violinhalf(
        data = data_list[[type]],
        mapping = aes(x = type, y = log10(SA)), 
        fill = "#388fe0",
        width = 0.5, 
        flip = T,
        linewidth = NA,
        position = position_nudge(x = -0.10),
        show.legend = F) +
      geom_boxplot(
        data = data_list[[type]],
        mapping = aes(x = type, y = log10(SA)),
        position = position_nudge(x = -0.15),
        outlier.shape = NA,
        width = 0.1,
        color = "black",
        fill = NA,
        linetype = "solid") +
      stat_summary(
        data = data_list[[type]], mapping = aes(x = type, y = log10(SA)),
        fun = mean, geom = "point", color = "black", size = 6, shape = 17,  
        position = position_nudge(x = -0.15))
  }
}

print(test)

# Saving the plot
ggsave(filename = file.path(figurePath, 'UPDATED3 with 2024-BE - WBAT overall pairwise comparsion - 70kHz.png'), plot = test, width = 14, height = 10)

lay_overview <- rbind(c(1),
                      c(2))

WBAT_CPOD_grid <- grid.arrange(test, C4,
                               widths = c(1),
                               layout_matrix = lay_overview)

ggsave(filename = file.path(figurePath, 'testing the grid function to merge WBAT and CPOD.png'), plot = WBAT_CPOD_grid, width = 14, height = 16)

###############################################################################
# Working on WBAT_season showing SA over the seasons ----
################################################################################
# Sub-setting on threshold -50 & mutating season
WBAT_season <- WBAT.all.summary %>%
  filter(n >= 8, treshold == -50) %>% 
  mutate(season = case_when(
    dataSet == "2021-BE" ~ "Summer",
    dataSet == "2023-BE" ~ "Spring",
    dataSet == "2023-BSW" ~ "Summer",
    dataSet == "2024-BE" ~ "Autumn"),
    season = factor(season, levels = c("Spring", "Summer", "Autumn")))

str(WBAT_season)

# Overview of seasons - Not needed for in the presentations 
ggplot(subset(WBAT_season, frequency == 70), aes(x = season, y = log10(SA), fill = type)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 4)) +
  labs(x = "Type", y = "Mean log10(SA)") +
  theme_minimal()
  # theme(legend.position = "none")

# ggsave(filename = file.path(figurePath, 'WBAT season - grouped on Type - 70khz.png'), plot = test, width = 15, height = 10)


# Summarizing mean_SA, mutate slope & give colors
WBAT_season_sum <- WBAT_season %>%
  group_by(stationSet, pairingName, type, pairing, frequency, season) %>% 
  summarise(mean_SA = mean(log10(SA), na.rm = T),
            SAsd = sd(log10(SA), na.rm = T),
            sem_SA = sd(log10(SA), na.rm = TRUE) / sqrt(n()),
            Q1_SA = quantile(log10(SA), probs = 0.25, na.rm = TRUE),  # First quartile (Q1)
            Q3_SA = quantile(log10(SA), probs = 0.75, na.rm = TRUE))   # Third quartile (Q3)

WBAT_season_sum <- WBAT_season_sum %>% 
  group_by(pairingName, frequency) %>%
  summarise(slope = (mean_SA[type == "Control"] - 
                       mean_SA[type == "OWF"]) / 
              (as.numeric(factor(type))[type == "Control"] - 
                 as.numeric(factor(type))[type == "OWF"]),
            .groups = "drop") %>%
  left_join(WBAT_season_sum, join_by(pairingName, frequency)) %>% 
  mutate(line_color = ifelse(slope > 0, "#e4d49a", "#91c1ee"))


pairing_colors_season <- setNames(RColorBrewer::brewer.pal(n = length(unique(WBAT_season_sum$pairingName)), "Set1"),
                           unique(WBAT_season_sum$pairingName))
line_colors_season <- c("#e4d49a" = "#e4d49a", "#91c1ee" = "#91c1ee")
all_colors_season <- c(pairing_colors_season, line_colors_season)

# Plotting grouped on season
p1_season <-
  ggplot(subset(WBAT_season_sum, frequency == 70), aes(x = type, y = mean_SA)) +
  # geom_errorbar(aes(
  #   # ymin = Q1_SA,  # First quartile (lower bound)
  #   # ymax = Q3_SA   # Third quartile (upper bound)
  #   ymin = mean_SA - sem_SA,  # Lower bound of error bar
  #   ymax = mean_SA + sem_SA  # Upper bound of error bar
  # ), width = 0.2, linewidth = 0.5) +
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
  labs(x = "Type", y = "Mean log<sub>10</sub>(SA)") +
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

ggsave(filename = file.path(figurePath, "WBAT season2 - grouped on Type with pairwise comparison split on season - 70khz.png"), plot = p1_season, width = 15, height = 10)

# Season per pairing
p2_season <- ggplot(subset(WBAT_season_sum, frequency == 70), aes(x = type, y = mean_SA)) +
  geom_line(aes(group = pairingName, color = line_color), 
            linewidth = 1.25, lineend = "round") +
  geom_point(aes(color = pairingName), size = 6) +
  scale_color_manual(values = all_colors_season) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "Type", y = "Mean log10(SA)") +
  theme_minimal() 

ggsave(filename = file.path(figurePath, "WBAT season - grouped on Type with pairwise comparison - 70khz.png"), plot = p2_season, width = 15, height = 10)

################################################################################
# Working on WBAT_weekday showing SA over the different weekdays and workdays/weekenddays ----
################################################################################

# Sub-setting on threshold -50 & mutating season
WBAT_weekday <- WBAT.all.summary %>%
  filter(n >= 8, treshold == -50) %>% 
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
# Working on week plotting showing SA per over the week numbers ----
################################################################################
WBAT_weeknumber <- WBAT.all.summary %>%
  filter(n >= 8, treshold == -50) %>% 
  mutate(week_num = isoweek(datetime)) 

WBAT_weeknumber <- WBAT_weeknumber %>% 
  complete(week_num = seq(min(week_num, na.rm = T), max(week_num, na.rm = T), by = 1)) 

all_weeks <- tibble(week_num = seq(min(WBAT_weeknumber$week_num, na.rm = TRUE), 
                                   max(WBAT_weeknumber$week_num, na.rm = TRUE), 
                                   by = 1))

# Join the full sequence with the original data
WBAT_weeknumber <- all_weeks %>%
  left_join(WBAT_weeknumber, by = "week_num") %>% 
  filter(!week_num %in% c("18", "49"))

unique(WBAT_weeknumber$week_num)

p1_weeknum <- ggplot(subset(WBAT_weeknumber, frequency == 70), aes(x = as.factor(week_num), y = log10(SA), fill = type)) +
  annotate("rect", 
           xmin = .5, xmax = 7.5, 
           ymin = -Inf, ymax = Inf, 
           fill = "#92c84c", alpha = .2) +
  annotate("rect", 
           xmin = 7.5, xmax = 20.5, 
           ymin = -Inf, ymax = Inf, 
           fill = "#f47155", alpha = .2) +
  annotate("rect", 
           xmin = 20.5, xmax = 30.5, 
           ymin = -Inf, ymax = Inf, 
           fill = "#f7b354", alpha = .2) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#388fe0", "#ccac3d")) +
  coord_cartesian(ylim = c(0, 3.5)) +
  labs(y = "log<sub>10</sub>(s<sub>a</sub>)") +
  scale_x_discrete(limits = factor(sort(unique(WBAT_weeknumber$week_num)))) +
  theme_pubclean() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.title.y = element_markdown(size = 25),
    axis.text.y = element_text(size = 20, color = "black"),
    legend.position = "none"
    )

station_sets_per_week <- WBAT_weeknumber %>%
  group_by(week_num, type) %>%
  summarise(unique_station_sets = n_distinct(stationSet, na.rm = T), .groups = "drop") %>% 
  mutate(unique_station_sets = if_else(is.na(unique_station_sets), 0L, unique_station_sets))

# Create a plot to show the number of unique station sets per week_num
p2_weeknum <- 
  ggplot(station_sets_per_week, aes(x = as.factor(week_num), y = unique_station_sets, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", width = .8) +
  scale_fill_manual(values = c("#388fe0", "#ccac3d")) +
  # geom_text(aes(label = unique_station_sets), vjust = .2, size = 4) +
  labs(x = "Week Number", y = "Number of station") +
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

plot <- grid.arrange(p1_weeknum, p2_weeknum, layout_matrix = lay)

ggsave(filename = file.path(figurePath, "WBAT weeknummber3 - grouped on Type - 70khz_draft.png"), plot = plot, width = 14, height = 10)



lay_week_number_combined <- rbind(c(1),
                                  c(2))

plot_week_number_combined <- grid.arrange(plot, CPOD_weeknum, layout_matrix = lay_week_number_combined)
ggsave(filename = file.path(figurePath, "Combining the CPOD and WBAT weeknumber plots.png"), plot = plot_week_number_combined, width = 14, height = 16)


################################################################################
# Working on WBAT_day/night showing SA day part ----
################################################################################

# Sub-setting on threshold -50 & mutating hour
WBAT_dayNight <- WBAT.all.summary %>%
  mutate(dayNight = factor(dayNight, levels = c("day", "night"), label = c("Day", "Night"))) %>%
  filter(n >= 8, treshold == -50) 

ggplot(subset(WBAT_dayNight, frequency == 70 & pairingName != "2021-BE_2"), 
             aes(x = type, y = log10(SA), fill = dayNight)) +
  geom_boxplot() +
  # stat_summary(aes(x = type, label = round(..y.., 2)),
  #              fun = mean, geom = "text", color = "black", size = 4, 
  #              hjust = -.5, vjust = 0) +
  labs(x = "Day or Night", y = "log10(SA)") +
  coord_cartesian(ylim = c(0.1, 4)) +
  theme_minimal() +
  facet_grid(~ pairingName)

ggsave(filename = file.path(figurePath, "WBAT dayNight - 70khz.png"), plot = p2_season, width = 15, height = 10)

# Testing ----------
WBAT_dayNight_sum <- WBAT_dayNight %>%
  group_by(stationSet, pairingName, pairing, frequency, dayNight, type) %>% 
  summarise(mean_SA = mean(log10(SA), na.rm = T),
            SAsd = sd(log10(SA), na.rm = T),
            sem_SA = sd(log10(SA), na.rm = TRUE) / sqrt(n())) 

WBAT_dayNight_sum <- WBAT_dayNight_sum %>% 
  group_by(stationSet, frequency) %>%
  summarise(slope = (mean_SA[dayNight == "Night"] - mean_SA[dayNight == "Day"]) / (as.numeric(factor(dayNight))[dayNight == "Night"] - as.numeric(factor(dayNight))[dayNight == "Day"]),
            .groups = "drop") %>%
  left_join(WBAT_dayNight_sum, join_by(stationSet, frequency)) %>% 
  mutate(line_color = ifelse(slope > 0, "black", "coral"))

custom_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))
pairing_colors_dayNight <- setNames(custom_palette(length(unique(WBAT_dayNight_sum$stationSet))), unique(WBAT_dayNight_sum$stationSet))

line_colors_dayNight <- c("grey" = "grey", "black" = "black")
all_colors_dayNight <- c(pairing_colors_dayNight, line_colors_dayNight)



ggplot(subset(WBAT_dayNight_sum, frequency == 70), aes(x = dayNight, y = mean_SA)) +
  geom_line(aes(group = stationSet, color = line_color), 
            linewidth = 1.25, lineend = "round") +
  geom_point(aes(color = stationSet), size = 6) +
  scale_color_manual(values = all_colors_dayNight) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "dayNight", y = "Mean log10(SA)") +
  theme_minimal() 

ggsave(filename = file.path(figurePath, "WBAT dayNight pairwise comparison - 70khz.png"), plot = p2_season, width = 15, height = 10)

ggplot(subset(WBAT_dayNight_sum, frequency == 70 & type == "OWF"), aes(x = dayNight, y = mean_SA)) +
  geom_line(aes(group = stationSet, color = line_color), 
            linewidth = 1.25, lineend = "round") +
  geom_point(aes(color = stationSet), size = 6) +
  scale_color_manual(values = all_colors_dayNight) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "dayNight", y = "Mean log10(SA)") +
  theme_minimal() 

ggplot(subset(WBAT_dayNight_sum, frequency == 70 & type == "Control"), aes(x = dayNight, y = mean_SA)) +
  geom_line(aes(group = stationSet, color = line_color), 
            linewidth = 1.25, lineend = "round") +
  geom_point(aes(color = stationSet), size = 6) +
  scale_color_manual(values = all_colors_dayNight) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "dayNight", y = "Mean log10(SA)") +
  theme_minimal() 

ggplot(subset(WBAT_dayNight_sum, frequency == 70), aes(x = dayNight, y = mean_SA)) +
  geom_line(aes(group = pairingName, color = line_color), 
            linewidth = 1.25, lineend = "round") +
  geom_point(aes(color = pairingName), size = 6) +
  scale_color_manual(values = all_colors_dayNight) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "dayNight", y = "Mean log10(SA)") +
  theme_minimal() +
  facet_grid( ~ type)


# Creating it on type 
pairing_colors_dayNight <- setNames(c("#ccac3d", "#388fe0"), unique(WBAT_season_sum$type))
line_colors_dayNight <- c("black" = "black", "coral" = "coral")
all_colors_dayNight <- c(pairing_colors_dayNight, line_colors_dayNight)

#WBAT dayNight on type - 70khz.png
WBAT_dayNight <-
  ggplot(subset(WBAT_dayNight_sum, frequency == 70), aes(x = dayNight, y = mean_SA)) +
  geom_line(aes(group = stationSet, color = line_color), 
            linewidth = 1.25, lineend = "round", show.legend = F) +
  geom_point(aes(color = type 
                 # shape = type
                 ), size = 6) +
  scale_color_manual(values = all_colors_dayNight) +
  # scale_shape_manual(values = c("OWF" = 16, "control" = 17)) +
  # coord_cartesian(ylim = c(0, 3.5)) +
  labs(x = "Day or Night", y = "Mean log<sub>10</sub>(SA)") +
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

ggsave(filename = file.path(figurePath, "WBAT dayNight - 70khz2.png"), plot = WBAT_dayNight, width = 15, height = 10)





################################################################################
# Working on WBAT_hour showing SA per hour ----
################################################################################

# Sub-setting on threshold -50 & mutating hour
WBAT_hour <- WBAT.all.summary %>%
  filter(n >= 8, treshold == -50) %>% 
  mutate(time = as.numeric(format(datetime, "%H")))

str(WBAT_hour)

# Plotting time 
taf.png(file.path(figurePath, paste0("WBAT per hour as facet grid - 70khz.png")))
ggplot(subset(WBAT_hour, frequency == 70), aes(x = as.factor(time), y = log10(SA), fill = type)) +
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
  ggtitle('WBAT hour - with 2024 - 70khz.png')
dev.off()

################################################################################
# Working on WBAT against the current ----
################################################################################


# Sub-setting on threshold -50 & mutating hour
WBAT_hour <- WBAT.all.summary %>%
  filter(n >= 8, treshold == -50) %>% 
  mutate(time = as.numeric(format(datetime, "%H")))



current <- read_csv(file.path(dataPath, "avrcur_TS_09d9_e042_b77d.csv"))

str(current)     


station_summary <- current %>% 
  group_by(station_id) %>% 
  summarise(lon = first(longitude),
            lat = first(latitude),
            min_speed = min(avrcur, na.rm = T),
            max_speed = max(avrcur, na.rm = T))



################################################################################-
# Visualizing the sampling dates for Jeroen ----

CPOD_all_min

WBAT_overview_summary <- WBAT_overview %>%
  group_by(stationSet) %>%
  summarise(
    first_sampling = min(datetime),
    last_sampling = max(datetime))




