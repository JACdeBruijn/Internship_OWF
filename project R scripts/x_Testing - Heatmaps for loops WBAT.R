if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)

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

load(file = file.path(resultPath,'CPOD_WBAT_workspace.RData'))

### Working with data

data_filtered <- WBAT.all.summary %>%
  filter(stationSet == "2021-BE_grafton")

data_filtered$time <- as.numeric(format(data_filtered$datetime, "%H"))
################################################################################
# This is the line that I need for the SA over time to make the datetime into hour (time)
################################################################################
WBAT.all.summary$time <- as.numeric(format(WBAT.all.summary$datetime, "%H"))

WBAT.all_filtered <- WBAT.all.summary %>% 
  filter(!dataSet %in% c("2022-cpower", "2022-HKZ", "2023-HKN"))
  

################################################################################
# Grouping on season
################################################################################

WBAT.all.summary <- WBAT.all.summary %>%
  mutate(
    season = case_when(
      dataSet == "2021-BE" ~ "summer",
      dataSet == "2023-BE" ~ "spring",
      dataSet == "2023-BSW" ~ "summer",
      dataSet == "2024-BE" ~ "autumn"),
    season = factor(season, levels = c("spring", "summer", "autumn")))

WBAT.all.summary <- WBAT.all.summary %>%
  mutate(week_day = wday(datetime, label = TRUE, abbr = FALSE) %>% 
           as.character()) %>%
  mutate(week_day = factor(week_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

WBAT.all.summary <- WBAT.all.summary %>% 
  mutate(
    weekend_boys = case_when(
      week_day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~ "a Work day",
      week_day %in% c("Saturday", "Sunday") ~ "b Weekend day"))

ggplot(WBAT.all.summary, aes(x = as.factor(weekend_boys), y = log10(SA))) +
  geom_boxplot() +
  labs(title = "Hourly log SA over time per pair",
       x = "Datetime",
       y = "log10(SA)",
       color = "Time of Day") +
  theme_minimal() +
  ylim(-0.5, 4.5) +
  theme(# panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.5, linetype = "dotted")) +
  facet_grid(pairingName~ type)




means <- WBAT_weekday %>%
  group_by(weekend_boys, pairingName, type) %>%
  summarize(mean_SA = mean(log10(SA), na.rm = TRUE)) %>%
  ungroup()

ggplot(WBAT.all.summary, aes(x = as.factor(weekend_boys), y = log10(SA), colour = type)) +
  geom_segment(data = means %>%
                 pivot_wider(names_from = weekend_boys, values_from = mean_SA) %>%
                 filter(!is.na(`Work day`) & !is.na(`Weekend day`)), # Ensure both means exist
               aes(
                 x = "Work day", xend = "Weekend day",
                 y = `Work day`, yend = `Weekend day`,
                 group = interaction(pairingName, type),
                 color = type
               ),
               inherit.aes = FALSE, 
               # color = "red", 
               size = 1, linetype = "dashed") +
  labs(title = "mean log10(SA) for workdays and weekenddays per type",
       x = "Datetime",
       y = "log10(SA)",
       color = "Time of Day") +
  theme_minimal() +
  ylim(1, 3) +
  theme(# panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.5, linetype = "dotted")) +
  facet_grid(~ pairingName)

######################means#####################################################
# figuring out what the phase 1 and phase 2 are
################################################################################

Phase_questionmark <- WBAT.all_filtered %>% 
  select(stationSet, treshold, IDinter, frequency, phase, SA, datetime)

ggplot(Phase_questionmark, aes(x = datetime, y = stationSet, group = interaction(stationSet, phase), color = phase)) +
  geom_line()

Phase_wide <- Phase_questionmark %>%
  pivot_wider(names_from = phase, values_from = datetime, names_prefix = "datetime_")


ggplot(Phase_wide, aes(y = stationSet)) +
  geom_line(aes(x = P1, color = "Phase 1")) +
  geom_line(aes(x = P2, color = "Phase 2")) +
  labs(
    title = "Datetime Comparison between Phase 1 and Phase 2",
    x = "Datetime",
    y = "Station Set"
  ) +
  scale_color_manual(values = c("Phase 1" = "blue", "Phase 2" = "red"), name = "Phase") +
  theme_minimal()

Phase_questionmark <- Phase_questionmark %>% 
  filter(stationSet == "2021-BE_grafton") 

Phase_1 <- Phase_questionmark %>% 
  filter(phase == "P1")

Phase_2 <- Phase_questionmark %>% 
  filter(phase == "P2")

time_diff_data <- Phase_1 %>%
  inner_join(Phase_2, by = "stationSet", suffix = c("_1", "_2")) 

data_long <- time_diff_data %>%
  pivot_longer(cols = starts_with("datetime"), names_to = "phase", values_to = "datetime") %>%
  mutate(phase = ifelse(phase == "datetime_1", "Phase 1", "Phase 2"))

time_diff_summary <- time_diff_data %>%
  summarize(
    mean_diff = mean(time_diff, na.rm = T),
    median_diff = median(time_diff, na.rm = T),
    sd_diff = sd(time_diff, na.rm = T),
    max_diff = max(time_diff, na.rm = T),
    min_diff = min(time_diff, na.rm = T)
  )


########################################################################

########################################################################


ggplot(data_filtered, aes(x = as.factor(time), y = log10(SA))) +
  geom_boxplot()+
  labs(
    title = "Hourly Mean SA Value Over Time for StationSet 2021_BE_grafton",
    x = "Datetime",
    y = "Mean SA Value",
    color = "Time of Day"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.5, linetype = "dotted")
  )

################################################################################
# This is the plot that I need for the SA over time
################################################################################
ggplot(WBAT.all.summary, aes(x = as.factor(time), y = log10(SA))) +
  geom_boxplot() +
  labs(title = "Hourly log SA over time per pair",
       x = "Datetime",
       y = "log10(SA)",
       color = "Time of Day") +
  theme_minimal() +
  ylim(-0.5, 4.5) +
  theme(# panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.5, linetype = "dotted")) +
  facet_grid( ~ type)


ggplot(data_filtered, aes(x = datetime, y = log10(SA), color = dayNight)) +
  geom_line(size = 1) +  # Line plot to show SA over time
  geom_point(size = 2) +  # Optional: add points to highlight individual hourly values
  labs(
    title = "Hourly Mean SA Value Over Time for StationSet 2021_BE_grafton",
    x = "Datetime",
    y = "Mean SA Value",
    color = "Time of Day") +
  theme_minimal() +
  theme(
    # panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.5, linetype = "dotted"))


ggplot(data_filtered, aes(x = SA, fill = dayNight)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(
    title = "Distribution of SA Values for StationSet 2021_BE_grafton",
    x = "SA Value",
    y = "Frequency",
    fill = "Time of Day"
  ) +
  theme_minimal()


############################
# SA distribution
############################
WBAT.all_filtered_T50_f70 <- WBAT.all_filtered %>% 
  filter(treshold == "-50",
         frequency == "70")

WBAT.all_filtered_T60_f70 <- WBAT.all_filtered %>% 
  filter(treshold == "-60",
         frequency == "70")

ggplot(WBAT.all_filtered_T50_f70, aes(x = log10(SA))) +
  geom_histogram(binwidth = .1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of SA Values at -50dB for 70kHz",
    x = "log10(SA)",
    y = "Frequency") +
  theme_minimal()

ggplot(WBAT.all_filtered_T60_f70, aes(x = log10(SA))) +
  geom_histogram(binwidth = .1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of SA Values ar -60dB for 70kHz",
       x = "log10(SA)",
       y = "Frequency") +
  theme_minimal()





############################
# Plotting depth against time for SA visuals
############################
WBAT_2021_BE_P1_belwind_70khz <- WBAT.all
####### Separate 

WBAT_2021_BE_P1_belwind_70khz_50freq <- WBAT_2021_BE_P1_belwind_70khz %>% 
  filter(treshold %in% c("-50"), depth != 0) %>% 
  arrange(datetime) %>%
  mutate(log10_SA = ifelse(SA == 0, NA, log10(SA)),
         interval_number = dense_rank(datetime))

ggplot(WBAT_2021_BE_P1_belwind_70khz_50freq, aes(x = log10(SA))) +
  geom_histogram(binwidth = .1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of SA Values at -50dB for 70kHz",
       x = "log10(SA)",
       y = "Frequency") +
  theme_minimal()

WBAT_2021_BE_P1_belwind_70khz_50freq_25drop <- quantile(WBAT_2021_BE_P1_belwind_70khz_50freq$log10_SA, 0.25, na.rm = TRUE)
WBAT_2021_BE_P1_belwind_70khz_50freq_50drop <- quantile(WBAT_2021_BE_P1_belwind_70khz_50freq$log10_SA, 0.50, na.rm = TRUE)
WBAT_2021_BE_P1_belwind_70khz_50freq_75drop <- quantile(WBAT_2021_BE_P1_belwind_70khz_50freq$log10_SA, 0.75, na.rm = TRUE)

WBAT_2021_BE_P1_belwind_70khz_50freq_25drop <- WBAT_2021_BE_P1_belwind_70khz_50freq %>% 
  filter(log10_SA > WBAT_2021_BE_P1_belwind_70khz_50freq_25drop)

WBAT_2021_BE_P1_belwind_70khz_50freq_50drop <- WBAT_2021_BE_P1_belwind_70khz_50freq %>% 
  filter(log10_SA > WBAT_2021_BE_P1_belwind_70khz_50freq_50drop)

WBAT_2021_BE_P1_belwind_70khz_50freq_75drop <- WBAT_2021_BE_P1_belwind_70khz_50freq %>% 
  filter(log10_SA > WBAT_2021_BE_P1_belwind_70khz_50freq_75drop)

ggplot(WBAT_2021_BE_P1_belwind_70khz_50freq_25drop, aes(x = log10(SA))) +
  geom_histogram(binwidth = .1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of SA Values at -50dB for 70kHz",
       x = "log10(SA)",
       y = "Frequency") +
  theme_minimal()




############################
# for -60dB 
WBAT_2021_BE_P1_belwind_70khz_60freq <- WBAT_2021_BE_P1_belwind_70khz %>% 
  filter(treshold %in% c("-60"),
         depth != 0) %>%
  arrange(datetime) %>%
  mutate(SA = SA + 0,001,
    log10_SA = ifelse(SA == 0, 0, log10(SA)),
         interval_number = dense_rank(datetime))

ggplot(WBAT_2021_BE_P1_belwind_70khz_60freq, aes(x = log10(SA))) +
  geom_histogram(binwidth = .1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of SA Values at -60dB for 70kHz",
       x = "log10(SA)",
       y = "Frequency") +
  theme_minimal()

WBAT_2021_BE_P1_belwind_70khz_60freq_25drop <- quantile(WBAT_2021_BE_P1_belwind_70khz_60freq$log10_SA, 0.25, na.rm = TRUE)
WBAT_2021_BE_P1_belwind_70khz_60freq_50drop <- quantile(WBAT_2021_BE_P1_belwind_70khz_60freq$log10_SA, 0.50, na.rm = TRUE)
WBAT_2021_BE_P1_belwind_70khz_60freq_75drop <- quantile(WBAT_2021_BE_P1_belwind_70khz_60freq$log10_SA, 0.75, na.rm = TRUE)

WBAT_2021_BE_P1_belwind_70khz_60freq_25drop <- WBAT_2021_BE_P1_belwind_70khz_50freq %>% 
  filter(log10_SA > WBAT_2021_BE_P1_belwind_70khz_60freq_25drop)

WBAT_2021_BE_P1_belwind_70khz_60freq_50drop <- WBAT_2021_BE_P1_belwind_70khz_50freq %>% 
  filter(log10_SA > WBAT_2021_BE_P1_belwind_70khz_60freq_50drop)

WBAT_2021_BE_P1_belwind_70khz_60freq_75drop <- WBAT_2021_BE_P1_belwind_70khz_50freq %>% 
  filter(log10_SA > WBAT_2021_BE_P1_belwind_70khz_60freq_75drop)

ggplot(WBAT_2021_BE_P1_belwind_70khz_60freq_25drop, aes(x = log10(SA))) +
  geom_histogram(binwidth = .1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of SA Values at -60dB for 70kHz",
       x = "log10(SA)",
       y = "Frequency") +
  theme_minimal()

############################
# Plotting depth against time for SA visuals
############################

frequency <- c(70, 200)
threshold <- c("-50", "-60")
data_drop <- c(0, 25, 50, 75)

for(i in frequency){
  for(j in threshold){
    for(k in data_drop){
      plot_intervals_in_chunks(test, num_iterations = 3)
    }
  }
}


data_depth_00 <- WBAT_2021_BE_P1_belwind_70khz_60freq %>% 
  filter(depth == 0)



ggplot(subset(WBAT_2021_BE_P1_belwind_70khz_60freq_25drop, WBAT_2021_BE_P1_belwind_70khz_60freq_25drop$datetime < as.POSIXct("2021-07-14 10:05:29", tz = "UTC")), 
       aes(x = interval_number, y = depth, fill = log10(SA))) +
  geom_tile() +
  geom_line(aes(y = depthIntegration), color = "red", size = 1) +
  geom_line(data = subset(data_depth_00, data_depth_00$datetime < as.POSIXct("2021-07-14 10:05:29", tz = "UTC"))
            , mapping = aes(y = (log10_SA*4)), color = "green", size = 2) +
  scale_fill_gradientn(
    colors = c("blue","white", "red"),  # Gradient from blue to white to red
    limits = c(-2.5, 5),                  # Set gradient limits for SA values
    name = "SA Value") +
  
  labs(
    title = "SA Values by Depth and Time",
    x = "Time",
    y = "Depth (m)") +
  theme_minimal() +
  theme(panel.grid = element_blank())

########################################################################
# Function to plot heat maps of the depth over time per 2 days
### Testing with 3 iterations
########################################################################

# Define the function to plot the heatmap for a limited number of intervals (e.g., 3 iterations)
plot_intervals_in_chunks <- function(data, gap_interval = 10, num_iterations = 3, frequency, treshold, data_drop) {
  
  # Ensure that data is ordered by interval_number (or datetime if needed)
  data <- data %>% 
    arrange(interval_number) %>% 
    mutate(interval_number_with_gap = ifelse((interval_number %% gap_interval) == 0, NA, interval_number))
  
  # data_depth_0 <- data %>% 
  #   filter(depth == 0)
  
  # Get the total number of interval_numbers
  total_intervals <- nrow(data)
  chunk_size <- 240
  
  # Limit the number of iterations if num_iterations is provided
  num_iterations <- min(num_iterations, ceiling(total_intervals / chunk_size))  # Ensure that we do not exceed available chunks
  
  for (i in 1:num_iterations) {
    # Calculate the start and end interval numbers for the current chunk (240 intervals)
    start_interval <- (i - 1) * chunk_size + 1
    end_interval <- min(i * chunk_size, total_intervals)
    
    # Get the subset of data for the current chunk based on interval_number
    data_chunk <- data %>% filter(interval_number >= start_interval & interval_number <= end_interval)
    
    if (nrow(data_chunk) == 0) {                                                
      message("No data left to process, stopping the loop.")
      break
    }
    
    # Create the heatmap plot for the current chunk
    plot <- ggplot(data_chunk, aes(x = interval_number_with_gap, y = depth, fill = log10_SA)) +
      geom_tile() +
      geom_line(aes(y = depthIntegration), color = "red", size = .5) +
      # geom_line(data = data_depth_0, mapping = aes(y = log10_SA), color = "green", size = .5) +
      scale_fill_gradientn(
        colors = c("blue", "white", "red"),  # Gradient from blue to white to red
        limits = c(-2.5, 5),                # Set gradient limits for SA values
        name = "SA Value") +
      labs(
        title = paste0("log10(SA) at ", frequency, "kHz for ", treshold, "dB with datadrop of ", data_drop, "% - ", start_interval, " to ", end_interval),
        x = "Interval Number",
        y = "Depth (m)") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)  # Optional: tilt x-axis labels
      )
    
    # Save the plot as a PNG file with the correct date and interval range in the filename
    output_folder <- file.path(dataPath, "SA_Heatmaps")
    if(!dir.exists(output_folder)) dir.create(output_folder)
    filename <- paste0(output_folder, "/SA_Heatmap_", frequency, "kHz_", treshold, "dB_datadrop_", data_drop, "%_", start_interval, "_to_", end_interval, ".png")
    ggsave(filename, plot = plot, width = 10, height = 6)
  }
}

# Example usage with WBAT.2021_BE_belwind_70kHz_test_tres50 dataset, limiting to 3 iterations
plot_intervals_in_chunks(WBAT_2021_BE_P1_belwind_70khz_60freq_75drop, gap_interval = 10, num_iterations = 3, frequency = 70, treshold = -60, data_drop = 75)





#######################################################################################################################
# Define the function to plot the heatmap with gaps after every 10 interval_numbers for all iterations
#######################################################################################################################
plot_intervals_gap <- function(data, gap_interval = 10, frequency, treshold) {
  
  data <- data %>% 
    arrange(interval_number) %>%                                                # Ensure that data is ordered by interval_number (or datetime if needed)
    mutate(interval_number_with_gap = ifelse((interval_number %% gap_interval) == 0, NA, interval_number)) # Add gaps to the interval_number every 10 interval_numbers and set interval_number to NA every 10th one
                           
  total_intervals <- nrow(data)                                                 # Get the total number of interval_numbers
  chunk_size <- 240
  
  num_iterations <- ceiling(total_intervals / chunk_size)                       # Calculate the total number of iterations (chunks of 240)
  
  for (i in 1:num_iterations) {
    start_interval <- (i - 1) * chunk_size + 1                                  # Calculate the start and end interval numbers for the current chunk (240 intervals)
    end_interval <- min(i * chunk_size, total_intervals)
    data_chunk <- data %>% filter(interval_number >= start_interval & interval_number <= end_interval)  # Get the subset of data for the current chunk based on interval_number
    
    if (nrow(data_chunk) == 0) {                                                # If the data_chunk has fewer rows than expected (meaning we're at the end of the data), break the loop
      message("No data left to process, stopping the loop.")
      break
    }
    
    plot <- ggplot(data_chunk, aes(x = interval_number_with_gap, y = depth, fill = log10(SA))) +  # Create the heatmap plot for the current chunk
      geom_tile() +
      geom_line(aes(y = depthIntegration), color = "red", size = .5) +
      scale_fill_gradientn(colors = c("blue", "white", "red"), limits = c(-2.5, 5), name = "SA Value") +
      labs(
        title = paste("SA Values by Depth and Interval Numbers", start_interval, "to", end_interval),
        x = "Interval Number", y = "Depth (m)") +
      theme_minimal() +
      theme(panel.grid = element_blank(), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    output_folder <- file.path(dataPath, "SA_Heatmaps")                         # Save the plot as a PNG file with the correct interval range in the filename
    if(!dir.exists(output_folder)) dir.create(output_folder)
    filename <- paste0(output_folder, "/SA_Heatmap_", frequency, "kHz_", treshold, "dB_", start_interval, "_to_", end_interval, ".png")
    ggsave(filename, plot = plot, width = 10, height = 6)
    
  }
}

# The starter ----
plot_intervals_gap(WBAT_2021_BE_P1_belwind_70khz_60freq, gap_interval = 10, frequency = 70, treshold = -60)










#### Code dump from the time that I thought WBAT data had ZEROs in them (14/01/25)
# But in reality the n >= 8 filter was kicking my but!!
# However I managed to reduce the comp time by taking out the tresholds that I dont need!

ggplot(subset(WBAT_modeling, 
              datetime > as.POSIXct("2021-07-14 10:05:29", tz = "UTC") &
                datetime < as.POSIXct("2021-08-20 10:05:29", tz = "UTC") &
                stationSet %in% c("2021-BE_birkenfels")), 
       aes(x = datetime, y = SA)) +
  geom_point() +
  # scale_x_datetime(
  #   breaks = seq(from = as.POSIXct("2021-07-14 10:00:00", tz = "UTC"), 
  #                to = as.POSIXct("2021-07-15 10:00:00", tz = "UTC"), 
  #                by = "hour"),
  # labels = scales::date_format("%H")) +
  theme_minimal()

WBAT_testing <- WBAT_modeling %>% 
  filter(stationSet %in% c("2021-BE_birkenfels"))

WBAT_testing <- WBAT_testing %>% 
  mutate(datetime = ymd_hms(datetime))  # Use ymd_hms to parse the datetime

# Define the start and end datetime for your sequence
start_datetime <- min(WBAT_testing$datetime)  # Use the earliest datetime in your data
end_datetime <- max(WBAT_testing$datetime)    # Use the latest datetime in your data

# Create a complete sequence of datetime values (every hour)
complete_datetime <- tibble(
  datetime = seq(from = start_datetime, to = end_datetime, by = "hour"))

# Merge the complete datetime sequence with your existing data
full_data <- complete_datetime %>%
  full_join(WBAT_testing, by = "datetime") %>%  # Use full_join to include all datetimes
  mutate(SA = if_else(is.na(SA), 0, SA))

ggplot(subset(full_data, 
              datetime > as.POSIXct("2021-07-13 10:05:29", tz = "UTC") &
                datetime < as.POSIXct("2021-07-14 10:05:29", tz = "UTC") &
                stationSet %in% c("2021-BE_birkenfels")), 
       aes(x = datetime, y = SA)) +
  geom_point() +
  scale_x_datetime(
    breaks = seq(from = as.POSIXct("2021-07-13 10:00:00", tz = "UTC"), 
                 to = as.POSIXct("2021-07-14 10:00:00", tz = "UTC"), 
                 by = "hour"),
    labels = scales::date_format("%H")) +
  theme_minimal()

# Plotting SA normal
ggplot(full_data, aes(x = (SA))) + geom_histogram()

# Plotting SA in the log 
ggplot(full_data, aes(x = log10(SA))) + geom_histogram()

hist(full_data$SA)







# This used to be the improved heatmaps script and is now combined to one since it is not the main aim of the internship

############################################################################################
# Improving the plots based on input Jeroen
############################################################################################
# Loading data
WBAT_2021_BE_P1_belwind_70khz <- WBAT.all

# Working with the -60dB data
WBAT_2021_BE_P1_belwind_70khz_60freq <- WBAT_2021_BE_P1_belwind_70khz %>% 
  filter(treshold %in% c("-60")) %>% 
  arrange(datetime) %>%
  mutate(SA = SA + 0.00001,
         log10_SA = log10(SA),
         interval_number = dense_rank(datetime))

#######################
# This is not the correct script for the data wranling is a older one
#######################
# data <- WBAT_2021_BE_P1_belwind_70khz_60freq %>% 
#   filter(depth != 0) %>% 
#   group_by(interval_number) %>% 
#   mutate(quantile_25 = quantile(log10_SA, 0.25, na.rm = T),
#          log10_SA = ifelse(log10_SA < quantile_25, -5.0, log10_SA)) %>%
#   ungroup() %>% 
#   
#   summarise(SA_sum_filtered  = sum(SA, na.rm = T)) %>%
#   right_join(WBAT_2021_BE_P1_belwind_70khz_60freq, by = "interval_number") %>%
#   mutate(SA = ifelse(depth == 0.0, SA_sum_filtered , SA),
#          log10_SA = ifelse(depth != 0 & log10_SA < quantile_25, -5.0, log10_SA)) %>% 
#   select(-SA_sum_filtered, -quantile_25)

# This is the correct data wrangling try out
data <- WBAT_2021_BE_P1_belwind_70khz_60freq %>%
  group_by(interval_number) %>%
  mutate(quantile_25 = quantile(log10_SA[depth != 0], 0.75, na.rm = TRUE),    # Calculate the 25th percentile of log10_SA, ignoring NA values
         log10_SA = ifelse(log10_SA < quantile_25 & depth != 0, -5.0, log10_SA),    # Replace log10_SA values below the 25th percentile with -5.0
         SA_sum_filtered = sum(ifelse(log10_SA >= quantile_25 & depth != 0, SA, 0), na.rm = TRUE)) %>%    # Calculate the sum of SA (after filtering out low log10_SA values) for each interval_number
  ungroup() %>%
  mutate(SA = ifelse(depth == 0, SA_sum_filtered, SA)) %>%     # Replace SA at depth == 0.0 with SA_sum_filtered
  select(-SA_sum_filtered, -quantile_25) 

summary(Huh)
summary(Huh_75)

# This is the correct data 0.0 code try out 
data_depth_00 <- data %>% 
  filter(depth == 0) %>% 
  select(datetime, depth, SA, log10_SA, interval_number) %>% 
  mutate(log10_SA = log10(SA))

# This is a density plot to visu
ggplot(Huh_75, aes(x = log10_SA)) +
  geom_histogram(binwidth = .1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of SA Values at -60dB for 70kHz",
       x = "log10(SA)",
       y = "Frequency") +
  theme_minimal()


#######################
# This is the 'I want to try something new' plot 
ggplot(subset(Huh_75, 
              datetime > as.POSIXct("2021-07-13 10:05:29", tz = "UTC") & 
                datetime < as.POSIXct("2021-07-13 18:05:29", tz = "UTC")), 
       aes(x = interval_number, y = depth, fill = log10_SA)) +
  geom_tile() +
  geom_line(subset(data_depth_00,
                   datetime > as.POSIXct("2021-07-13 10:05:29", tz = "UTC") & 
                     datetime < as.POSIXct("2021-07-13 18:05:29", tz = "UTC")),
            mapping = aes(y = (log10_SA*5)), color = "purple", size = 1) +
  scale_fill_gradientn(colors = c("white", "lightblue", "red"),
                       values = c(0, 0.6, 1),
                       limits = c(-5, 5),
                       name = "log10(SA)") +
  labs(title = "SA Values by Depth and Time", x = "Time", y = "Water depth (m)") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 5, 
                                         name = "Total log10(SA) Values (Purple line)")) +
  theme_minimal() 
# theme(panel.grid = element_blank())


################################################################################
# Function for the new plots
################################################################################

# Define the function to plot the heatmap for a limited number of intervals (e.g., 3 iterations)
format_SA_heatmaps_manual <- function(data, gap_interval = 10, num_iterations = 3, frequency, threshold, data_drop) {
  
  data <- data %>% 
    filter(treshold %in% c(threshold)) %>% 
    arrange(datetime) %>%
    mutate(SA = SA + 0.00001,
           log10_SA = log10(SA),
           interval_number = dense_rank(datetime)) %>% 
    arrange(interval_number) %>% 
    mutate(interval_number_with_gap = ifelse((interval_number %% gap_interval) == 0, NA, interval_number))
  
  # Ensure that data is ordered by interval_number (or datetime if needed)
  
  data <- data %>%
    group_by(interval_number) %>%
    mutate(quantile_25 = quantile(log10_SA[depth != 0], data_drop, na.rm = TRUE),    # Calculate the 25th percentile of log10_SA, ignoring NA values
           log10_SA = ifelse(log10_SA < quantile_25 & depth != 0, -5.0, log10_SA),    # Replace log10_SA values below the 25th percentile with -5.0
           SA_sum_filtered = sum(ifelse(log10_SA >= quantile_25 & depth != 0, SA, 0), na.rm = TRUE)) %>%    # Calculate the sum of SA (after filtering out low log10_SA values) for each interval_number
    ungroup() %>%
    mutate(SA = ifelse(depth == 0, SA_sum_filtered, SA)) %>%     # Replace SA at depth == 0.0 with SA_sum_filtered
    select(-SA_sum_filtered, -quantile_25) 
  
  data_depth_00 <- data %>%
    filter(depth == 0) %>%
    select(datetime, depth, SA, log10_SA, interval_number) %>%
    mutate(log10_SA = log10(SA))
  
  # Get the total number of interval_numbers
  total_intervals <- max(data$interval_number, na.rm = TRUE)
  chunk_size <- 240
  num_iterations <- min(num_iterations, ceiling(total_intervals / chunk_size))
  
  for (i in 1:num_iterations) {
    # Calculate the start and end interval numbers for the current chunk (240 intervals)
    start_interval <- (i - 1) * chunk_size + 1
    end_interval <- min(i * chunk_size, total_intervals)
    data_chunk <- data %>% 
      filter(interval_number >= start_interval & interval_number <= end_interval)
    
    if (nrow(data_chunk) == 0) {                                                
      message("No data left to process, stopping the loop.")
      break
    }
    
    # Create the heatmap plot for the current chunk
    plot <- ggplot(data_chunk, aes(x = interval_number_with_gap, y = depth, fill = log10_SA)) +
      geom_tile() +
      geom_line(data = data_depth_00 %>%
                  filter(interval_number >= start_interval & interval_number <= end_interval),
                mapping = aes(x = interval_number, y = (log10_SA*5)), color = "purple", size = 1) +
      scale_fill_gradientn(colors = c("lightblue", "lightblue", "red"),
                           values = c(0, 0.6, 1),
                           limits = c(-5, 5),
                           name = "log10(SA)") +
      labs(
        title = paste0("log10(SA) at ", 
                       # idxDataSet, " ", 
                       frequency, "kHz for ", threshold, "dB with datadrop of ", data_drop, "% - ", start_interval, " to ", end_interval),
        x = "Time in interval Number",
        y = "Water depth (m)") +
      scale_y_continuous(sec.axis = sec_axis(~ . / 5, 
                                             name = "Total log10(SA) Values (Purple line)")) +
      theme_minimal() +
      theme(
        # panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)  # Optional: tilt x-axis labels
      )
    
    # Save the plot as a PNG file with the correct date and interval range in the filename
    output_folder <- file.path(resultPath, "SA_Heatmaps_test")
    if(!dir.exists(output_folder)) dir.create(output_folder)
    filename <- paste0(output_folder, "/SA_Heatmap_", 
                       # idxDataSet, "_", 
                       threshold, "dB_datadrop_", data_drop, "%_", start_interval, "_to_", end_interval, ".png")
    ggsave(filename, plot = plot, width = 10, height = 6)
  }
}

# Example usage with WBAT.2021_BE_belwind_70kHz_test_tres50 dataset, limiting to 3 iterations
format_SA_heatmaps_manual(WBAT_2021_BE_P1_belwind_70khz, gap_interval = 10, num_iterations = 3, frequency = 70, threshold = -50, data_drop = .0)



#################################################################################
# For Loop for all data files - preperation 
#################################################################################
# Define the values
threshold <- c(-50, -60)
data_drop <- c(.0, .25, .50, .75)

# Define the subfolder name
subfolder <- "sub"

# Initialize progress bar
total_iterations <- length(unique(WBAT.tab$surveName)) * length(threshold) * length(data_drop)
pb <- progress_bar$new(
  format = "  Processing [:bar] :percent | Elapsed: :elapsed | Remaining: :eta",
  total = total_iterations,
  clear = FALSE,
  width = 100)

#################################################################################
# For Loop for all data files - the actual loop
#################################################################################

for (z in unique(WBAT.tab$surveName)) {                                         # Iterate over each unique survey name                          
  tab.filt <- WBAT.tab[WBAT.tab$surveName == z, ]                               # Filter the table for the current survey name
  
  for (idxDataSet in tab.filt$surveName) {                                      # Iterate over each dataset in the filtered table                                    
    data_file <- file.path(dataPath, subfolder, paste0('WBAT_', idxDataSet, '.RData'))  # Construct the path to the subfolder containing the dataset
    
    if (file.exists(data_file)) {                                               # Check if the file exists and load it
      load(data_file)
    } else {
      message("File not found: ", data_file)
      next                                                                      # Skip to the next dataset if file doesn't exist
    }
    
    freq_match <- stringr::str_extract(idxDataSet, "(?i)(\\d+)(?=khz$)")        # Match digits ending in 'khz', ignoring case
    if (is.na(freq_match)) {
      message("Frequency not found in surveName: ", idxDataSet)                 # Extract frequency from the dataset name
      next                                                                      # Skip this entry if frequency cannot be found
    }
    frequency <- as.numeric(freq_match)                                         # Convert "70kHz" to 70
    
    
    for (j in threshold) {                                                      # Iterate over threshold and data_drop
      for (k in data_drop) {
        tryCatch({
          format_SA_heatmaps(
            WBAT.all, 
            gap_interval = 10, 
            num_iterations = 3, 
            frequency = frequency,                                              # Pass extracted frequency
            threshold = j,                                                       # Pass the current threshold
            data_drop = k                                                       # Pass the current data_drop
          )
        }, error = function(e) {
          message("Error processing dataset ", idxDataSet, " with threshold ", j, " and data_drop ", k, ": ", e$message)
        })
        
        pb$tick()                                                               # Update progress bar
      }
    }
  }
}




