if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)

rm(list=ls())

sourceDir <- function(directory) {
  files <- list.files(directory, pattern = "\\.R$", full.names = TRUE)
  for (file in files) {
    source(file)
  }
}

sourceDir(file.path('.','Function'))
figurePath    <- file.path('.','Figures')
dataPath      <- file.path('.','Data')
resultPath    <- file.path('.','Results')

load(file = file.path(resultPath,'CPOD_WBAT_workspace.RData'))

### Working with data

data_filtered <- WBAT.all.summary %>%
  filter(stationSet == "2021-BE_grafton")

data_filtered$time <- as.numeric(format(data_filtered$datetime, "%H"))
WBAT.all.summary$time <- as.numeric(format(WBAT.all.summary$datetime, "%H"))

WBAT.all_filtered <- WBAT.all.summary %>% 
  filter(!dataSet %in% c("2022-cpower", "2022-HKZ", "2023-HKN"))
  
  # filter(!stationSet %in% c("2023-HKN_267838", "2023-HKN_274174", 
  #                           "2023-HKN_278093", "2023-HKN_267814", 
  #                           "2022-HKZ_278093", "2022-HKZ_274174",
  #                           "2022-cpower_267814", "2022-cpower_274174",
  #                           "2022-cpower_278093", "2022-cpower_267838",
  #                           "2022-HKZ_267814", "2022-HKZ_267838"))

########################################################################
# figuring out what the phase 1 and phase 2 are
########################################################################

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


ggplot(WBAT.all_filtered, aes(x = as.factor(time), y = log10(SA))) +
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


ggplot(data_filtered, aes(x = datetime, y = log10(SA), color = dayNight)) +
  geom_line(size = 1) +  # Line plot to show SA over time
  geom_point(size = 2) +  # Optional: add points to highlight individual hourly values
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





