if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, progress)

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

################################################################################
# Loading in the overview data sets
################################################################################
WBAT.tab <- read.csv(file.path(dataPath,'survey_db.csv'))                 
WBAT.tab <- WBAT.tab[,c(1:5,8,9)]
WBAT.tab$dataSet_station <- paste0(WBAT.tab$dataSet,'_',WBAT.tab$station)
WBAT.tab <- WBAT.tab %>% 
  filter(!(dataSet %in% c("2022-cpower", "2022-HKZ", "2023-HKN")))

overview.tab <- read.csv(file.path(dataPath,'data_overview.csv'))               # Creating overview.tab from data_overview.csv
overview.tab$stationSet <- paste0(overview.tab$dataSet,'_',overview.tab$station)
overview.tab$pairingName <- paste0(overview.tab$dataSet,'_',overview.tab$pairing)
overview.tab <- overview.tab %>% 
  select(-c('year','dataSet','station')) %>%
  select(stationSet, everything()) %>% 
  filter(pairing != "-1")


############################################################################################
# Beginning steps of script

# This works to load all the data files 
############################################################################################

# Have included this later in the script in a for loop
# for(i in unique(WBAT.tab$surveName)){                              # Loop that will do everything for each unique dataSet_station that does also occur in overview.tab
#   tab.filt <- WBAT.tab[WBAT.tab$surveName == i,]
#   
#   for(idxDataSet in tab.filt$surveName){                                          # Reading all the data on station and data set for pelegc fish data
#     load(file.path(dataPath, paste0('WBAT_',idxDataSet,'.RData')))
#     
#   }
# }

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




