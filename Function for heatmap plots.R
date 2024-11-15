################################################################################
# Script for plotting heatmaps of depth over time
# For 1 dataset, yet all variables such as threshold, freq, 4 levels of data drop
################################################################################

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

################################################################################
# Loading in the correct WBAT data sets
################################################################################

for(stationSet in unique(WBAT.tab$surveName)){                              # Loop that will do everything for each unique dataSet_station that does also occur in overview.tab
  print(stationSet)
  tab.filt <- WBAT.tab[WBAT.tab$dataSet_station == stationSet,]
  
  for(idxDataSet in tab.filt$surveName){                                          # Reading all the data on station and data set for pelegc fish data
    print(idxDataSet)
    load(file.path(dataPath,paste0('WBAT_',idxDataSet,'.RData')))
    
  }
}










  
################################################################################
# This is for later use
  if(flagFirst){
    df.join.all <- df.join
    flagFirst <- F
  }else{
    df.join.all <- rbind(df.join.all,df.join)
  }








ggplot(subset(data, 
              datetime > as.POSIXct("2021-07-13 10:05:29", tz = "UTC") & 
                datetime < as.POSIXct("2021-07-13 18:05:29", tz = "UTC")), 
       aes(x = interval_number, y = depth, fill = log10_SA)) +
  geom_tile() +
  geom_line(subset(data_depth_00,
                   datetime > as.POSIXct("2021-07-13 10:05:29", tz = "UTC") & 
                     datetime < as.POSIXct("2021-07-13 18:05:29", tz = "UTC")),
            mapping = aes(y = (log10_SA*5)), color = "purple", size = 1) +
  scale_fill_gradientn(colors = c("lightblue", "lightblue", "red"),
                       values = c(0, 0.6, 1),
                       limits = c(-5, 5),
                       name = "log10(SA)") +
  labs(title = "SA Values by Depth and Time", x = "Time", y = "Water depth (m)") +
  scale_y_continuous(sec.axis = sec_axis(~ . / 5, 
                                         name = "Total log10(SA) Values (Purple line)")) +
  theme_minimal() 

plot_intervals_in_chunks <- function(data, gap_interval = 10, num_iterations = 3, frequency, treshold, data_drop) {
  # Preprocess the data
  data <- data %>%
    filter(treshold %in% c(treshold)) %>%
    arrange(datetime) %>%
    mutate(
      SA = SA + 0.00001,
      log10_SA = log10(SA),
      interval_number = dense_rank(datetime)
    ) %>%
    arrange(interval_number) %>%
    mutate(interval_number_with_gap = ifelse((interval_number %% gap_interval) == 0, NA, interval_number))
  
  # Filter and calculate the quantile logic
  data <- data %>%
    group_by(interval_number) %>%
    mutate(
      quantile_25 = quantile(log10_SA[depth != 0], data_drop, na.rm = TRUE),
      log10_SA = ifelse(log10_SA < quantile_25 & depth != 0, -5.0, log10_SA),
      SA_sum_filtered = sum(ifelse(log10_SA >= quantile_25 & depth != 0, SA, 0), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(SA = ifelse(depth == 0, SA_sum_filtered, SA)) %>%
    select(-SA_sum_filtered, -quantile_25)
  
  # Separate data for depth = 0
  data_depth_00 <- data %>%
    filter(depth == 0) %>%
    select(datetime, depth, SA, log10_SA, interval_number) %>%
    mutate(log10_SA = log10(SA))
  
  # Define chunk size and iterations
  total_intervals <- max(data$interval_number, na.rm = TRUE)
  chunk_size <- 240
  num_iterations <- min(num_iterations, ceiling(total_intervals / chunk_size))
  
  for (i in 1:num_iterations) {
    # Subset data for the current chunk
    start_interval <- (i - 1) * chunk_size + 1
    end_interval <- min(i * chunk_size, total_intervals)
    data_chunk <- data %>%
      filter(interval_number >= start_interval & interval_number <= end_interval)
    
    if (nrow(data_chunk) == 0) {
      message("No data left to process, stopping the loop.")
      break
    }
    
    # Create the heatmap plot
    plot <- ggplot(data_chunk, aes(x = interval_number_with_gap, y = depth, fill = log10_SA)) +
      geom_tile() +
      geom_line(aes(y = depthIntegration), color = "red", size = .5) +
      geom_line(
        data = data_depth_00 %>%
          filter(interval_number >= start_interval & interval_number <= end_interval),
        aes(x = interval_number, y = log10_SA), 
        color = "purple", size = 1
      ) +
      scale_fill_gradientn(
        colors = c("lightblue", "lightblue", "red"),
        values = c(0, 0.6, 1),
        limits = c(-5, 5),
        name = "log10(SA)"
      ) +
      labs(
        title = paste0("log10(SA) at ", frequency, "kHz for ", treshold, "dB with datadrop of ", data_drop, "% - ", start_interval, " to ", end_interval),
        x = "Time in Interval Number",
        y = "Water Depth (m)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Save the plot
    output_folder <- file.path(resultPath, "SA_Heatmaps")
    if (!dir.exists(output_folder)) dir.create(output_folder)
    filename <- paste0(output_folder, "/SA_Heatmap_", frequency, "kHz_", treshold, "dB_datadrop_", data_drop, "%_", start_interval, "_to_", end_interval, ".png")
    ggsave(filename, plot = plot, width = 10, height = 6)
  }
}










