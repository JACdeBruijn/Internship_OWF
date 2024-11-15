################################################################################
# Function for the heatmap plots - 15/11/2024
################################################################################

# Define the function to plot the heatmap for a limited number of intervals (e.g., 3 iterations)
format_SA_heatmaps <- function(data, gap_interval = 10, num_iterations = 3, frequency, threshold, data_drop) {
  
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
        title = paste0("log10(SA) at ", idxDataSet, " ", "kHz for ", threshold, "dB with datadrop of ", data_drop, "% - ", start_interval, " to ", end_interval),
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
    output_folder <- file.path(resultPath, "SA_Heatmaps_auto_test")
    if(!dir.exists(output_folder)) dir.create(output_folder)
    filename <- paste0(output_folder, "/SA_Heatmap_", idxDataSet, "_", threshold, "dB_datadrop_", data_drop, "%_", start_interval, "_to_", end_interval, ".png")
    ggsave(filename, plot = plot, width = 10, height = 6)
  }
}
