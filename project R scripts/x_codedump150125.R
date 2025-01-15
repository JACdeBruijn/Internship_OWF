# Code dump for figuring out why WBAT and CPOD joining gived 201 NA values
sapply(df_joined_WBAT_CPOD, function(x) length(unique(x))) 
sapply(df_joined_WBAT_CPOD, function(x) sum(is.na(x))) 


unique(df.join.all$type)
sum(is.na(df.join.all$type))
Na_rows <- df.join.all %>% filter(is.na(type))

rows_pph_NA <- df.join.all %>% 
  filter(is.na(pph)) %>% 
  mutate(dataSet_station = str_c(dataSet, '_', station)) %>% 
  select(dataSet_station, everything())


data_split_by_stationSet <- split(rows_pph_NA, rows_pph_NA$dataSet_station)
row_counts <- sapply(data_split_by_stationSet, nrow)

ggplot(rows_pph_NA, aes(x = day_ToD, y = log10(SA))) +
  geom_point() +
  facet_wrap(~ dataSet_station)

ggplot(subset(rows_pph_NA, dataSet_station == "2021-BE_belwind"), aes(x = day_ToD, y = log10(SA))) +
  geom_point() +
  facet_wrap(~ dataSet_station)


sapply(rows_pph_NA, function(x) length(unique(x))) 
sapply(rows_pph_NA, function(x) sum(is.na(x))) 


testing_na <- Na_rows %>% 
  filter(dataSet == "2021-BE",
         station == "grafton")

testing <- CPOD_sample %>% 
  filter(dataSet == "2021-BE",
         station == "grafton")

common_points <- testing %>%
  semi_join(testing_na, by = "day_ToD")


testing_na <- Na_rows %>% 
  filter(dataSet == "2021-BE",
         station == "grafton")

testing <- CPOD_sample %>% 
  filter(dataSet == "2021-BE",
         station == "grafton")

common_points <- CPOD_sample %>%
  semi_join(Na_rows, by = "day_ToD")

common_points %>% 
  filter(dataSet == "2021-BE",
         station == "grafton")










data_split_by_stationSet <- split(CPOD_all_hour, CPOD_all_hour$stationSet)
row_counts <- sapply(data_split_by_stationSet, nrow)


time_differences <- sapply(data_split_by_stationSet, function(df) {
  # Get the first and last time_hour for the data frame
  first_time <- min(df$time_hour, na.rm = TRUE)
  last_time <- max(df$time_hour, na.rm = TRUE)
  
  # Calculate the difference in hours
  diff_in_hours <- as.numeric(difftime(last_time, first_time, units = "hours"))
  
  return(diff_in_hours)
})

missing_rows <- time_differences - row_counts


sum(missing_rows)






# Step 1: Sort the data by time_hour and convert to POSIXct
sorted_data <- lapply(data_split_by_stationSet, function(df) {
  df %>%
    mutate(time_hour = as.POSIXct(time_hour)) %>% # Ensure time_hour is POSIXct
    arrange(time_hour) %>%
    select(time_hour, everything()) # Keep all other columns in the dataframe
})

# Step 2: Define a function to find and extract missing rows for each split
find_missing_rows <- function(df) {
  # Ensure time_hour is POSIXct
  df <- df %>% mutate(time_hour = as.POSIXct(time_hour))
  
  # Create a sequence of all expected time intervals between the first and last time_hour
  time_sequence <- seq(from = min(df$time_hour), to = max(df$time_hour), by = "hour")
  
  # Find the missing time intervals (those not in the actual data)
  missing_times <- setdiff(time_sequence, df$time_hour)
  
  # Create a data frame for the missing rows
  missing_rows <- data.frame(time_hour = missing_times)
  
  # Merge the missing time intervals back with the full data to get the missing rows
  missing_rows_full <- merge(missing_rows, df, by = "time_hour", all.x = TRUE)
  
  return(missing_rows_full)
}

# Step 3: Apply the find_missing_rows function to each subset
missing_rows_list <- lapply(sorted_data, find_missing_rows)

# Step 4: Combine all the missing rows from all station sets into one data frame
missing_rows_combined <- bind_rows(missing_rows_list)

# Step 5: View the missing rows in the Viewer
View(missing_rows_combined)
