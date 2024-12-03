################################################################################
# Work around the malfunctioning IDinter
# From the RAW data to the seperate WBAT.files the IDinter is stopping at 200 and resetting to 1
# This is the solution
################################################################################
# Load in WBAT data by hand -----

rm(list=ls())

# Filter on treshold
WBAT.all <- WBAT.all %>% 
  filter(treshold == -50)

# Now gives multiple dates
print(n = "inf", WBAT.all %>%
        filter(IDinter == 1) %>%
        distinct(datetime) %>% 
        arrange(datetime))

# Now gives a zig-zag pattern
ggplot(WBAT.all, aes(x = datetime, y = IDinter)) +
  geom_line()

################################################################################
# This should correct for the mistake
WBAT.all_corrected <- WBAT.all %>%
  arrange(datetime) %>%                                    # Ensure data is sorted by datetime
  mutate(reset_flag = if_else(IDinter == 1 & lag(IDinter, default = 1) == 200, 1, 0), # Detect reset point
         reset_group = cumsum(reset_flag),                      # Increment group counter for each reset
         IDinter = IDinter + (reset_group * 200)) %>%      # Adjust IDinter by adding multiples of 200
  select(-reset_flag, -reset_group) 

# Control - should give one date 
print(n = "inf", WBAT.all_corrected %>%
        filter(IDinter == 1) %>%
        distinct(datetime) %>% 
        arrange(datetime))

# Control - should be a stright line 
ggplot(WBAT.all_corrected, aes(x = datetime, y = IDinter)) +
  geom_line()

################################################################################
# This new IDinter must be incorporated in the WBAT.all data sets 






