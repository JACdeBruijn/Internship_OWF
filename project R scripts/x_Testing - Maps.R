### Loading packages ###
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, leaflet, sf)

rm(list=ls())

### Loading data ####
figurePath    <- file.path('.','figures')      
dataPath      <- file.path('.','data')
resultPath    <- file.path('.','results')

locations_GIT <- read_csv("Data/data_overview.csv")                # Location data from github deposit
locations_ETN <- read_csv("Data/Locations_ETN_database/ETN-data_From_JH.csv")     # Location data from ETN deposit

shp <- st_read(file.path(dataPath, "Map_files/EMODnet_HA_WindFarms_20220324/EMODnet_HA_WindFarms_pg_20220324.shp"))
turbine.NL <- st_read(file.path(dataPath, "Map_files/EMODnet_HA_WindFarms_20220324/turbines.shp"))
turbine.BE <- st_read(file.path(dataPath, "Map_files/EMODnet_HA_WindFarms_20220324/WTG.shp"))

### Wrangling data ####
locations_GIT_filtered <- locations_GIT %>% 
  filter(pairing > 0) %>% 
  mutate(station = ifelse(row_number() %in% c(9, 10, 11, 12), substr(station, 8, nchar(station)), station)) %>% 
  mutate(ID = as.factor(paste0(dataSet, "_", station))) %>% 
  mutate(number = as.factor(row_number())) %>% 
  rename(lat_GIT = lat, lon_GIT = lon) %>%                                              # Rename coordinates 
  mutate(lat_GIT = as.numeric(lat_GIT))

head(locations_GIT_filtered)
str(locations_GIT_filtered)

locations_ETN_filtered <- locations_ETN %>% 
  filter(stationName %in% c("bpns-AP_CW", "bpns-Grafton", "bpns-AP_BW", "bpns-Birkenfels",      # 2021 stations
                        "AP_CPower-2", "AP_Belwind", "AP_Gardencity", "AP_Grafton",             # 2023 stations 
                        "dpns-Borssele1", "dpns-Borssele2", "dpns-Borssele3", "dpns-Borssele4", # 2023 station Borssele
                        "Pelfish_Grafton", "PelFish_CPower-2")) %>%                             # 2024 stations
  select(c(stationName, deployDateTime, recoverDateTime, deployLat, deployLong)) %>%            # Only keeping relevant columns
  mutate(dataSet = case_when(                                                                   # Giving stations correct data set name
    stationName %in% c("bpns-AP_CW", "bpns-Grafton", "bpns-AP_BW", "bpns-Birkenfels") ~ "2021-BE",
    stationName %in% c("AP_CPower-2", "AP_Belwind", "AP_Gardencity", "AP_Grafton") ~ "2023-BE",
    stationName %in% c("dpns-Borssele1", "dpns-Borssele2", "dpns-Borssele3", "dpns-Borssele4") ~ "2023-BSW",
    stationName %in% c("Pelfish_Grafton", "PelFish_CPower-2") ~ "2024-BE")) %>% 
  mutate(station = case_when(                                                                   # Giving stations correct station name
    stationName %in% c("bpns-AP_BW", "AP_Belwind") ~ "belwind",
    stationName %in% c("bpns-Birkenfels") ~ "birkenfels",
    stationName %in% c("dpns-Borssele1") ~ "BSW1",
    stationName %in% c("dpns-Borssele2") ~ "BSW2",
    stationName %in% c("dpns-Borssele3") ~ "BSW3",
    stationName %in% c("dpns-Borssele4") ~ "BSW4",
    stationName %in% c("bpns-AP_CW", "AP_CPower-2", "PelFish_CPower-2") ~ "cpower",
    stationName %in% c("AP_Gardencity") ~ "gardencity",
    stationName %in% c("bpns-Grafton", "AP_Grafton", "Pelfish_Grafton") ~ "grafton")) %>% 
  mutate(ID = as.factor(paste0(dataSet, "_", station))) %>%                                     # Create same ID as other data set 
  rename(lat_ETN = deployLat, lon_ETN = deployLong)                                             # Rename coordinates 

head(locations_ETN_filtered)

### Comparing the coordinates from the two data sets ####
R <- 6371000      # Radius of the earth in meters

locations_compared <- locations_GIT_filtered %>% 
  left_join(locations_ETN_filtered, by = "ID") %>% 
  select(ID, lat_GIT, lon_GIT, lat_ETN, lon_ETN) %>% 
  mutate(lat_diff = abs(lat_GIT - lat_ETN),
         lon_diff = abs(lon_GIT - lon_ETN)) %>% 
  mutate(lat_meters = lat_diff * (R * pi / 180),                                                # Convert lat difference to meters
         lon_meters = lon_diff * (R * pi / 180 * cos(lat_GIT * pi / 180))) %>%                  # Convert lon difference to meters
  mutate(total_distance = sqrt(lat_meters^2 + lon_meters^2)) %>%                                # Total distance in meters
  mutate(lat_meters = round(lat_meters, 2),                                                     # Round latitude difference to 2 decimal places
         lon_meters = round(lon_meters, 2),                                                     # Round longitude difference to 2 decimal places
         total_distance = round(total_distance, 2))                                             # Round total distance to 2 decimal places

view(locations_compared)

# Correcting the coordinated in GIT data set based on the ETN data set

dataoverview_updated <- locations_GIT_filtered %>%
  left_join(locations_ETN_filtered %>% select(ID, lat_ETN, lon_ETN), by = "ID") %>%
  mutate(lat = if_else(lat_GIT != lat_ETN, lat_ETN, lat_GIT),
         lon = if_else(lon_GIT != lon_ETN, lon_ETN, lon_GIT)) %>%
  select(-lat_ETN, -lon_ETN, -lat_GIT, -lon_GIT, -number) %>% 
  select(ID, year, dataSet, station, freq, date_start, date_end,
         lat, lon, everything())

# The corninated have been corrected in the data_overview data set and GIT is now leading - code is not needed anymore
# write.csv(dataoverview_updated, file = file.path(resultPath, "data_overview2.csv"))             # The 

# Filter on only the different data
only_diff_GIT <- locations_GIT_filtered %>% 
  filter(ID %in% c("2021-BE_birkenfels", "2023-BE_grafton", 
                   "2021-BE_belwind", "2021-BE_cpower", 
                   "2024-BE_cpower", "2023-BE_gardencity"))

only_diff_ETN <- locations_ETN_filtered %>% 
  filter(ID %in% c("2021-BE_birkenfels", "2023-BE_grafton", 
                   "2021-BE_belwind", "2021-BE_cpower", 
                   "2024-BE_cpower", "2023-BE_gardencity"))

### sf package - Mapping the deployments in the wind farms ####
sf_data_GIT <- st_as_sf(locations_GIT_filtered, coords = c("lon_GIT", "lat_GIT"), crs = 4326)
sf_data_ETN <- st_as_sf(locations_ETN_filtered, coords = c("lon_ETN", "lat_ETN"), crs = 4326)
sf_only_diff_GIT <- st_as_sf(only_diff_GIT, coords = c("lon_GIT", "lat_GIT"), crs = 4326)
sf_only_diff_ETN <- st_as_sf(only_diff_ETN, coords = c("lon_ETN", "lat_ETN"), crs = 4326)

Shapefile_netherlands <- st_read("Data/Map_files/Netherlands_shapefile/nl_1km.shp")
Shapefile_OWF <- st_read("Data/Map_files/OWF_polygons/windfarmspolyPolygon.shp")

Crop_bbox <- st_bbox(c(xmin = 2.2, ymin = 51.3, xmax = 3.5, ymax = 51.9), crs = st_crs(Shapefile_netherlands))
Cropped_Netherlands_shp <- st_crop(Shapefile_netherlands, Crop_bbox)

Crop_bbox_OWF <- st_bbox(c(xmin = 2.2, ymin = 51.3, xmax = 3.5, ymax = 51.9), crs = st_crs(Shapefile_OWF))
Cropped_OWF_shp <- st_crop(Shapefile_OWF, Crop_bbox_OWF)

ggplot() +
  geom_sf(data = Cropped_Netherlands_shp, fill = "lightblue", color = "black") +
  geom_sf(data = Cropped_OWF_shp, aes(), size = 2) +
  # geom_sf(data = sf_data_ETN, aes(color = "red"), size = 5) +                           # For normal map
  # geom_sf(data = sf_data_GIT, aes(color = "GIT"), size = 5) +                         # For comparison between GIT and ETN map
  geom_sf(data = sf_data_ETN, aes(color = "ETN"), size = 5) +                         # For comparison between GIT and ETN map
  # geom_sf(data = sf_only_diff_ETN, aes(color = "diff_ETN"), size = 5) +               # Only differences between GIT and ETN
  # geom_sf(data = sf_only_diff_GIT, aes(color = "diff_GIT"), size = 5) +               # Only differences between GIT and ETN
  # geom_sf_text(data = sf_only_diff_ETN, aes(label = ID), nudge_y = 0, color = "black") +
  # geom_sf_text(data = sf_only_diff_GIT, aes(label = ID), nudge_y = 0, color = "black") +
  labs(title = "Map of experimental area", x = "Longitude", y = "Latitude") +
  theme_minimal()

### sf package - Mapping the turbines in the wind farms ####

sf_turbine.NL <- turbine.NL %>% 
  st_as_sf(coords = c("utm_x", "utm_y"), crs = 32631) %>% 
  st_transform(crs = 4326)
  # { bind_cols(st_drop_geometry(.), st_coordinates(.)) } %>% 
  # rename(lon = X, lat = Y)

ggplot() +
  # geom_sf(data = Shapefile_netherlands, fill = "lightblue", color = "black") +
  geom_sf(data = Cropped_OWF_shp, aes(), size = 2) +
  geom_sf(data = cropped_sf_turbine.BE, aes(color = "red"), size = 1) +
  geom_sf(data = cropped_sf_turbine.NL, aes(color = "blue"), size = 1) +
  geom_sf(data = sf_data_ETN, aes(color = "ETN"), size = 5)


sf_turbine.BE <- turbine.BE

# Define the bounding box coordinates
xmin <- 2.2
ymin <- 51.3
xmax <- 3.5
ymax <- 51.9

# Create the polygon (bounding box)
bounding_box <- st_polygon(list(rbind(
  c(xmin, ymin),  # Lower left corner
  c(xmax, ymin),  # Lower right corner
  c(xmax, ymax),  # Upper right corner
  c(xmin, ymax),  # Upper left corner
  c(xmin, ymin)   # Close the polygon
)))

# Convert to sf object and set the CRS (EPSG: 4326 for WGS84 latitude/longitude)
bounding_box_sf <- st_sfc(bounding_box, crs = 4326)

# Check the bounding box geometry
print(bounding_box_sf)

cropped_sf_turbine.BE <- sf_turbine.BE[st_within(sf_turbine.BE, bounding_box_sf, sparse = FALSE), ]
cropped_sf_turbine.NL <- sf_turbine.NL[st_within(sf_turbine.NL, bounding_box_sf, sparse = FALSE), ]


# Check the CRS of your spatial data
st_crs(sf_turbine.BE)

# Check the CRS of the bounding box
st_crs(bounding_box_sf)


sf_turbine.BE <- st_transform(sf_turbine.BE, crs = st_crs(bounding_box_sf))


Wreck_data <- tibble(
  wreckname = c("BSW1", "BSW2"),
  NCN = c("2404", "189"),
  diepte = c("25.9", "19.2"),
  lat = c("51.83984", "51.70465"),
  lon = c("3.23594", "3.24060"),
  length = c("34", "67"), 
  width = c("9", "14"),
  highed = c("5", ""),
  boeg_deg = c("216", "131")) %>% 
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon))

str(Wreck_data)


### Working with the dates ####
Correct_dates <- locations_GIT %>% 
  filter(pairing > 0) %>%
  mutate(station = ifelse(row_number() %in% c(9, 10, 11, 12), substr(station, 8, nchar(station)), station)) %>% 
  mutate(ID = as.factor(paste0(dataSet, "_", station)))

q <- Correct_dates %>%
  mutate(
    date_start = as.Date(mdy(date_start)),  # Convert from MM/DD/YYYY to Date format
    date_end = as.Date(mdy(date_end)))       # Convert from MM/DD/YYYY to Date format

str(q)
# view(q)

deploy1 <-
  ggplot(q, aes(y = as.factor(station))) + 
  geom_segment(aes(x = date_start, xend = date_end, y = station, yend = station), color = "blue", size = 4) +
  geom_point(aes(x = date_start, y = station), color = "green", size = 6) +  # Mark the start date
  geom_point(aes(x = date_end, y = station), color = "red", size = 6) +      # Mark the end date
  labs(title = "Measurement timeline for each station",
       x = "Date", 
       y = "Stations") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 20, color = "black"), 
    axis.text.x = element_text(size = 20, color = "black", angle = 45, hjust = 1),
    axis.title = element_text(size = 30),
    title = element_text(size = 30))


ggsave(filename = file.path(figurePath, "deploy periods.png"), plot = deploy1, width = 15, height = 10)

