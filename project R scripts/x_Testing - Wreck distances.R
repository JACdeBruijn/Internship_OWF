################################################################################
# Script for testing the current flow / water speed - 11/12/24
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, RColorBrewer, icesTAF, see, leaflet, sf, geosphere)

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

################################################################################
current <- read_csv(file.path(dataPath, "avrcur_TS_09d9_e042_b77d.csv"))

str(current)     
   

station_summary <- current %>% 
  group_by(station_id) %>% 
  summarise(lon = first(longitude),
            lat = first(latitude),
            min_speed = min(avrcur, na.rm = T),
            max_speed = max(avrcur, na.rm = T))

stations_sf <- st_as_sf(station_summary, coords = c("lon", "lat"), crs = 4326)


Shapefile_netherlands <- st_read("Data/Map_files/Netherlands_shapefile/nl_1km.shp")
Shapefile_OWF <- st_read("Data/Map_files/OWF_polygons/windfarmspolyPolygon.shp")

Crop_bbox <- st_bbox(c(xmin = 2.2, ymin = 51.3, xmax = 3.5, ymax = 51.9), crs = st_crs(Shapefile_netherlands))
Cropped_Netherlands_shp <- st_crop(Shapefile_netherlands, Crop_bbox)

Crop_bbox_OWF <- st_bbox(c(xmin = 2.2, ymin = 51.3, xmax = 3.5, ymax = 51.9), crs = st_crs(Shapefile_OWF))
Cropped_OWF_shp <- st_crop(Shapefile_OWF, Crop_bbox_OWF)


locations_GIT <- read_csv("Data/data_overview.csv")
locations_GIT_filtered <- locations_GIT %>% 
  filter(pairing > 0) %>% 
  mutate(station = ifelse(row_number() %in% c(9, 10, 11, 12), substr(station, 8, nchar(station)), station)) %>% 
  mutate(ID = as.factor(paste0(dataSet, "_", station))) %>% 
  mutate(number = as.factor(row_number())) %>% 
  rename(lat_GIT = lat, lon_GIT = lon) %>%                                              # Rename coordinates 
  mutate(lat_GIT = as.numeric(lat_GIT))
sf_data_GIT <- st_as_sf(locations_GIT_filtered, coords = c("lon_GIT", "lat_GIT"), crs = 4326)


ggplot() +
  # geom_sf(data = Cropped_Netherlands_shp, fill = "lightblue", color = "black") +
  geom_sf(data = Cropped_OWF_shp, aes(), size = 2) +
  geom_sf(data = cropped_sf_turbine.BE, aes(color = "red"), size = 1) +
  geom_sf(data = cropped_sf_turbine.NL, aes(color = "blue"), size = 1) +
  geom_sf(data = sf_data_GIT, aes(color = "GIT"), size = 5) + 
  geom_sf(data = sf_data_wreck, aes(color = "wreckname"), size = 5) + 
  
  
  geom_sf(data = stations_sf, aes(color = station_id), size = 3) +
  labs(title = "Map of experimental area", x = "Longitude", y = "Latitude") +
  theme_minimal()


################################################################################
shp <- st_read(file.path(dataPath, "Map_files/EMODnet_HA_WindFarms_20220324/EMODnet_HA_WindFarms_pg_20220324.shp"))
turbine.NL <- st_read(file.path(dataPath, "Map_files/EMODnet_HA_WindFarms_20220324/turbines.shp"))
turbine.BE <- st_read(file.path(dataPath, "Map_files/EMODnet_HA_WindFarms_20220324/WTG.shp"))


sf_turbine.NL <- turbine.NL %>% 
  st_as_sf(coords = c("utm_x", "utm_y"), crs = 32631) %>% 
  st_transform(crs = 4326)
# { bind_cols(st_drop_geometry(.), st_coordinates(.)) } %>% 
# rename(lon = X, lat = Y)

sf_turbine.BE <- turbine.BE %>% 
  st_as_sf(coords = c("utm_x", "utm_y"), crs = 32631) %>% 
  st_transform(crs = 4326)
# { bind_cols(st_drop_geometry(.), st_coordinates(.)) } %>% 
# rename(lon = X, lat = Y)

ggplot() +
  # geom_sf(data = Shapefile_netherlands, fill = "lightblue", color = "black") +
  geom_sf(data = Cropped_OWF_shp, aes(), size = 2) +
  geom_sf(data = cropped_sf_turbine.BE, aes(color = "red"), size = 1) +
  geom_sf(data = cropped_sf_turbine.NL, aes(color = "blue"), size = 1)


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





DATA_wrecks <- read_csv(file.path(dataPath, "LOC_wrecks.csv"))

sf_data_wreck <- st_as_sf(DATA_wrecks, coords = c("lon", "lat"), crs = 4326)

################################################################################
# All wrecks 
ggplot() +
  geom_sf(data = subset(sf_data_GIT), aes(color = "GIT"), size = 5) + 
  geom_sf(data = subset(sf_data_wreck), aes(color = "wreckname"), size = 5) + 
  # geom_sf(data = stations_sf, aes(color = station_id), size = 3) +
  labs(title = "Map of experimental area", x = "Longitude", y = "Latitude") +
  theme_minimal() 

# Specific wrecks
ggplot() +
  geom_sf(data = subset(sf_data_GIT, station %in% c("BSW1")), aes(color = "GIT"), size = 5) + 
  geom_sf(data = subset(sf_data_wreck, station == "BSW1"),aes(color = "wreckname"), size = 5) + 
  labs(title = "Map of experimental area", x = "Longitude", y = "Latitude") +
  theme_minimal()

ggplot() +
  geom_sf(data = subset(sf_data_GIT, station %in% c("BSW2")), aes(color = "GIT"), size = 5) + 
  geom_sf(data = subset(sf_data_wreck, station == "BSW2"),aes(color = "wreckname"), size = 5) + 
  labs(title = "Map of experimental area", x = "Longitude", y = "Latitude") +
  theme_minimal()

ggplot() +
  geom_sf(data = subset(sf_data_GIT, station %in% c("grafton")), aes(color = "GIT"), size = 5) + 
  geom_sf(data = subset(sf_data_wreck, station == "grafton"),aes(color = "wreckname"), size = 5) + 
  labs(title = "Map of experimental area", x = "Longitude", y = "Latitude") +
  theme_minimal()

ggplot() +
  geom_sf(data = subset(sf_data_GIT, station %in% c("birkenfels")), aes(color = "GIT"), size = 5) + 
  geom_sf(data = subset(sf_data_wreck, station == "birkenfels"),aes(color = "wreckname"), size = 5) + 
  labs(title = "Map of experimental area", x = "Longitude", y = "Latitude") +
  theme_minimal()

ggplot() +
  geom_sf(data = subset(sf_data_GIT, station %in% c("gardencity")), aes(color = "GIT"), size = 5) + 
  geom_sf(data = subset(sf_data_wreck, station == "gardencity"),aes(color = "wreckname"), size = 5) + 
  labs(title = "Map of experimental area", x = "Longitude", y = "Latitude") +
  theme_minimal()



R <- 6371000      # Radius of the earth in meters

locations_wrecks <- locations_GIT_filtered %>% 
  left_join(DATA_wrecks, by = "station") %>% 
  select(ID, station, loc_on_wreck, lat_GIT, lon_GIT, lat, lon, bow_deg, length) %>% 
  mutate(lat_diff = abs(lat_GIT - lat),
         lon_diff = abs(lon_GIT - lon)) %>% 
  mutate(lat_meters = lat_diff * (R * pi / 180),                                                # Convert lat difference to meters
         lon_meters = lon_diff * (R * pi / 180 * cos(lat_GIT * pi / 180))) %>%                  # Convert lon difference to meters
  mutate(total_distance = sqrt(lat_meters^2 + lon_meters^2)) %>%                                # Total distance in meters
  mutate(lat_meters = round(lat_meters, 2),                                                     # Round latitude difference to 2 decimal places
         lon_meters = round(lon_meters, 2),                                                     # Round longitude difference to 2 decimal places
         total_distance = round(total_distance, 2))  

test <- locations_wrecks %>%
  filter(complete.cases(.)) %>% 
  group_by(station) %>% 
  rowwise() %>% 
  mutate(
    locA = list(destPoint(c(lat, lon), bow_deg, length)),
    locA_lon = locA[1],  
    locA_lat = locA[2],
    locB = list(destPoint(c(lat, lon), (bow_deg + 180), length)),
    locB_lon = locB[1],  
    locB_lat = locB[2]) %>% 
  select(-locA, -locB)

test_dis <- test %>% 
  mutate(lat_diff_A = abs(lat_GIT - locA_lat),
         lon_diff_A = abs(lon_GIT - locA_lon)) %>% 
  mutate(lat_meters_A = lat_diff_A * (R * pi / 180),                                                # Convert lat difference to meters
         lon_meters_A = lon_diff_A * (R * pi / 180 * cos(lat_GIT * pi / 180))) %>%                  # Convert lon difference to meters
  mutate(total_distance_A = sqrt(lat_meters_A^2 + lon_meters_A^2)) %>%                                # Total distance in meters
  mutate(lat_meters_A = round(lat_meters_A, 3),                                                     # Round latitude difference to 2 decimal places
         lon_meters_A = round(lon_meters_A, 3),                                                     # Round longitude difference to 2 decimal places
         total_distance_A = round(total_distance_A, 2))





p <- cbind(5, 52)
destPoint(p, 26 + 180, 100)





without_NA <- locations_wrecks %>% 
  # filter(complete.cases(.)) %>% 
  # filter(ID == "2021-BE_grafton") %>% 
  # filter(ID == "2021-BE_birkenfels") %>%
  # filter(ID == "2023-BE_gardencity") %>%
  select(ID, loc_on_wreck, 
         # lat_meters, lon_meters, 
         total_distance) %>% 
  arrange(total_distance)





















################################################################################

energy_cables <- st_read(file.path(dataPath, "Map_files/MUMM_energy_cables_ETRS89_2/MUMM_energy_cables_ETRS89_2.shp"))
pipelines <- st_read(file.path(dataPath, "Map_files/MUMM_pipelines_ETRS89/MUMM_pipelines_ETRS89.shp"))
windfarms_BE_correct <- st_read(file.path(dataPath, "Map_files/MUMM_Windfarm_Concessions_ETRS89/MUMM_Windfarm_Concessions_ETRS89.shp"))

ssf_turbine.NL <- turbine.NL %>% 
  st_as_sf(coords = c("utm_x", "utm_y"), crs = 32631) %>% 
  st_transform(crs = 4326)

energy_cables <- energy_cables %>% 
  st_as_sf(coords = c("utm_x", "utm_y"), crs = 32631) %>% 
  st_transform(crs = 4326)

pipelines <- pipelines %>% 
  st_as_sf(coords = c("utm_x", "utm_y"), crs = 32631) %>% 
  st_transform(crs = 4326)

windfarms_BE_correct <- windfarms_BE_correct %>% 
  st_as_sf(coords = c("utm_x", "utm_y"), crs = 32631) %>% 
  st_transform(crs = 4326)

Crop_bbox <- st_bbox(c(xmin = 2.2, ymin = 51.3, xmax = 3.5, ymax = 51.9), crs = st_crs(energy_cables))
Cropped_energy_cables_shp <- st_crop(energy_cables, Crop_bbox)
Cropped_pipelines_shp <- st_crop(pipelines, Crop_bbox)
Cropped_windfarms_BE_correct_shp <- st_crop(windfarms_BE_correct, Crop_bbox)


ggplot() +
  # geom_sf(data = Shapefile_netherlands, fill = "lightblue", color = "black") +
  geom_sf(data = Cropped_OWF_shp, aes(), size = 2) +
  geom_sf(data = cropped_sf_turbine.BE, aes(color = "red"), size = 1) +
  geom_sf(data = cropped_sf_turbine.NL, aes(color = "blue"), size = 1) +
  geom_sf(data = Cropped_energy_cables_shp, aes(color = "blue"), size = 1) +
  geom_sf(data = Cropped_pipelines_shp, aes(color = "black"), size = 1) +
  geom_sf(data = Cropped_windfarms_BE_correct_shp, aes(), size = 1) 
  
ggplot() +
  # geom_sf(data = Shapefile_netherlands, fill = "lightblue", color = "black") +
  geom_sf(data = Cropped_OWF_shp, aes(), size = 2) +
  geom_sf(data = cropped_sf_turbine.BE, aes(color = "red"), size = 1) +
  geom_sf(data = cropped_sf_turbine.NL, aes(color = "blue"), size = 1) +
  geom_sf(data = energy_cables, aes(color = "blue"), size = 1) +
  geom_sf(data = pipelines, aes(color = "black"), size = 1) +
  geom_sf(data = windfarms_BE_correct, aes(), size = 1) 


