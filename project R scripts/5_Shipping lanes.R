################################################################################-
##Plotting the shipping lanes 
################################################################################-
if(!require(pacman)) install.packages("pacman")
pacman::p_load(terra, tidyverse)

rm(list=ls())
figurePath    <- file.path('.','figures')   
dataPath      <- file.path('.','data')
shipping <- rast("data/Shipping_lanes/EMODnet_HA_Vessel_Density_all_2017-2023Avg/vesseldensity_all_2023.tif")

e <- ext(1, 5, 51, 53)
vect_e <- as.polygons(e, crs = "EPSG:4326")
vect_e_proj <- project(vect_e, crs(shipping))
shipping_crop <- crop(shipping, vect_e_proj)

rast_max <- function(x, max = 30) {
  new_x <- ifelse(x > max, max, x)
  return(new_x)
}

shipping_maxed <- app(shipping_crop, rast_max)

# Plotting the shipping lanes
plot(shipping_maxed)


################################################################################-
# Adding the OWFs shape file
OWF_shp <- vect(file.path(dataPath, "Map_files/OWF_polygons/windfarmspolyPolygon.shp"))

OWF_shp <- project(OWF_shp, crs(shipping))
OWF_shp_crop <- crop(OWF_shp, vect_e_proj)
plot(OWF_shp_crop, add = T,
     border = "red")


################################################################################-
# Adding the frame coordinates
locations_GIT <- read_csv("Data/data_overview.csv")                               # Location data from github deposit
locations_GIT_filtered <- locations_GIT %>% 
  filter(pairing > 0) %>% 
  mutate(pairingName = str_c(dataSet, '_', pairing)) %>% 
  mutate(ID = row_number())

points_vect <- vect(locations_GIT_filtered, geom = c("lon", "lat"), crs = "EPSG:4326")
points_proj <- project(points_vect, crs(shipping_crop))
plot(points_proj, add = T, 
     col = "red", pch = 16)


# The entire plot
plot(shipping_maxed)
plot(OWF_shp_crop, add = T,
     border = "red")
plot(points_proj, add = T, 
     col = "red", pch = 16)


ggsave(filename = file.path(figurePath, "Vessel densities 2023.png"), plot = interaction_WBAT_CPOD, width = 14, height = 10)

################################################################################-
# Calculating the vessel densities for 1 km 
buffers <- buffer(points_vect, width = 1000)
vals_all <- terra::extract(shipping_maxed, buffers)
mean_density_per_point_1km <- vals_all %>%
  group_by(ID) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE)))

Radius_1km <- locations_GIT_filtered %>% 
  left_join(mean_density_per_point_1km, by = "ID")

ggplot(Radius_1km, aes(x = type, y = lyr.1, fill = type)) +
  geom_boxplot() +
  theme_minimal()


################################################################################-
# Calculating the vessel densities for 2 km 
buffers <- buffer(points_vect, width = 2000)
vals_all <- terra::extract(shipping_maxed, buffers)
mean_density_per_point_2km <- vals_all %>%
  group_by(ID) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE)))

Radius_2km <- locations_GIT_filtered %>% 
  left_join(mean_density_per_point_2km, by = "ID")

ggplot(Radius_2km, aes(x = type, y = lyr.1, fill = type)) +
  geom_boxplot() +
  theme_minimal()


################################################################################-
# Calculating the vessel densities for 5 km 
buffers <- buffer(points_vect, width = 5000)
vals_all <- terra::extract(shipping_maxed, buffers)
mean_density_per_point_5km <- vals_all %>%
  group_by(ID) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE)))

Radius_5km <- locations_GIT_filtered %>% 
  left_join(mean_density_per_point_5km, by = "ID")

ggplot(Radius_5km, aes(x = type, y = lyr.1, fill = type)) +
  geom_boxplot() +
  theme_minimal()















