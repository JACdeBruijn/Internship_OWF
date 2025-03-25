### Loading packages ###
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, leaflet, sf, rnaturalearth, rnaturalearthdata, ggspatial,
               marmap, raster, ggstar)

rm(list=ls())

###############################################################################-
## loading file paths ----

figurePath    <- file.path('.','figures')      
dataPath      <- file.path('.','data')
resultPath    <- file.path('.','results')

###############################################################################-
### Working on the coordinates of the multi-sensor frames ----
locations_GIT <- read_csv("Data/data_overview.csv")                               # Location data from github deposit
locations_GIT_filtered <- locations_GIT %>% 
  filter(pairing > 0) %>% 
  mutate(pairingName = str_c(dataSet, '_', pairing))

# Converting the GIT coords to sf file
sf_data_GIT <- st_as_sf(locations_GIT_filtered, coords = c("lon", "lat"), crs = 4326)

###############################################################################-
## Loading shapefiles of the turbines ----
turbine_NL <- st_read(file.path(dataPath, "Map_files/EMODnet_HA_WindFarms_20220324/turbines.shp"))
turbine_BE <- st_read(file.path(dataPath, "Map_files/EMODnet_HA_WindFarms_20220324/WTG.shp"))

## Boundingbox to crop the files to the same level
bbox <- st_bbox(c(xmin = 2.2, ymin = 51.3, xmax = 3.5, ymax = 51.9), crs = st_crs(4326)) %>% st_as_sfc()

## Fixing the projections of the NL turbines ----
turbine_NL <- st_transform(turbine_NL, crs = 4326)
turbine_NL <- st_crop(turbine_NL, bbox)
turbine_NL <- turbine_NL %>%
  mutate(lon = st_coordinates(.)[,1],  # Extract X (Longitude)
         lat  = st_coordinates(.)[,2])  # Extract Y (Latitude)
turbine_NL_final <- turbine_NL %>%
  dplyr::select(objectid, naam, lat, lon)
print(head(turbine_NL_final, 10))

## Fixing the projections of the BE turbines ----
turbine_BE <- st_transform(turbine_BE, crs = 4326)
turbine_BE <- st_crop(turbine_BE, bbox)
turbine_BE <- turbine_BE %>%
  mutate(lon = st_coordinates(.)[,1],  
         lat  = st_coordinates(.)[,2])
turbine_BE_final <- turbine_BE %>%
  dplyr::select(OBJECTID, code, concession, lat, lon)
print(head(turbine_BE_final, 10))

# Saving the coordinates to a csv file 
# write.csv(turbine_NL_final, file.path(resultPath, "Map_files/turbine_NL_coordinates.csv"), row.names = T)
# write.csv(turbine_BE_final, file.path(resultPath, "Map_files/turbine_BE_coordinates.csv"), row.names = T)

## First basis plot to see if the projections are now correct
ggplot() +
  geom_sf(data = sf_data_GIT, aes(color = "red"), size = 2) +
  geom_sf(data = turbine_BE_final) +
  geom_sf(data = turbine_NL_final) 


###############################################################################-
# Wrangling turbine data, mutating ID to country, rbind dataframes together
turbine_BE_tmp <- turbine_BE_final %>% 
  mutate(ID_name = paste(concession, OBJECTID, sep = "_")) %>% 
  dplyr::select(-OBJECTID, -code, -concession)

turbine_NL_tmp <- turbine_NL_final %>% 
  mutate(ID_name = paste(naam, objectid, sep = "_")) %>% 
  dplyr::select(-objectid, -naam)

all_turbines_tmp <- rbind(turbine_BE_tmp, turbine_NL_tmp)
# write.csv(all_turbines_tmp, file.path(resultPath, "Map_files/all_turbines_coordinates.csv"), row.names = T)

# Removing the shipwrecks from the Git coordinates file to only have the frames inside the windfarms
sf_data_GIT_onlyOWFs <- sf_data_GIT %>% 
  filter(type != "control") %>% 
  dplyr::select(year, dataSet, station, type)

###############################################################################-
# Calculating the distances between the turbines and the frames 
distance_to_structure <- sf_data_GIT_onlyOWFs %>%
  mutate(min_dist_m = apply(st_distance(sf_data_GIT_onlyOWFs, all_turbines_tmp), 1, min))
# write.csv(distance_to_structure, file.path(resultPath, "Map_files/distance_to_structure.csv"), row.names = T)

## plot to see the frame locations against the turbine locations 
ggplot() +
  geom_sf(data = all_turbines_tmp) +
  geom_sf(data = sf_data_GIT_onlyOWFs, aes(color = "red"), size = 3,
          show.legend = F)


###############################################################################-
# Adding the shapefiles of the OWFs to the map
OWF_shp <- st_read(file.path(dataPath, "Map_files/OWF_polygons/windfarmspolyPolygon.shp"))
# Other_OWF_shp <- st_read(file.path(dataPath, "Map_files/EMODnet_HA_WindFarms_20220324/EMODnet_HA_WindFarms_pg_20220324.shp"))

# This shapefile has the updated windpark in the belgium part, however the projection is not fixble so I leave it for now
# OWF_shp_BE <- st_read(file.path(dataPath, "Map_files/MUMM_Windfarm_Concessions_ETRS89/MUMM_Windfarm_Concessions_ETRS89.shp"))

# Cropping the OWF polygons to the correct bounding box
OWF_shp <- st_crop(OWF_shp, bbox)
# Other_OWF_shp <- st_crop(OWF_shp, bbox)

ggplot() +
  geom_sf(data = OWF_shp) +
  geom_sf(data = sf_data_GIT, aes(color = "red"), size = 3,
          show.legend = F) +
  theme_minimal()


###############################################################################-
# Adding bathymetric ----

# Example for the right coordinates
#c(xmin = 2.2, ymin = 51.3, xmax = 3.5, ymax = 51.9), crs = st_crs(4326))
bathy_noaa <- getNOAA.bathy(lon1 = 2.2, lon2 = 4, lat1 = 51.3, lat2 = 51.9, resolution = 1)

blues <- c("#08306b", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#deebf7", "#f7fbff")  # Ocean gradient
land_color <- "grey70"  # Solid grey for land

# Convert bathymetry data to dataframe
bathy_noaa_df <- fortify.bathy(bathy_noaa)

# Separate land and sea
bathy_noaa_df <- bathy_noaa_df %>%
  mutate(type = ifelse(z >= 0, "land", "sea"))  # Land (>=0), Sea (<0)

# Plot in ggplot2
ggplot() +
  # Raster for ocean bathymetry with gradient
  geom_tile(data = bathy_noaa_df %>% filter(type == "sea"), aes(x = x, y = y, fill = z)) +
  scale_fill_gradientn(colors = blues, name = "Depth (m)", limits = c(min(bathy_noaa_df$z), 0)) +
  
  # Raster for land with solid grey color
  geom_tile(data = bathy_noaa_df %>% filter(type == "land"), aes(x = x, y = y), fill = land_color) +
  
  # Contour line for coastline at depth = 0
  geom_contour(data = bathy_noaa_df, aes(x = x, y = y, z = z), 
               breaks = 0, color = "black", size = 0.6) +
  
  geom_sf(data = OWF_shp, fill = NA, color = "black", size = 5) +  # Study area outline
  geom_sf(data = sf_data_GIT, color = "red", size = 3, show.legend = FALSE) +
  
  # geom_sf(data = Shapefile_netherlands, fill = NA, color = "grey", size = 0.8) +
  
  # Improve visualization
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")

###############################################################################-
pacman::p_load(tidyverse, leaflet, sf, rnaturalearth, rnaturalearthdata, ggspatial,
               marmap, raster, rgdal, mapdata, maptools, downloader, directlabels, rasterVis,
               XML, ncdf4, kableExtra, rjson, reshape2)Shapefile_netherlands <- st_read("Data/Map_files/Netherlands_shapefile/nl_1km.shp")

Shapefile_netherlands <- st_transform(Shapefile_netherlands, crs = 4326)
Shapefile_netherlands <- st_crop(Shapefile_netherlands, bbox)


EMODnet_waterboarders <- st_read(file.path(dataPath, "Map_files/EMODnet_waterboarders/mspspatialplan.shp"))
EMODnet_waterboarders <- st_transform(EMODnet_waterboarders, crs = 4326)
EMODnet_waterboarders <- st_crop(EMODnet_waterboarders, bbox)

# Download boundary data for Belgium and the Netherlands
countries <- ne_countries(scale = "medium", returnclass = "sf")
belgium_netherlands <- countries[countries$iso_a3 %in% c("BEL", "NLD"), ]
belgium_netherlands <- st_crop(belgium_netherlands, bbox)

sf_data_GIT <- sf_data_GIT %>% 
  mutate(type = factor(type)) +
  mutate(pairingName = factor(pairingName, 
                            levels = c("2021-BE_1", "2021-BE_2", "2023-BSW_1", "2023-BSW_2", "2023-BE_1", "2023-BE_2", "2024-BE_1"), 
                            label = c("July-Aug/21 - Birk-/Belw-",
                                      "July-Aug/21 - Graf-/Cpow-",
                                      "May-June/23 - Bors 1/Bors 4",
                                      "May-June/23 - Bors 2/Bors 3",
                                      "July-Sept/23 - Gard-/Belw-",
                                      "July-Sept/23 - Graf-/Cpow-",
                                      "Oct-Dec/23 - Graf-/Cpow-")))

CPOD_manual_shapes <- c(1, 5, 13, 15, 23, 28, 22)[seq_len(7)]

sf_data_GIT_star <- sf_data_GIT %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2])

# Define a function to read in raster data from the EMODnet bathymetry WCS
getbathymetry<-function (name = "emodnet:mean", xmin = 2.2, ymin = 51.3, xmax = 3.5, ymax = 51.9){
  bbox <- paste(xmin, ymin, xmax, ymax, sep = ",")
  
  con <- paste("https://ows.emodnet-bathymetry.eu/wcs?service=wcs&version=1.0.0&request=getcoverage&coverage=",
               name,"&crs=EPSG:4326&BBOX=", bbox,
               "&format=image/tiff&interpolation=nearest&resx=0.00208333&resy=0.00208333", sep = "")
  
  print(con)
  
  stop
  nomfich <- paste(name, "img.tiff", sep = "_")
  nomfich <- tempfile(nomfich)
  download(con, nomfich, quiet = TRUE, mode = "wb")
  img <- raster(nomfich)
  img[img > 0] <- 0
  img[img == 0] <- NA
  names(img) <- paste(name)
  return(img)
}



map_bathy <- ggplot() +
  geom_raster(data = bathy, aes(x = x, y = y, fill = emodnet.mean)) +
  # scale_fill_gradient(low = "darkblue", high = "white", name = "Depth (m)") +
  scale_fill_gradientn(colors = blues, name = "Depth (m)", limits = c(min(bathy$emodnet.mean), 0)) +
  
  geom_sf(data = EMODnet_waterboarders, fill = NA, color = alpha("orange3", .6), linewidth = 1) +
  
  geom_sf(data = belgium_netherlands, fill = "darkseagreen", color = "black", linewidth = .5) +
  
  geom_sf(data = OWF_shp, fill = NA, color = "black", linewidth = 1) +  # Study area outline
  
  # geom_sf(data = sf_data_GIT, color = "red", size = 3, show.legend = FALSE) +  # Points in red
  
  geom_star(data = sf_data_GIT_star, aes(x = lon, y = lat, starshape = pairingName),
            color = "red",
            fill = NA,
            starstroke = 1.5,
            size = 6) +
  scale_starshape_manual(values = CPOD_manual_shapes,
                         name = "Pairings - Period & Location") +
  # scale_fill_manual(values = c("#388fe0", "#ccac3d")) +
  
  ggtitle("EMODnet Bathymetry") + xlab("Longitude") + ylab("Latitude") +
  theme_bw() +
  theme(
    plot.title = element_blank(),
    axis.title.x = element_text(size = 15),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 15),
    axis.text.y = element_text(size = 10, color = "black")
  )


ggsave(filename = file.path(figurePath,'Map of the bathy and stydyarea.png'), 
       plot = map_bathy, width = 14, height = 10)















