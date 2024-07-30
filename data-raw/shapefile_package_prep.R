library(terra)
library(sf)
library(tidyverse)

# Read in a template SNODAS raster. (used rsnodas2 package to download)
# rsnodas2::download_snodas("2022-12-29")
t_rast <- rast("data-raw/sample_forecast_files/snodas_data/sp2022-12-29.tif")

# Crop both rasters based on a buffer extend around the state of Nevada.
nevada_map <- maps::map("county", "nevada", plot = FALSE, fill = TRUE)
nevada_map2 <- maps::map("state", "nevada", plot = FALSE, fill = TRUE)

# Convert the map data to an sf object
nevada_sf <- st_as_sf(nevada_map)
nevada_sf2 <- st_as_sf(nevada_map2)
nevada_sf <- st_transform(nevada_sf, crs(t_rast))
nevada_sf2 <- st_transform(nevada_sf2, crs(t_rast))

# Transform to a projection that uses meters (e.g., UTM zone 11N, EPSG:32611)
nevada_sf_utm <- st_transform(nevada_sf2, 32611)
# Create a 50km buffer
nevada_buffer_utm <- st_buffer(nevada_sf_utm, dist = 50000)
# Create a bigger buffer to help with the raster reprojections.
nevada_buffer_utm_big <- st_buffer(nevada_sf_utm, dist = 250000)
# Transform the buffer back to the original geographic coordinates (WGS84)
nevada_buffer <- st_transform(nevada_buffer_utm, st_crs(t_rast))
nevada_buffer_big <- st_transform(nevada_buffer_utm_big, st_crs(t_rast))

nevada_roads <- st_read("data-raw/simple_features", layer = "nv_road_simp") |>
  st_transform(crs(t_rast))

nv_cities <- read.csv("data-raw/nv_cities.csv")
coi <- c("Reno", "Winnemucca", "Elko", "Tonopah", "Ely", "Wells",
         "Las Vegas", "Eureka")
nevada_cities <- nv_cities |>
  select(name = Feature.Name, lat = Latitude, lon = Longitude) |>
  filter(name %in% coi) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs = crs(t_rast))

use_data(nevada_sf)
use_data(nevada_sf2)
use_data(nevada_buffer, overwrite = TRUE)
use_data(nevada_buffer_big, overwrite = TRUE)
use_data(nevada_roads)
use_data(nevada_cities)
