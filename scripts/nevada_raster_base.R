library(terra)
library(sf)
library(tidyverse)
library(rasterVis)
library(latticeExtra)
library(stars)

rast1 <- rast("data-raw/jennings_et_al_2018_file4_temp50_raster.tif")
rast2 <- rast("data-raw/jennings_et_al_2018_file5_temp50_linregr_raster.tif")

crs(rast1) <- crs(rast2) <-  "EPSG:4326"

# Code below aided by ChatGPT:

# Crop both rasters based on a buffer extend around the state of Nevada.
nevada_map <- maps::map("county", "nevada", plot = FALSE, fill = TRUE)
nevada_map2 <- maps::map("state", "nevada", plot = FALSE, fill = TRUE)

# Convert the map data to an sf object
nevada_sf <- st_as_sf(nevada_map)
nevada_sf2 <- st_as_sf(nevada_map2)

# Transform to a projection that uses meters (e.g., UTM zone 11N, EPSG:32611)
nevada_sf_utm <- st_transform(nevada_sf, 32611)
# Create a 50km buffer
nevada_buffer_utm <- st_buffer(nevada_sf_utm, dist = 50000)
# Transform the buffer back to the original geographic coordinates (WGS84)
nevada_buffer <- st_transform(nevada_buffer_utm, st_crs(rast1))
nevada_sf <- st_transform(nevada_sf, st_crs(rast1))
nevada_sf2 <- st_transform(nevada_sf2, st_crs(rast1))

rast1_crop <- crop(rast1, nevada_buffer)
rast2_crop <- crop(rast2, nevada_buffer)

rast_final <- cover(rast1_crop, rast2_crop)

rast_stars <- st_as_stars(rast_final)
st_crs(rast_stars)

nevada_roads <- st_read("data-raw/simple_features", layer = "nv_road_simp") |>
  st_transform(st_crs(nevada_sf))

nv_cities <- read.csv("data-raw/nv_cities.csv")
coi <- c("Reno", "Winnemucca", "Elko", "Tonopah", "Ely", "Wells",
         "Las Vegas", "Eureka")
nv_cities_sub <- nv_cities |>
  select(name = Feature.Name, lat = Latitude, lon = Longitude) |>
  filter(name %in% coi) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs = st_crs(rast1))

ggplot() +
  geom_stars(data = rast_stars) +
  scale_fill_viridis_c("Temperature Threshold (deg C)") + # Optional: Use a color scale for the raster
  geom_sf(data = nevada_sf, fill = NA, color = "black") +
  geom_sf(data = nevada_sf2, fill = NA, color = "black", lwd = 1) +
  geom_sf(data = nevada_roads, fill = NA, color = "gray40", lty = 2) +
  geom_sf(data = nv_cities_sub) +
  geom_sf_text(data = nv_cities_sub, aes(label = name),
               nudge_y = .2) +
  # geom_text(data = nv_cities_sub, aes(label = name), size = 3, nudge_y = 0.1) +
  theme_minimal() +
  theme(axis.title = element_blank()) +
  guides(fill = guide_colorbar(barwidth = 2, barheight = 25,
                               title.position = "right",
                               title.theme = element_text(angle = -90, vjust = 0)))

