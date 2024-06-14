library(terra)
library(sf)
library(tidyverse)
library(rasterVis)
library(latticeExtra)
library(stars)

rast_final <- rast("data-raw/simple_features/temp_thresh_rast.nc")

rast_stars <- st_as_stars(rast_final)

# Crop both rasters based on a buffer extend around the state of Nevada.
nevada_map <- maps::map("county", "nevada", plot = FALSE, fill = TRUE)
nevada_map2 <- maps::map("state", "nevada", plot = FALSE, fill = TRUE)

# Convert the map data to an sf object
nevada_sf <- st_as_sf(nevada_map)
nevada_sf2 <- st_as_sf(nevada_map2)
nevada_sf <- st_transform(nevada_sf, st_crs(rast_final))
nevada_sf2 <- st_transform(nevada_sf2, st_crs(rast_final))

nevada_roads <- st_read("data-raw/simple_features", layer = "nv_road_simp") |>
  st_transform(st_crs(nevada_sf))

nv_cities <- read.csv("data-raw/nv_cities.csv")
coi <- c("Reno", "Winnemucca", "Elko", "Tonopah", "Ely", "Wells",
         "Las Vegas", "Eureka")
nv_cities_sub <- nv_cities |>
  select(name = Feature.Name, lat = Latitude, lon = Longitude) |>
  filter(name %in% coi) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(crs = st_crs(rast_final))



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

