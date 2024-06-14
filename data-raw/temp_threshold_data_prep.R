library(terra)
library(sf)

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

writeCDF(rast_final, filename = "data-raw/simple_features/temp_thresh_rast.nc",
         overwrite = TRUE)
