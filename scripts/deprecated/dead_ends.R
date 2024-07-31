library(tidyverse)
library(sf)
library(sf)
library(stars)

crs_string <- "+proj=lcc +lat_1=38.5 +lat_2=38.5 +lat_0=21.138 +lon_0=-97.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m"

# rast2 <- rast("data-raw/sample_forecast_files/SNODAS_20240212_SWE.nc")

rast1 <- rast("data-raw/sample_forecast_files/nam.t18z.conusnest.hiresf00.tm00.nc")
rast1 <- rast1$TMP_P0_L1_GLC0
crs(rast1) <- crs_string

new_resolution <- c(3000, 3000)  # 3 km in meters (x and y resolution)
nrows <- nrow(rast1)
ncols <- ncol(rast1)
current_extent <- ext(rast1)

new_extent <- ext(current_extent[1],
                  current_extent[2] - res(rast1)[1] + new_resolution[1] * ncols,
                  current_extent[3],
                  current_extent[4] - res(rast1)[2] + new_resolution[2] * nrows)

ext(rast1) <- new_extent
rast1

temp2 <- as.points(rast1)
temp3 <- sf::st_as_sf(temp2)
# res(rast1) <- new_resolution
# rast1



rast_sub <- rast1[100:400, 200:425, drop = FALSE]
# plot(rast_sub$gridlon_0)
# plot(rast_sub$gridlat_0)
temp <- as.points(rast_sub)



nevada_map2 <- maps::map("state", "nevada", plot = FALSE, fill = TRUE)

# Convert the map data to an sf object
nevada_sf2 <- st_as_sf(nevada_map2)
nevada_sf2 <- st_transform(nevada_sf2, st_crs(rast1))
nevada_vec <- vect(nevada_sf2)

plot(rast1$TMP_P0_L1_GLC0)
plot(nevada_vec, add = TRUE)


rast1_p <- sf::st_as_sf(as.points(rast_sub)) |>
  select(TMP_P0_L1_GLC0,
         APCP_P8_L1_GLC0_acc,
         gridlat_0,
         gridlon_0)

test <- st_transform(rast1_p, crs = 4326)

summary(rast1_p$TMP_P0_L1_GLC0)
summary(rast1_p$TMP_P0_L1_GLC0)
