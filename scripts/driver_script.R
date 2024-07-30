# Driver script
library(terra)
library(sf)
library(stars)
library(ndotr)

# Read in the nam_hires data and calculate the present weather potential (pwp)
# thresholds.
thires <- hires_prep(path = "data-raw/sample_forecast_files/hires_sample")

# Calculate PWP
pwp <- pwp_calc(thires)

# Prep snodas snowpack ratio data, excluding all cells for which there
# is less than 10mm of SWE.
snow_ratio <- snodas_prep(path = "data-raw/sample_forecast_files/snodas_data",
                          type = "tif")

# Reproject the snow ratio to be identical to the hires data.
# About 9 grid cells contribute to each of the reprojected ones. The
# method of choice is to take the 75th percentile of the contributing cells.
snow_ratio <- project(snow_ratio, pwp, method = "q3")

# Calculate ROS TWI.
ros_twi <- twi_calc(snow_ratio, pwp)

# Transform all the spatial information to match the raster.
nv_sf <- st_transform(ndotr::nevada_sf, crs(ros_twi))
nv_sf2 <- st_transform(ndotr::nevada_sf2, crs(ros_twi))
nv_roads <- st_transform(ndotr::nevada_roads, crs(ros_twi))
nv_cities <- st_transform(ndotr::nevada_cities, crs(ros_twi))

ros_stars <- st_as_stars(ros_twi)

ggplot() +
  geom_stars(data = ros_stars) +
  geom_sf(data = nv_sf, fill = NA, color = "black") +
  geom_sf(data = nv_sf2, fill = NA, color = "black", lwd = 1) +
  geom_sf(data = nv_roads, fill = NA, color = "gray40", lty = 2) +
  geom_sf(data = nv_cities) +
  geom_sf_text(data = nv_cities, aes(label = name),
               nudge_y = 20000) +
  # geom_text(data = nv_cities_sub, aes(label = name), size = 3, nudge_y = 0.1) +
  theme_minimal() +
  theme(axis.title = element_blank())
