# Driver script
library(ndotr) # Custom package to facilitate calculations and visualizations
library(terra) # For raster manipulations
library(sf) # For other spatial manipulations
library(ggplot2) # For plotting
library(stars) # For plotting rasters naturally in ggplot

# This is the only section of the script that should require editing.
#===============================================================================
# Specify the location of the folder containing the NAM, hires forecasts.
hires_path <- "data-raw/sample_forecast_files/hires_sample"
# Specify the file type of the hires data
hires_type <- "grib2"

# Specify the location of the folder containing the SNODAS snow depth and SWE
# measurements.
snodas_path <- "data-raw/sample_forecast_files/snodas_data"
# Specify the file type of the SNODAS data
snodas_type <- "tif"

# Specify the location of the folder in which to put the PNG files
# that will be displayed.
png_path <- "figures/png_test"
#===============================================================================

# Clean up the figure folder in preparation for making new ones
# clean_folder(png_path)

### RASTER PREP ###

# Read in the nam_hires data and calculate the present weather potential (pwp)
# thresholds.
thires <- hires_prep(path = hires_path, type = hires_type)

# Calculate PWP
pwp <- pwp_calc(thires)

# Read in SNODAS data and calculate density ratio excluding all cells for which
# there is less than 10mm of SWE.
snow_ratio <- snodas_prep(path = snodas_path, type = snodas_type)

# Reproject the snow ratio to be identical to the hires data.
# About 9 grid cells contribute to each of the reprojected ones. The
# method of choice is to take the 75th percentile of the contributing cells.
snow_ratio <- project(snow_ratio, pwp, method = "q3")

# Calculate ROS TWI.
ros_twi <- twi_calc(snow_ratio, pwp)

### IMAGE PREP ###

# Transform all the spatial information to match the raster.
nv_sf <- st_transform(ndotr::nevada_sf, crs(ros_twi))
nv_sf2 <- st_transform(ndotr::nevada_sf2, crs(ros_twi))
nv_roads <- st_transform(ndotr::nevada_roads, crs(ros_twi))
nv_cities <- st_transform(ndotr::nevada_cities, crs(ros_twi))

# Define a pallette of colors based on recommendations from Heggli et al. (2023)
# Color palette from: https://colorbrewer2.org/
# Help on color palette from chatGPT
# Help on legend entries from (stefan Jul 14 comment):
# - https://stackoverflow.com/questions/10002627/ggplot2-0-9-0-automatically-dropping-unused-factor-levels-from-plot-legend
colors <- c("0" = "gray97", "1" = "#d0d1e6", "2" = "#a6bddb",
            "3" = "#74a9cf", "4" = "#3690c0", "5" = "#0570b0",
            "6" = "#034e7b")

### IMAGE RUN ###
for (i in seq_len(dim(pwp)[3])) {
  ros_stars <- st_as_stars(ros_twi[[i]])
  ros_stars[[1]] <- factor(ros_stars[[1]], levels = 0:6)


  tplot <- ggplot() +
    geom_stars(data = ros_stars, show.legend = TRUE) +
    geom_sf(data = nv_sf, fill = NA, color = "black") +
    geom_sf(data = nv_sf2, fill = NA, color = "black", lwd = 1) +
    geom_sf(data = nv_roads, fill = NA, color = "gray30", lty = 2) +
    geom_sf(data = nv_cities) +
    geom_sf_text(data = nv_cities, aes(label = name),
                 nudge_y = 20000) +
    scale_fill_manual("ROS TWI\nPotential", values = colors, drop = FALSE) +
    ggtitle(format(time(pwp)[i], tz = "America/Los_Angeles", usetz = TRUE)) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          text = element_text(size = 16),
          legend.text = element_text(size = 18),
          plot.title = element_text(hjust = 0.5))

  png(filename = paste0(png_path, "/forecast_", i, ".png"),
      width = 8, height = 8, units = "in", res = 600)
  print(tplot)
  dev.off()
}

# Clean out forecast_folder (commented for now)
# clean_folder(hires_path)

# Clean out snodas_folder (commented for now)
# clean_folder(snodas_path)

# Clean up global environment (commented for now)
# remove(list = ls())
