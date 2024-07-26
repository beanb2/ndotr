library(terra)

# Create a character string representing the date and time of the
# hires intitial forcing.
datetime_string <- "07/24/2024 12:00"
datetime_object <- as.POSIXct(datetime_string, format = "%m/%d/%Y %H:%M")

# Determine the layer names from the hires forecast stuff
# Temperature is a 12 hour forecase and precip is a 9-12 hour accumulation
layer_names <- c("SFC=Ground or water surface; Total precipitation [kg/(m^2)]",
                 "SFC=Ground or water surface; Temperature [C]")

# List all the forecast files.
tfiles <- list.files(path = "data-raw/sample_forecast_files",
                   pattern = "\\.grib2$", full.names = TRUE)

# Extract the hours (using base R intentionally)
t_hours <- regmatches(tfiles, gregexpr("f[[:digit:]]+\\.", tfiles)) |>
  unlist() |>
  gsub(pattern = "[^[:digit:]]", replacement = "") |>
  as.numeric()

# Then, ensure the tfiles are sorted properly.
tfiles <- tfiles[order(t_hours)]

# Create a sequence of times where each time from the hires data represents
# hours.
datetime_vec <- datetime_object + 3600*t_hours

# Looks like forecast is 3 hour total precip.
test <- terra::rast(tfiles[4]) |>
  terra::subset(layer_names)



# Now create a multi-raster layer of the grib2 files.
hires_list <- lapply(tfiles, terra::rast)

hires_list <- terra::rast(hires_list)

