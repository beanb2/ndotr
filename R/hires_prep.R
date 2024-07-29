#' NOAA NAM Hires Prep
#'
#' Function to prepare NOAA NAM Hires forecast data for inclusion in the TWI
#'   decision support tool.
#'
#' @param path File path containing a folder of NAM hires forecast rasters
#' @param type File type
#' @param cropper An sf shapefile used to crop the forecast data to a relevant
#'   spatial extent.
#' @param layer_names A vector of layer names (see NAM hires files and
#'   documentation for details). The first layer name must correspond to precip
#'   and the second must correspond to temperature.
#' @returns A list of three including (1) a raster of 6 hour precipitation
#'   forecasts, (2) a raster of 6 hour maximum temperature, and (3) a vector
#'   of timestamps as obtained from the raster files.
#' @export
hires_prep <- function(path, type = "grib2", cropper = ndotr::nevada_buffer,
                       layer_names = c(
                         paste(
                           "SFC=Ground or water surface;",
                           "03 hr Total precipitation",
                           "[kg/(m^2)]"
                         ),
                         paste(
                           "SFC=Ground or water surface;",
                           "Temperature [C]"
                         )
                       )) {
  # List all the forecast files.
  tfiles <- list.files(
    path = path,
    pattern = paste0("\\.", type, "$"),
    full.names = TRUE
  )

  # Extract the hours (using base R intentionally)
  t_hours <- regmatches(tfiles, gregexpr("f[[:digit:]]+\\.", tfiles)) |>
    unlist() |>
    gsub(pattern = "[^[:digit:]]", replacement = "") |>
    as.numeric()

  # Then, ensure the tfiles are sorted properly.
  tfiles <- tfiles[order(t_hours)]
  t_length <- length(tfiles)

  # Use the first (initial) file to determine the CRS, then ignore it
  # thereafter.
  t_crs <- terra::crs(terra::rast(tfiles[1]))

  # Crop the forecast files based on the saved Nevada Buffer
  nevada_buffer_t <- cropper |>
    sf::st_transform(crs = t_crs) |>
    terra::vect()

  # Download, subset, and crop the rasters into a list.
  rast_list <- vector("list", t_length - 1)
  time <- as.POSIXct(rep(NA, t_length - 1), tz = "US/Pacific")
  for (i in 2:t_length) {
    rast_list[[i - 1]] <- terra::rast(tfiles[i]) |>
      terra::subset(layer_names) |>
      terra::crop(nevada_buffer_t)

    time[i - 1] <- terra::time(rast_list[[i - 1]])[1]
  }

  # Anne's method using 6 hourly forecasts. Hires provides 3 hourly.
  # Combine every two forecasts to make things work.
  n_length <- floor((t_length - 1) / 2)
  new_seq <- seq(1, t_length - 1, 2)

  # If new_seq is odd, then we need to cut off the last value
  # to make things work
  if (max(new_seq) == (t_length - 1)) {
    new_seq <- new_seq[-length(new_seq)]
  }

  # Take the sum of 6 hour precip and the max of 6 hour temp.
  precip_list <- temp_list <- vector("list", n_length)
  tind <- 1
  for (i in new_seq) {
    t1_precip <- terra::subset(rast_list[[i]], layer_names[1])
    t2_precip <- terra::subset(rast_list[[i + 1]], layer_names[1])
    t1_temp <- terra::subset(rast_list[[i]], layer_names[2])
    t2_temp <- terra::subset(rast_list[[i + 1]], layer_names[2])
    precip_list[[tind]] <- t1_precip + t2_precip
    temp_list[[tind]] <- max(t1_temp, t2_temp)
    tind <- tind + 1
  }

  # Create a single raster layer for precip and temp and store the time.
  precip_final <- terra::rast(precip_list)
  temp_final <- terra::rast(temp_list)
  time_final <- time[new_seq + 1]

  list(precip_final, temp_final, time_final)
}
