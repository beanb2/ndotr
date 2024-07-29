#' Calculate SNODAS snowpack density
#'
#' A function to determine the density of the snowpack, as calculated from
#'   SNODAS gridded products. Note that the function also excludes
#'   all snowpack from consideration if the SWE is less than 10mm
#'   per the recommendations from Musselmann et al. (2019) regarding rain on
#'   snow events with flood generation potential.
#'
#' @param swe_path The full file path to the snodas raster containing SWE
#'   values. Must include the file extension (e.g., tif, nc, etc.)
#' @param snowd_path The full file path to the snodas raster containing snow
#'   depth values. Must include the file extension (e.g., tif, nc, etc.)
#' @param cropper An sf shapefile used to crop the forecast data to a relevant
#'   spatial extent.
#' @param swe_thresh The amount of SWE required for the ratio calculation
#'   to be relevant. All locations with SWE below this value are assigned
#'   a ratio of zero automatically. Default is the 10mm cited in the Musselmann
#'   et al. (2019) paper.
#' @returns A single layer spatraster containing the snow density percentages
#'   as needed for the ROS TWI decision support tool.
#' @details
#' This package assumes that there will be precisely two raster files stored
#'   in a folder: one containing the SWE values and another containing the
#'   snow depth values on the same day. There should be no other files in this
#'   folder. The function assumes that the file name for the SWE values will
#'   include "SWE" or "swe" in the name and that the snow depth file will
#'   not have "swe" anywhere in its name.
#'
#' @export
snodas_prep <- function(swe_path, snowd_path,
                        cropper = ndotr::nevada_buffer,
                        swe_thresh = 10){
  swe <- terra::rast(swe_path)
  snowd <- terra::rast(snowd_path)

  nevada_buffer_t <- cropper |>
    sf::st_transform(crs = terra::crs(swe)) |>
    terra::vect()

  swe <- terra::crop(swe, nevada_buffer_t)
  snowd <- terra::crop(snowd, nevada_buffer_t)

  # Replace any nominally small values of snow depth with a negative value.
  snowd[snowd < 1e-4] <- -1

  ratio = (swe/snowd) * 100

  # Replace all negative ratios with 0
  ratio[ratio < 0] <- 0
  # Replace all ratios with 0 whenever the snowpack is less than 10mm swe.
  ratio[swe < swe_thresh] <- 0

  ratio
}
