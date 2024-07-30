#' PWP threshold calculation
#'
#' A function to calculate the Present Weather Potential (PWP)
#'   thresholds as contained in Heggli (2023).
#'
#' @param ras_list A list of rasters, where the first raster is precipitation
#'   forecasts and the second is temperature forecasts. All other elements
#'   of the list will be ignored.
#' @param precip_thresh A vector of 6 hour precipitation total thresholds
#'   for the decision support tool (must be exactly THREE thresholds)
#' @param temp_thresh A vector of 6 hr max temperature thresholds for the
#'   decision support tool (must be exactly FOUR thresholds)
#' @returns A single raster layer of PWP, where the values correspond to
#'   different colors in the visual.
#' @export
pwp_calc <- function(ras_list, precip_thresh = c(5, 10.1, 20.3),
                     temp_thresh = c(0.4, 1.4, 2.7, 4.1)) {
  precip_calc <- terra::app(ras_list[[1]],
                            findInterval,
                            vec = c(-Inf, precip_thresh, Inf)
  )

  temp_calc <- terra::app(ras_list[[2]],
                          findInterval,
                          vec = c(-Inf, temp_thresh, Inf)
  )



  pwp_list <- vector("list", min(c(dim(precip_calc)[3], dim(precip_calc)[3])))

  for (i in seq_len(length(pwp_list))) {
    pwp_list[[i]] <- terra::app(c(precip_calc[[i]], temp_calc[[i]]), lookup_pwp)
  }

  # Convert to single spatraster
  pwp_list <- terra::rast(pwp_list)

  # Carry through the time stamps.
  time(pwp_list) <- time(ras_list[[1]])

  pwp_list
}


# Helper function to assign values from the matrix based on the two
# sets of thresholds. Here x is a placeholder for a spatRaster object
# with exactly two layers.
lookup_pwp <- function(x) {
  # ============================================================================
  # Define the matrix of "color cutoffs" as in Heggli (2023). This is
  # not something that can be changed by the user.
  mat_pwp <- matrix(c(2, 3, 3, 3, 3, 4, 3, 4, 5, 3, 4, 5),
                    nrow = 3, ncol = 4
  )
  # Add a row and columns of zeros for things completely outside the thresholds.
  mat_pwp <- cbind(1, mat_pwp)
  mat_pwp <- rbind(1, mat_pwp)
  # ============================================================================

  mat_pwp[x[1], x[2]]
}
