#' ROS TWI threshold calculation
#'
#' A function to calculate the Rain on Snow (ROS) Terrestrial Water Input (TWI)
#'   thresholds as contained in Heggli (2023).
#'
#' @param dens_ras A spatraster object with exactly one layer, representing the
#'   density of the snow, expressed as a percentage of the snow water equivalent
#'   to the snowpack depth.
#' @param pwp_ras A multilayer spatraster object, where each layer represents
#'   the present weather potential (PWP) for a specific 6 hour forecast window
#' @param dens_thresh A vector of snowpack densities, expressed as a
#'   percentage related the snow water equivalent to the snowpack depth.
#' @returns A multilayer spatraster object, where the values correspond to the
#'   colors in the ROS TWI decision support tool in Heggli (2023).
#' @export
twi_calc <- function(dens_ras, pwp_ras,
                     dens_thresh = c(26.5, 27.1, 33.0, 38.1)){

  dens_calc <- terra::app(dens_ras,
                          findInterval, vec = c(-Inf, dens_thresh, Inf))

  twi_list <- vector("list", length(pwp_ras))
  for(i in seq_len(length(twi_list))){
    twi_list[[i]] <- terra::app(c(dens_calc, pwp_ras[[i]]), lookup_twi)
  }

  terra::rast(twi_list)
}


# Helper function to assign values from the matrix based on the two
# sets of thresholds.
lookup_twi <- function(x){
  #=============================================================================
  # Define the matrix of "color cutoffs" as in Heggli (2023). This is
  # not something that can be changed by the user.
  mat_twi <- matrix(c(1, 2, 3, 3, 2, 3, 4, 4, 3, 4, 5, 6, 4, 5, 6, 6),
                    nrow = 4, ncol = 4)

  # Add a row and columns of zeros for things completely outside the thresholds.
  mat_twi <- cbind(0, mat_twi)
  mat_twi <- rbind(0, mat_twi)
  #=============================================================================

  mat_twi[x[1], x[2]]
}
