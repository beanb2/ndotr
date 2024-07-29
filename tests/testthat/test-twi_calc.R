test_that("twi_calc works as expected", {
  # Create a matrix of the different combinations of precip and temperature
  t_pwp <- 1:5
  t_dens <- c(0, 26.6, 27.2, 33.1, 38.2)

  # Create a data frame with all combinations
  combinations <- expand.grid(t_pwp = t_pwp, t_dens = t_dens)

  # Fill the SpatRaster with the values from the combinations
  nrow <- length(t_pwp)
  ncol <- length(t_dens)

  values_pwp <- matrix(combinations$t_pwp, nrow = nrow, byrow = TRUE)
  values_dens <- matrix(combinations$t_dens, nrow = nrow, byrow = TRUE)

  # Create two layers for the SpatRaster
  tlist <- vector("list", 2)
  tlist[[1]] <- terra::rast(nrows = nrow, ncols = ncol, vals = values_dens)
  tlist[[2]]  <- terra::rast(nrows = nrow, ncols = ncol, vals = values_pwp)


  test <- twi_calc(tlist[[1]], tlist[[2]],
                   dens_thresh = c(26.5, 27.1, 33.0, 38.1))

  tmat <- matrix(as.vector(test[[1]]), nrow = 5, ncol = 5, byrow = TRUE)

  answer_mat <- matrix(c(rep(0, 5), 0, 1:4, 0, 2:5, 0, 3:6, 0, 3, 4, 6, 6),
                       nrow = 5, ncol = 5, byrow = TRUE)

  expect_equal(tmat, answer_mat)
})
