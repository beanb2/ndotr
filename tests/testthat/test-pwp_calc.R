test_that("pwp_calc works as expected", {
  # Create a matrix of the different combinations of precip and temperature
  t_precip <- c(0, 5.1, 10.2, 20.4)
  t_temp <- c(0, 0.5, 1.5, 2.8, 4.2)

  # Create a data frame with all combinations
  combinations <- expand.grid(t_precip = t_precip, t_temp = t_temp)

  # Fill the SpatRaster with the values from the combinations
  nrow <- length(t_precip)
  ncol <- length(t_temp)

  values_precip <- matrix(combinations$t_precip, nrow = nrow, byrow = TRUE)
  values_temp <- matrix(combinations$t_temp, nrow = nrow, byrow = TRUE)

  # Create two layers for the SpatRaster
  tlist <- vector("list", 2)
  tlist[[1]]  <- terra::rast(nrows = nrow, ncols = ncol, vals = values_precip)
  tlist[[2]] <- terra::rast(nrows = nrow, ncols = ncol, vals = values_temp)

  test <- pwp_calc(tlist, precip_thresh = c(5, 10.1, 20.3),
                   temp_thresh = c(0.4, 1.4, 2.7, 4.1))

  tmat <- matrix(as.vector(test[[1]]), nrow = 4, ncol = 5, byrow = TRUE)

  # +1 reflects a necessary change so we do not attempt a zero index in the
  # twi function that depends upon this output.
  answer_mat <- matrix(c(rep(0, 5), 1, 2, 2, 0, 2, 2, 3, 0, 2, 3,
                         4, 0, 2, 3, 4) + 1, nrow = 4, ncol = 5, byrow = TRUE)

  expect_equal(tmat, answer_mat)
})
