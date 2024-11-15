% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hires_prep.R
\name{hires_prep}
\alias{hires_prep}
\title{NOAA NAM Hires Prep}
\usage{
hires_prep(
  path,
  type = "grib2",
  cropper = ndotr::nevada_buffer,
  layer_names = c(paste("SFC=Ground or water surface;", "03 hr Total precipitation",
    "[kg/(m^2)]"), paste("SFC=Ground or water surface;", "Temperature [C]"))
)
}
\arguments{
\item{path}{File path containing a folder of NAM hires forecast rasters}

\item{type}{File type (should be a raster file type such as nc, grib2,
tif, etc.)}

\item{cropper}{An sf shapefile used to crop the forecast data to a relevant
spatial extent.}

\item{layer_names}{A vector of layer names (see NAM hires files and
documentation for details). The first layer name must correspond to precip
and the second must correspond to temperature.}
}
\value{
A list of three including (1) a raster of 6 hour precipitation
forecasts, (2) a raster of 6 hour maximum temperature, and (3) a vector
of timestamps as obtained from the raster files.
}
\description{
Function to prepare NOAA NAM Hires forecast data for inclusion in the TWI
decision support tool.
}
