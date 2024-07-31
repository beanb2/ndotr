# Info on documenting multiple datasets with one set of documentation:
# - https://stackoverflow.com/questions/57390342/use-roxygen2-to-document-multiple-datasets-in-a-single-documentation-object

#' Nevada State Shapefiles
#'
#' These four sf objects are different variations shapefiles for Nevada.
#'   The "buffer" and "buffer_big" are buffers that allows us to fully resolve
#'   the edges of the state when cropping and reprojecting raster values.
#'   The "sf" and "sf_2" values provide outlines for the state and county
#'   endlines for Nevada.
#' @name nvshape
"nevada_sf"

#' @rdname nvshape
"nevada_sf2"

#' @rdname nvshape
"nevada_buffer"

#' @rdname nvshape
"nevada_buffer_big"

#' Nevada cities and roads.
#'
#' These two sf objects provide outlines for major cities and roads in Nevada
#'   and are used for plot annotations.
#' @name nvpoint
"nevada_cities"

#' @rdname nvshape
"nevada_roads"
