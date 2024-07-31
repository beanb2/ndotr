#' Delete files from a folder
#'
#' Function to clean out all the contents of a folder once finished with it.
#' @param path location of the folder in which to delete all contents.
#' @returns NULL.
clean_folder <- function(path){
  # Steps of function created with chatGPT
  files <- list.files(path, full.names = TRUE, recursive = TRUE)

  # Remove all files and directories
  unlink(files, recursive = TRUE)
}
