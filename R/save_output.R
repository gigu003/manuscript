#' Save result to data_output folder.
#'
#' @param x Object
#' @param name File name.
#'
#' @importFrom fs dir_exists
#' @importFrom fs dir_create
#'
#' @return Created file.
#' @export
#'
save_output <- function(x, name = "result") {
  # Define the directory path
  path <- "data/output/"

  # Check if the directory exists, if not, create it
  if (!fs::dir_exists(path)) {
    fs::dir_create(path, recurse = TRUE)
  }

  # Create the file path
  file <- paste0(path, name, ".RDS")

  # Save the object to the specified file
  saveRDS(x, file)
}
