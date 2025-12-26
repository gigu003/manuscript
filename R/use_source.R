#' Source all R files in a directory
#'
#' @param path The folder path containing the R files. Default is "scripts".
#' @param pattern The pattern used to identify R files. Default is "\\.R$" to match all .R files.
#' @param quiet Logical value. If TRUE, suppress messages. Default is FALSE.
#'
#' @importFrom fs dir_exists
#' @return A vector of the file paths that were sourced.
#' @export
#'
use_source <- function(path = "scripts",
                       pattern = "\\.R$",
                       quiet = TRUE) {
  # Ensure the directory exists
  if (!fs::dir_exists(path)) {
    stop("The specified directory does not exist: ", path)
  }

  # List all the R files in the directory
  r_files <- list.files(path = here::here(path), pattern = pattern, full.names = TRUE)

  # Check if there are any R files to source
  if (length(r_files) == 0) {
    warning("No R files found in the specified directory: ", path)
    return(invisible(NULL))
  }

  # Source each R file and return the list of sourced files
  sapply(r_files, source)

  # Output the number of sourced files if quiet is FALSE
  if (!quiet) {
    message(length(r_files), " files have been sourced:\n", paste(r_files, collapse = "\n"))
  }

}
