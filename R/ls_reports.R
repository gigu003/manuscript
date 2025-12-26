#' List R Markdown and Quarto Reports
#'
#' @param path The root directory where the search starts. Defaults to the current directory.
#' @param manuscript_sources The subdirectory within `path` where reports are located.
#' @param pattern An optional regex pattern to filter the listed reports.
#' @param ... Additional arguments passed to `grep()`.
#'
#' @importFrom fs dir_exists
#' @return A character vector of file paths to R Markdown and Quarto reports.
#' @export
#'
ls_reports <- function(path = ".", manuscript_sources = "qmd_sources", pattern = NULL, ...) {

  # Construct the full path to the manuscript_sources directory
  full_path <- file.path(path, manuscript_sources)

  # Check if the directory exists
  if (!dir_exists(full_path)) {
    stop("The directory '", full_path, "' does not exist.")
  }

  # List all Rmd and Qmd files in the directory recursively
  out <- list.files(
    full_path,
    pattern = "\\.[RrQq]md$",
    recursive = TRUE
  )

  # Filter with grep if a pattern is provided
  if (!is.null(pattern)) {
    out <- grep(pattern, out, value = TRUE, ...)
  }

  # Return the list of files
  return(out)
}
