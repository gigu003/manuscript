#' Create one or more directories (recursively if needed).
#'
#' @param ... Character vectors specifying directory paths to create.
#' @param recursive Logical; whether to create parent directories.
#' @param quiet Logical; suppress messages if directories already exist.
#'
#' @return Invisibly returns the created directory paths.
#' @export
create_folder <- function(..., recursive = TRUE, quiet = TRUE) {
  fs::dir_create(..., recurse = recursive, quiet = quiet)
}
