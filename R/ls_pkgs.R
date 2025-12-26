#' List all R packages used in R scripts.
#'
#' @param path Path of the R files or folders contains R files.
#' @param include_install Logical value, whether list installed packages or not.
#' @importFrom fs is_file
#' @importFrom fs is_dir
#' @return A character vector contains R package names.
#' @export
#'
ls_pkgs <- function(path = "scripts", include_install = FALSE) {

  if (all(fs::is_file(path))) {
    packages <- unlist(lapply(path, pkgs))
    } else if (all(fs::is_dir(path))) {
      r_files <- lapply(path, list.files, pattern = ".R", full.names = TRUE)
      r_files <- unlist(r_files)
      packages <- unlist(lapply(r_files, pkgs))
    }
  packages <- unique(packages)
  # find the uninstalled packages
  if (!include_install) {
    installed <- basename(find.package(packages, quiet = TRUE))
    packages <- setdiff(packages, installed)
  }
  return(packages)
}

pkgs <- function(file){
  con <- file(file, "r")
  packages <- character()
  while(TRUE) {
    line <- readLines(con, n = 1, warn = FALSE)
    if (length(line) == 0) {
      break
    }
    if (grepl("library\\(|require\\(", line)) {
      pkg <- gsub(".*library\\(|require\\(", "", line)
      pkg <- gsub("\\).*", "", pkg)
      pkg <- gsub("\\'|\"", "", pkg)
      packages <- c(packages, pkg)
    }
  }
  close(con)
  return(unique(packages))
}
