#' Create a non-package project, i.e. a manuscript project
#'
#' @param name Manuscript project name.
#' @param path A path. If it exists, it is used. If it does not exist,
#' @param prefix Prefix of the manuscript name.
#' @param lang Language used for the manuscript project.
#' @param use_rproj Logical value, whether initiated with Rstudio project.
#' @param use_gitignore Logical value, whether add the gitignore file.
#' @param use_readme Logical value, whether add the readme md file.
#' @param type Type of the manuscript.
#'
#' @return Path to the newly created project or package, invisibly.
#' @export
#'
use_manuscript <- function(name = "manuscript",
                           path = "/Users/qc0824/Documents/1_Projects/",
                           prefix = NULL,
                           lang = "en",
                           use_rproj = TRUE,
                           use_gitignore = TRUE,
                           use_readme = FALSE,
                           type = "manuscript") {
  # Replace spaces with hyphens in the manuscript name
  manuscript <- gsub(" +", "-", name)

  # Generate the project name prefix based on the name_prefix argument
  if (is.null(prefix)) {
    p_name <- NULL
  } else if (prefix == "date") {
    p_name <- format(Sys.time(), "%Y%m%d")
  } else if (prefix == "rank") {
    p_name <- length(list.files(path, pattern = "Rproj")) + 1
    p_name <- sprintf("%03d", p_name)
  }

  # Add prefix if p_name is not NULL
  if (!is.null(p_name)) {
    manuscript <- paste0(p_name, "_", manuscript)
  }

  # Create the manuscript folder
  root <- fs::path(path, manuscript)
  if (fs::dir_exists(root)) {
    stop("Directory '", manuscript, "' already exists. Aborting.", call. = FALSE)
  } else {
    fs::dir_create(root)
  }

  # Create sub-directories
  fs::dir_create(fs::path(root, "outputs"))
  fs::dir_create(fs::path(root, "qmd_sources"))
  fs::dir_create(fs::path(root, "scripts"))

  # Create formats folder with citation and templates sub-folders
  formats_path <- fs::path(root, "formats")
  fs::dir_create(fs::path(formats_path, "citation"))
  fs::dir_create(fs::path(formats_path, "templates"))

  # Create data directories
  fs::dir_create(fs::path(root, "data", "clean"), recurse = TRUE)
  fs::dir_create(fs::path(root, "data", "raw"), recurse = TRUE)

  # Conditionally create the README
  if (use_readme) {
    copy_internal_file("skeletons", "README.md",
                       dest = root)
  }

  if (type == "manuscript"){
    copy_internal_file("skeletons", "_quarto.yml", dest = root)
    copy_internal_file("skeletons", "header_en.qmd",
                       dest = fs::path(root, "index.qmd"))
  }



  # Initialize the project
  if (use_rproj && rstudioapi::isAvailable()) {
    rstudioapi::initializeProject(root)
  } else if (use_rproj) {
    copy_internal_file("skeletons", "skeletonRproj.Rproj",
                       dest = fs::path(root, paste0(manuscript, ".Rproj"))
    )
  }

  # create .here file
  fs::file_create(fs::path(root, ".here"))

  # Use the gitignore file
  if (use_gitignore) {
    copy_internal_file("skeletons", "skeleton.gitignore",
                       fs::path(root, ".gitignore"))
  }

  # Open the RStudio project
  if (use_rproj && rstudioapi::isAvailable()) {
    rstudioapi::openProject(root)
  } else {
    setwd(root)
  }

  invisible(root)
}

copy_internal_file <- function(folder, file, dest, overwrite = FALSE) {
  f <- system.file(folder, file, package = "manuscript")
  fs::file_copy(f, dest, overwrite = overwrite)
}
