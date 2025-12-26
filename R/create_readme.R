#' Create README.md files and open them
#'
#' Create README.md files in directories and open them in the editor.
#'
#' @param paths Character vector of directory paths.
#' @param text Character scalar; content written to README.md.
#' @param overwrite Logical; whether to overwrite existing README.md.
#' @param open Logical; whether to open README.md after creation.
#'
#' @return Invisibly returns paths of created README files.
#' @export
create_readme <- function(
    paths,
    text = "# README\n",
    overwrite = FALSE,
    open = TRUE
) {
  paths <- unique(paths)

  fs::dir_create(paths, recurse = TRUE)
  readme_paths <- fs::path(paths, "README.md")

  if (!overwrite) {
    readme_paths <- readme_paths[!fs::file_exists(readme_paths)]
  }

  if (length(readme_paths) == 0) {
    return(invisible(character()))
  }

  writeLines(text, readme_paths)

  if (open && rstudioapi::isAvailable()) {
    for (f in readme_paths) {
      rstudioapi::navigateToFile(f)
    }
  }

  invisible(readme_paths)
}
