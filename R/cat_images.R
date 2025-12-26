cat_images <- function(folder) {
  images <- fs::dir_ls(folder, recurse = TRUE)
  images <- sort(images)
  glue::glue("![]({images})\n\n")
}
