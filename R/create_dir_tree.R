#' Generate an overview of a manuscript factory.
#'
#' @param path A path to print the tree from.
#' @param recurse If TRUE recurse fully, if a positive number the number of
#'   levels to recurse.
#' @param ... Arguments passed on to dir_ls.
#'
#' @importFrom fs dir_tree
#' @return Invisibly returns a character of the files and directories within the
#'   desired folder.
#' @export
#'
create_dir_tree <- function(path = ".", recurse = TRUE, ...) {
  fs::dir_tree(path, recurse = recurse, ...)
}
