#' Path of the raw or clean data.
#'
#' @rdname path_file
#' @param filename File name
#' @importFrom here here
#' @return Character string contain the path of data.
#' @export
#'
path_raw <- function(filename) {
  here::here("data", "raw", filename)
}

#' @rdname path_file
#' @importFrom here here
#' @export
path_clean <- function(filename) {
  here::here("data", "clean", filename)
}

#' @rdname path_file
#' @importFrom here here
#' @export
path_report <- function(filename) {
  here::here("data", "report", filename)
}

#' @rdname path_file
#' @importFrom here here
#' @export
path_figure <- function(filename) {
  here::here("figures", filename)
}

