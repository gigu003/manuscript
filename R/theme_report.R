#' Apply a custom flextable theme for reports
#'
#' This function applies a standard set of borders to a flextable,
#' including thicker top/bottom borders and optional blank cell borders
#' for specific header cells (e.g., for merging titles).
#'
#' @param ft A `flextable` object.
#' @param blank_i Row index (or indices) of header cells to remove the top border.
#' @param blank_j Column index (or indices) of header cells to remove the top border.
#'
#' @return A `flextable` object with updated border styles.
#' @export
#'
theme_report <- function(
    ft,
    blank_i = NULL,
    blank_j = NULL
    ) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Please install 'flextable'")
  }
  border_thick <- flextable::fp_border_default(width = 1)
  border_thin  <- flextable::fp_border_default(width = 0.5)

  n_head <- flextable::nrow_part(ft, "header")
  n_body <- flextable::nrow_part(ft, "body")
  ft <- flextable::border_remove(ft)
  for (i in 1:n_head) {
    top_border <- if (i == 1) border_thick else border_thin
    ft <- flextable::border(
      ft,
      i = i,
      border.top = top_border,
      part = "header")
  }

  ft <- flextable::border(
    ft,
    i = n_head,
    border.bottom = border_thin,
    part = "header")

  ft <- flextable::border(
    ft,
    i = n_body,
    border.bottom = border_thick,
    part = "body")
  return(ft)
}
