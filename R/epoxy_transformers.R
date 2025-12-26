#' Manuscript Epoxy Transformers List
#'
#' A list of custom transformer functions that can be used with `epoxy::epoxy_transform_set()`.
#' This is provided for optional integration with the epoxy package.
#'
#' @format A named list of functions.
#' @export
epoxy_transformers <- function(){
  list(
    .per = function(x) paste0(x, "%"),
    .percent = function(x) paste0(formatC(x * 100, format = "f", digits = 2), "%"),
    .and_en = function(x) {
      glue::glue_collapse(x, sep = ", ", last = lang$and[1])
    },
    .and_cn = function(x) {
      glue::glue_collapse(x, sep = ", ", last = lang$and[2])
    },
    .rate_en = function(x) {
      paste0(
        formatC(x, format = "f", digits = 2),
        lang$per_100_thousand[1]
      )
    },
    .rate_cn = function(x) {
      paste0(
        formatC(x, format = "f", digits = 2),
        lang$per_100_thousand[2]
      )
    },
    .cancer_en = function(x) paste0(x, lang$cancer[1]),
    .cancer_cn = function(x) {
      ifelse(x %in% lang$not_cancer, x, paste0(x, lang$cancer[2]))
    },
    .digit2 = function(x) formatC(x, format = "f", digits = 2),
    .tolower = function(x) tolower(x),
    .space = function(x) {
      function(x) prettyNum(round(x), big.mark = " ")
    },
    .rank_en = function(x) paste0(x, "^th^"),
    .rank_cn = function(x) paste0("\u7b2c", x, "\u4f4d")
  )
}
