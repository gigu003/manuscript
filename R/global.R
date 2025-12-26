str_to_title_base <- function(x) {
  sapply(strsplit(x, " "), function(words) {
    words <- tolower(words)
    words <- paste0(toupper(substring(words, 1, 1)), substring(words, 2))
    paste(words, collapse = " ")
  }, USE.NAMES = FALSE)
}
