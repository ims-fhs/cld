format_wrap_labels <- function(.data, length = 30) {
  igraph::V(.data)$name <- sapply(igraph::V(.data)$name, function(x) {paste0(strwrap(x, width = length), collapse = "\n")})
  return(.data)
}
