add_group <- function(.data, ...) {
  dots <- quos(...)
  for (i in 1:length(dots)) {
    in_group <- as.character(dots[[i]])[2]
    vec <- gsub("_", " ", trimws(unlist(strsplit(in_group, "[+]"))))
    .data <- igraph::set_vertex_attr(.data, name = paste0("group_", names(dots[i])), value = sapply(names(igraph::V(.data)), function(x){x %in% vec}))
  }
  return(.data)
}
