add_group <- function(.data, ...) {
  dots <- quos(...)
  for (i in 1:length(dots)) {
    in_group <- as.character(dots[[i]])[2]
    vec <- gsub("_", " ", trimws(unlist(strsplit(in_group, "[+]"))))
    .data <- igraph::set_vertex_attr(.data, name = paste0("group_", names(dots[i])), value = sapply(names(igraph::V(.data)), function(x){x %in% vec}))
  }
  return(.data)
}

add_scenario <- function(.data, ...) {
  dots <- quos(...)
  for (i in 1:length(dots)) {
    .data <- igraph::set_vertex_attr(.data, name = paste0("scenario_", names(dots[i])), value = eval(parse(text = as.character(dots[[i]][2]))))
  }
  return(.data)
}
