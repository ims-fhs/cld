#' Title
#'
#' @param cld_igraph
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(magrittr)
#' path <- "tests/testthat/mdl/flexible-arbeitszeiten-part1.mdl"
#' g <- create_igraph(vertices(read_mdl(path)), edges(read_mdl(path)))
#' argument(g, Belastung) %>% plot_cld()
#' argument(g, Belastung %>% Flexibilisierung) %>% plot_cld()
#' argument(g, Belastung %>% Flexibilisierung %>% Möglichkeit) %>% plot_cld()
argument  <- function(.data, ...) {
  dots <- quos(...)
  in_group <- trimws(strsplit(as.character(dots[[1]])[2], "%>%")[[1]])
  .data <- igraph::delete_vertices(.data, grep(paste(in_group, collapse = "|"), igraph::V(.data)$name, invert = TRUE, value = TRUE))
  # .data[ from = igraph::V(.data)$name[length(in_group)], to = igraph::V(.data)$name[1]] <- 0
  .data
}
