#' create_igraph
#'
#' @param vertices
#' @param edges
#'
#' @return
#' @export
#'
#' @examples
#' g <- create_igraph(vertices(read_mdl("tests/testthat/mdl/cld-adoption.mdl")), edges(read_mdl("tests/testthat/mdl/cld-adoption.mdl")))
#' create_igraph(vertices(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl")), edges(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl")))
#' create_igraph(vertices(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl")), edges(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl")))
create_igraph <- function(vertices, edges) {
  g <- igraph::make_graph(edges = as.vector(t(edges)))
  igraph::V(g)$name <- vertices$label
  return(g)
}

#' Title
#'
#' @param cld_igraph
#' @param positions
#'
#' @return
#' @export
#'
#' @examples
#' library(magrittr)
#' g <- create_igraph(vertices(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl")), edges(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl"))) %>%
#'   add_positions(positions(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl")))
add_positions <- function(cld_igraph, positions) {
  igraph::V(cld_igraph)$x <- positions$x
  igraph::V(cld_igraph)$y <- positions$y
  return(cld_igraph)
}

add_polarities <- function(cld_igraph, polarity) {
  igraph::E(cld_igraph)$polarity <- polarity
  return(cld_igraph)
}


#' Title
#'
#' @param cld_igraph
#'
#' @return
#' @export
#'
#' @examples
#' g <- create_igraph(vertices(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl")), edges(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl"))) %>%
#'   add_positions(positions(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl"))) %>% add_polarities(polarities(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl")))
#' plot_cld(g)
#' g <- create_igraph(vertices(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl")), edges(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl"))) %>%
#'   add_positions(positions(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl"))) %>% add_polarities(polarities(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl")))
#' plot_cld(g)
#' g <- create_igraph(vertices(read_mdl("tests/testthat/mdl/cld-adoption.mdl")), edges(read_mdl("tests/testthat/mdl/cld-adoption.mdl"))) %>%
#'   add_positions(positions(read_mdl("tests/testthat/mdl/cld-adoption.mdl"))) %>% add_polarities(polarities(read_mdl("tests/testthat/mdl/cld-adoption.mdl")))
#' plot_cld(g)
#' plot_cld(g) + geom_node_text(aes(label = name, x = x, y = y), color = 'red')
#' g <- g %>% add_group(flows = adoption_rate)
#' g <- g %>% add_group(stocks = potential_adopters + adopters)
#' plot_cld(g) + show_group(group = "stocks")
#' plot_cld(g) + show_group(group = "flows")
plot_cld <- function(cld_igraph) {
  library(ggraph)
  ggraph(cld_igraph, layout = 'manual', node.positions = data.frame(x = igraph::V(cld_igraph)$x, y = igraph::V(cld_igraph)$y)) +
    geom_node_text(aes(label = name, x = x, y = y)) +
    geom_edge_arc(aes(label = polarity,
                      start_cap = label_rect(node1.name),
                      end_cap = label_rect(node2.name)),
                  angle_calc = 'along',
                  label_dodge = unit(2.5, 'mm'),
                  arrow = arrow(length = unit(4, 'mm')))
}

show_group <- function(mapping = NULL, data = NULL, group, color = 'red') {
  color <- ifelse(igraph::get.vertex.attribute(g, paste0("group_", group)), color, 'black')
  geom_node_text(aes(label = name, x = x, y = y), color = color)
}
