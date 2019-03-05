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
#' create_igraph(vertices(read_mdl("tests/testthat/mdl/flexible-arbeitszeiten.mdl")), edges(read_mdl("tests/testthat/mdl/flexible-arbeitszeiten.mdl")))
create_igraph <- function(vertices, edges) {
  g <- igraph::make_graph(edges = as.vector(t(edges[with(edges, order(from, to)), ][2:3])))
  vertices <- vertices[with(vertices, order(igraph::V(g)$name)), ]
  igraph::V(g)$name <- vertices$label
  igraph::V(g)$x <- vertices$x
  igraph::V(g)$y <- vertices$y
  igraph::E(g)$polarity <- edges[with(edges, order(from, to)), ]$polarity
  return(g)
}


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
#' g <- create_igraph(vertices(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl")), edges(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl")))
#' plot_cld(g)
#' g <- create_igraph(vertices(read_mdl("tests/testthat/mdl/cld-comma-and-umlaut.mdl")), edges(read_mdl("tests/testthat/mdl/cld-comma-and-umlaut.mdl")))
#' plot_cld(g)
#' # Shifting the burden
#' g <- create_igraph(vertices(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl")), edges(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl")))
#' plot_cld(g)
#' g <- g %>% add_group(internal = capabilities_of_internal_actors + internal_solution)
#' g <- g %>% add_group(external = external_intervention)
#' plot_cld(g) + show_group(group = "internal")
#' plot_cld(g) + show_group(group = "external")
#' g <- g %>% add_scenario(outsourcing = c(0.25,0.75,0.25,0.5))
#' g <- g %>% add_scenario(insourcing = c(0.25,0.25,0.75,0.75))
#' # Adoption
#' g <- create_igraph(vertices(read_mdl("tests/testthat/mdl/cld-adoption.mdl")), edges(read_mdl("tests/testthat/mdl/cld-adoption.mdl")))
#' plot_cld(g)
#' plot_cld(g) + geom_node_text(aes(label = name, x = x, y = y), color = 'red')
#' g <- g %>% add_group(flows = adoption_rate)
#' g <- g %>% add_group(stocks = potential_adopters + adopters)
#' plot_cld(g) + show_group(group = "stocks")
#' plot_cld(g) + show_group(group = "flows")
#' # Flexible Arbeitszeiten
#' g <- create_igraph(vertices(read_mdl("tests/testthat/mdl/flexible-arbeitszeiten.mdl")), edges(read_mdl("tests/testthat/mdl/flexible-arbeitszeiten.mdl")))
#' plot_cld(g)
#' plot_cld(g %>% argument("Belastung, wegen" %->% Flexibilisierung %->% "Möglichkeit private Dinge während"))
plot_cld <- function(cld_igraph) {
  library(ggraph)
  if (length(igraph::E(cld_igraph)) > 0) {
    ggraph(cld_igraph, layout = 'manual', node.positions = data.frame(x = igraph::V(cld_igraph)$x, y = igraph::V(cld_igraph)$y)) +
      geom_node_text(aes(label = name, x = x, y = y, family = "Comic Sans MS")) +
      geom_edge_arc(aes(label = polarity,
                        start_cap = label_rect(node1.name),
                        end_cap = label_rect(node2.name)),
                    angle_calc = 'along',
                    label_dodge = unit(2.5, 'mm'),
                    arrow = arrow(length = unit(4, 'mm'))) +
      xlim(200, 1200) +
      ylim(-600, 0) +
      theme_void()

  } else {
    ggraph(cld_igraph, layout = 'manual', node.positions = data.frame(x = igraph::V(cld_igraph)$x, y = igraph::V(cld_igraph)$y)) +
      geom_node_text(aes(label = name, x = x, y = y, family = "Comic Sans MS")) +
      xlim(200, 1200) +
      ylim(-600, 0) +
      theme_void()
  }
}

show_group <- function(mapping = NULL, data = NULL, group, color = 'red') {
  color <- ifelse(igraph::get.vertex.attribute(cld, paste0("group_", group)), color, 'black')
  geom_node_text(aes(label = name, x = x, y = y), color = color)
}


#' Title
#'
#' @param cld_igraph
#' @param scenario
#' @param color
#'
#' @return
#' @export
#'
#' @examples
#' # Shifting the burden
#' g <- create_igraph(vertices(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl")), edges(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl"))) %>%
#'   add_positions(positions(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl"))) %>% add_polarities(polarities(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl")))
#' g <- g %>% add_scenario(outsourcing = c(0.25,0.75,0.25,0.5))
#' g <- g %>% add_scenario(insourcing = c(0.25,0.25,0.75,0.75))
#' plot_scenario(cld_igraph = g, scenario = "scenario_insourcing")
#' plot_scenario(g, "scenario_insourcing", color = "black") %>%  add_trace(
#' r = igraph::get.vertex.attribute(g, "scenario_outsourcing"),
#' theta = names(igraph::V(g)),
#' name = "scenario_outsourcing",
#' marker = list(symbol = 1,
#'               size = 10,
#'               color = 'blue'),
#' fill = 'toself', fillcolor = col2rgb('blue', 0.1))
plot_scenario <- function(cld_igraph, scenario, color = 'black') {
  library(plotly)
  plot_ly(
    type = 'scatterpolar',
    mode = 'marker'
  ) %>% add_trace(
      r = igraph::get.vertex.attribute(cld_igraph, scenario),
      theta = names(igraph::V(cld_igraph)),
      name = scenario,
      marker = list(symbol = 1,
                    size = 10,
                    color = color),
      fill = 'toself', fillcolor = col2rgb(color, 0.1)) %>% config(displayModeBar = F) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,1),
          tickvals = c(.25, .75),
          ticktext = c("min", "max")
        )
      )
    )
}

#' Title
#'
#' @param cld_igraph
#'
#' @return
#' @export
#'
#' @examples
#' # Shifting the burden
#' g <- create_igraph(vertices(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl")), edges(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl"))) %>%
#'   add_definition(problem_symptom = "The problem as seen by the...", external_intervention = "Amount of external action...")
#' table_definitions(g)
table_definitions <- function(cld_igraph) {
  data.frame(variable_names = names(igraph::V(cld_igraph)), definitions = igraph::V(cld_igraph)$definition, stringsAsFactors = FALSE)
}

#' Title
#'
#' @param cld_igraph
#' @param ref_mode
#'
#' @return
#' @export
#'
#' @examples
#' # Shifting the burden
#' g <- create_igraph(vertices(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl")), edges(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl"))) %>%
#'   add_ref_mode(problem_symptom_hope = data.frame(time = c(0, 5,10,20,40, 100), value = c(0, 1,.9,.3,.1, 0)))
#' plot_ref_mode(g, "problem_symptom_hope")
plot_ref_mode <- function(cld_igraph, ref_mode) {
  df <- eval(parse(text = igraph::get.graph.attribute(cld_igraph, ref_mode)))
  library(ggplot2)
  names(df) <- c("x", "y")
  ggplot(df, aes(x, y)) +
    geom_step(data = df[1:2,], color = 'blue', size = 1) +
    geom_smooth(data = df[-1,], method = lm, formula = y ~ splines::bs(x, 4), se = FALSE, color = 'blue', size = 1) +
    coord_cartesian(xlim = c(0,40))
}
