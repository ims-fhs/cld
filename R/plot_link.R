position <- function(x_a, y_a, x_b, y_b, x_c, y_c) {
  ((x_b - x_a) * (y_c - y_a) - (y_b - y_a) * (x_c - x_a))
}

#' curvature
#'
#' @param cld
#'
#' @return
#'
#' @examples
#' cld <- curvature(link_coordinates(import("tests/testthat/mdl/burnout.mdl")))
curvature <- function(cld) {
  assertthat::assert_that(all(c("from_x", "from_y", "to_x", "to_y") %in% colnames(cld)))
  cld$curvature <- NA
  cld$curvature <- position(cld$from_x, cld$from_y, cld$to_x, cld$to_y, cld$x, cld$y)
  cld$curvature <- -(cld$curvature^.1 - 2)
  return(cld)
}


offset_coord <- function(factor) {
  function(from, to) return((to - from)/factor)
}

offset_x <- offset_coord(10)
offset_y <- offset_coord(100)

offset <- function(cld) {
  assertthat::assert_that(all(c("from_x", "from_y", "to_x", "to_y") %in% colnames(cld)))
  # cld$from_x <- cld$from_x + offset_x(cld$from_x, cld$to_x)
  # cld$to_x <- cld$to_x - offset_x(cld$from_x, cld$to_x)
  # cld$from_y <- cld$from_y + offset_y(cld$from_y, cld$to_y)
  # cld$to_y <- cld$to_y - offset_y(cld$from_y, cld$to_y)


  return(cld)
}







#' link_coordinates
#'
#' @param cld
#'
#' @return
#'
#' @examples
link_coordinates <- function(cld) {
  cld$from_x <- NA; cld$from_y <- NA; cld$to_x <- NA; cld$to_y <- NA
  for(i in 1:nrow(cld)) {
    if(cld$type[i] == "link") {
      cld$from_x[i] <- cld$x[cld$id == cld$from[i]]
      cld$from_y[i] <- cld$y[cld$id == cld$from[i]]
      cld$to_x[i] <- cld$x[cld$id == cld$to[i]]
      cld$to_y[i] <- cld$y[cld$id == cld$to[i]]
    }
  }
  return(cld)
}

# StatLink <- ggplot2::ggproto("StatLink",
#                             ggplot2::Stat,
#                             setup_data = function(data, params){
#                               data <- link_coordinates(data)
#                               data <- data[data$type == "link", ]
#                               print(data)
#                             },
#                             compute_group = function(data, scales) {
#                               print(data)
#                             }
# )
#
# stat_link <- function(mapping = NULL, data = NULL, geom = "link",
#                      position = "identity", na.rm = FALSE, show.legend = NA,
#                      inherit.aes = TRUE, ...) {
#   ggplot2::layer(
#     stat = StatLink, data = data, mapping = mapping, geom = geom,
#     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#     params = list(na.rm = na.rm, ...)
#   )
# }
#

# include type as a required aesthetic mapping in GeomLink
GeomLink <- ggplot2::ggproto("GeomLink", ggplot2::GeomCurve,
                            required_aes = c("id", "x", "y", "from", "to", "polarity", "type", "group"),
                            default_aes = ggplot2::aes(id = id, x = x, y = y, from = from, to = to, polarity = polarity, type = type, group = group, colour = "black",
                                                       angle = 0, hjust = 0.5, vjust = 0.5,
                                                       alpha = NA, fontface = 2,
                                                       lineheight = 1.2, length = 10),
                            setup_data = function(data, params){
                              data <- link_coordinates(data)
                              data <- data[data$type == "link", ]
                              print(data)
                            })


geom_link <- function(mapping = ggplot2::aes(x = from_x, y = from_y, xend = to_x, yend = to_y, id = id, from = from, to = to, polarity = polarity, type = type), data = NULL, position = "identity", stat = "identity",
                     ..., curvature = 0.5, angle = 90, ncp = 5, arrow = NULL,
                     arrow.fill = NULL, lineend = "butt", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE)
{
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomLink,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(arrow = arrow, arrow.fill = arrow.fill,
                               curvature = curvature, angle = angle, ncp = ncp,
                               lineend = lineend, na.rm = na.rm, ...))
}
