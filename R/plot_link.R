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

StatLink <- ggplot2::ggproto("StatLink",
                            ggplot2::Stat,
                            setup_data = function(data, params){
                              data <- link_coordinates(data)
                              data <- data[data$type == "link", ]
                              print(data)
                            },
                            compute_group = function(data, scales, length = 5) {
                              data$label <- sapply(data$label,
                                                   function(x) {paste0(strwrap(x, width = length),
                                                                       collapse = "\n")})
                              data
                            }
)

stat_link <- function(mapping = NULL, data = NULL, geom = "link",
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatLink, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


# include type as a required aesthetic mapping in GeomLink
GeomLink <- ggplot2::ggproto("GeomLink", ggplot2::GeomCurve,
                            required_aes = c("id", "x", "y", "from", "to", "polarity", "type", "group"),
                            default_aes = ggplot2::aes(id = id, x = x, y = y, from = from, to = to, polarity = polarity, type = type, group = group, colour = "black",
                                                       size = 4, angle = 0, hjust = 0.5, vjust = 0.5,
                                                       alpha = NA, fontface = 1,
                                                       lineheight = 1.2, length = 10))


geom_link <- function(mapping = ggplot2::aes(id = id, from = from, to = to, polarity = polarity, type = type), data = NULL, position = "identity",
                     ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE,
                     na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
{
  ggplot2::layer(data = data, mapping = mapping, stat = StatLink, geom = GeomLink,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}

