StatVar <- ggplot2::ggproto("StatVar",
                            ggplot2::Stat,
                            setup_data = function(data, params){
                              data <- data[data$type == "var", ]
                            },
                            compute_group = function(data, scales, length = 20) {
                              data$label <- sapply(data$label,
                                                   function(x) {paste0(strwrap(x, width = length),
                                                                       collapse = "\n")})
                              data
                            }
)

stat_var <- function(mapping = NULL, data = NULL, geom = "var",
                     position = "identity", na.rm = FALSE, show.legend = FALSE,
                     inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatVar, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


# include type as a required aesthetic mapping in GeomVar
GeomVar <- ggplot2::ggproto("GeomVar", ggplot2::GeomText,
                            required_aes = c("x", "y", "label", "type"),
                            default_aes = ggplot2::aes(x = x, y = y, label = label, type = type, colour = "black",
                                                       size = 10, family = font_var, angle = 0, hjust = 0.5, vjust = 0.5,
                                                       alpha = NA, fontface = "bold",
                                                       lineheight = 1.2, length = 10))


geom_var <- function(mapping = ggplot2::aes(label = label, type = type), data = NULL, position = "identity",
                     ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE,
                     na.rm = FALSE, show.legend = FALSE, inherit.aes = TRUE)
{
  ggplot2::layer(data = data, mapping = mapping, stat = StatVar, geom = GeomVar,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(parse = parse, check_overlap = check_overlap,
                               na.rm = na.rm, ...))
}

