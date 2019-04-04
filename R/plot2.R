# https://stackoverflow.com/questions/38775661/what-is-the-difference-between-geoms-and-stats-in-ggplot2
# https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
# https://rud.is/books/creating-ggplot2-extensions/demystifying-ggplot2.html

GeomVar <- ggplot2::ggproto("GeomVar", ggplot2::GeomText,
                   default_aes = ggplot2::aes(x = x, y = y, label = label, colour = "black", size = 4, angle = 0, hjust = 0.5, vjust = 0.5, alpha = NA, family = "Arial", fontface = 1, lineheight = 1.2, length = 10)
)
geom_var <- function(mapping = ggplot2::aes(label = label), data = NULL, position = "identity",
                     ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE,
                     na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.",
           call. = FALSE)
    }
    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }
  ggplot2::layer(data = data, mapping = mapping, stat = StatVar, geom = GeomVar,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(parse = parse, check_overlap = check_overlap,
                               na.rm = na.rm, ...))
}



StatVar <- ggplot2::ggproto("StatVar", ggplot2::Stat,
                   # compute_group = function(data, scales) {
                   #   data[chull(data$x, data$y), , drop = FALSE]
                   # },
                   compute_group = function(data, scales, length = 20) {
                     data$label <- sapply(data$label, function(x) {paste0(strwrap(x, width = length), collapse = "\n")})
                   }
)

stat_var <- function(mapping = NULL, data = NULL, geom = "var",
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatVar, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}



#' plot_cld2
#'
#' @param cld a data.frame containing CLD information
#'
#' @return
#' @export
#'
#' @examples
#' # library(cld)
#' library(ggplot2)
#' library(magrittr)
#' cld <- import("tests/testthat/mdl/flexible-arbeitszeiten-part1.mdl")
#' cld %<>% link(`Belastung, wegen`)
#' cld %>% link(Belastung %->% Flexibilisierung)
#' argument(g, "Belastung, wegen" %->% Flexibilisierung) %>% plot_cld()
#' argument(g, Belastung %->% Flexibilisierung %->% Möglichkeit) %>% plot_cld()
#' ggplot(v) + geom_var(aes(x, y))
#' ggplot(cld, aes(x, y)) + geom_var()
#' ggplot(cld, aes(x, y)) + geom_text(label = label)
#' cld[cld$type == "var", ] %>% ggplot() + geom_text(aes(x, y), label = label)
#' ggplot(cld[cld$type == "var", ], aes(x, y)) + geom_var()
#' ggplot(v, aes(x, y)) + geom_var(length = 10)
plot_cld2 <- function(cld) {
  library(ggplot2)
  ggplot(aes(x, y)) + geom_var()
}


plot.cld <- function(cld) {
  hist(1:10)
}
