# https://stackoverflow.com/questions/38775661/what-is-the-difference-between-geoms-and-stats-in-ggplot2
# https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
# https://rud.is/books/creating-ggplot2-extensions/demystifying-ggplot2.html



#' plot
#'
#' @param cld a data.frame containing CLD information
#'
#' @import ggplot2
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' cld <- import("tests/testthat/mdl/burnout.mdl")
#' vars <- cld[cld$type == "var", ]
#' ggplot(as.data.frame(vars), aes(x, y)) + geom_text(aes(label = label)) + theme_void()
#' ggplot(as.data.frame(cld), aes(x, y)) + geom_text(aes(label = label)) + theme_void()
#' ggplot(as.data.frame(cld), aes(x, y)) + geom_var() + theme_void()
#' ggplot(as.data.frame(cld), aes(x, y)) + geom_var() + geom_link() + theme_void()
#' plot(cld)
plot.cld <- function(cld) {
  cld <- as.data.frame(cld)
  ggplot(data = cld, aes(x, y)) + geom_var() + theme_void()
}
