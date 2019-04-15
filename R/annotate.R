#' annotate_ref_mode
#'
#' @param gg
#' @param cld
#'
#' @return
#' @import ggplot2
#'
#' @examples
#' library(ggplot2)
#' cld <- import("tests/testthat/mdl/burnout.mdl")
#' ggplot(data = as.data.frame(cld), aes(x, y)) + geom_text(aes(label = label))
#' # annotate_ref_mode(ggplot2::ggplot(aes(x, y)), cld)
#' cld <- cld %>% describe(type = "ref_mode", 0/0 %-% 1/1)
#' cld <- as.data.frame(cld)
#' gg <- ggplot(data = cld, aes(x, y)) + geom_text(aes(label = label))
#' gg
#' gg + annotate_ref_mode(gg, cld)
annotate_ref_mode <- function(gg, cld) {
  ref_mode <- cld$description[cld$type == "description_ref_mode"]
  assertthat::assert_that(length(ref_mode) == 1, msg = paste0("You provided ", length(ref_mode), " ref_modes, whereas annotate_ref_mode needs exactly 1 ref_mode."))
  return(ggplot2::annotation_custom(ggplot2::ggplotGrob(eval(parse(text = ref_mode))), xmin = mean(cld$x, na.rm = TRUE), ymin = mean(cld$y, na.rm = TRUE)))
}


#' annotate_text
#'
#' @param gg
#' @param cld
#'
#' @return
#' @import ggplot2
#'
#' @examples
#' library(ggplot2)
#' cld <- import("tests/testthat/mdl/burnout.mdl")
#' ggplot(data = as.data.frame(cld), aes(x, y)) + geom_text(aes(label = label))
#' annotate_text(ggplot2::ggplot(aes(x, y)), cld)
#' cld <- cld %>% describe(type = "text", "Some descriptions of what we see.")
#' cld <- as.data.frame(cld)
#' gg <- ggplot(data = cld, aes(x, y)) + geom_text(aes(label = label))
#' gg
#' gg + annotate_text(gg, cld)
annotate_text <- function(gg, cld) {
  col <- cld$division[cld$type == "description_text"]
  text <- cld$description[cld$type == "description_text"]
  assertthat::assert_that(length(text) == 1, msg = paste0("You provided ", length(text), " textual descriptions, whereas annotate_text needs exactly 1 description."))
  return(ggplot2::annotate("text", x = mean(cld$x, na.rm = TRUE), y = min(cld$y, na.rm = TRUE), label =  text, colour = cp[col], size = 8, family = font_description))
}

annotate_polarity <- function(gg, cld) {
  cld <- cld[cld$type == "link", ]
  return(ggplot2::annotate("text", x = cld$x, y = cld$y, label =  cld$polarity, colour = cp[cld$division], family = font_var))
}
