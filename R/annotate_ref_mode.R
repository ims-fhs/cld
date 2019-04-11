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
  return(annotation_custom(ggplotGrob(eval(parse(text = ref_mode))), xmin = mean(cld$x, na.rm = TRUE), ymin = mean(cld$y, na.rm = TRUE)))
}
