#' describe
#'
#' @param cld
#' @param type
#' @param description
#'
#' @return
#' @export
#'
#' @examples
#' cld <- import("tests/testthat/mdl/burnout.mdl")
#' cld %>% link(`hours` %->%`energy`) %>% describe(type = "text", description = "The more hours you work, the less energy you have.")
#' cld %>% link(`hours` %->%`energy`) %>% describe(type = "ref_mode", 0/0 %-% 10/0)
describe <- function(cld, type, description) {
  assertthat::assert_that(type %in% c("text", "ref_mode"), msg = paste0("'type' must be 'text' or 'ref_mode', not '", type, "'."))
  if(!("description" %in% names(cld))) {
    cld$description <- NA
  }
  if(type == "text") {
    cld <- add_textual_description(cld, description)
  } else {
    str <- rlang::as_label(rlang::enexpr(description))
    cld <- add_ref_mode(cld, str)
  }
  return(cld)
}

add_textual_description <- function(cld, description) {
  description <- data.frame(type = "description_text", description = description, group = max(cld$group), stringsAsFactors = FALSE)
  cld <- merge(cld, description, all = TRUE, sort = FALSE)
  return(cld)
}

add_ref_mode <- function(cld, str) {
  ggplot <- ref_mode_to_ggplot(str)
  description <- data.frame(type = "description_ref_mode", description = ggplot, group = max(cld$group), stringsAsFactors = FALSE)
  cld <- merge(cld, description, all = TRUE, sort = FALSE)
  return(cld)
}
