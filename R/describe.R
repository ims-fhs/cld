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
#' cld %>% link(`hours` %->%`energy`) %>% describe(type = "ref_mode", 0/0 %-% 10/0, labs = "labs(title = \"some title\", x = \"Year\", y = \"Energy\")")
describe <- function(cld, type, description, labs = "labs(title = \"\", x = \"\", y = \"\")") {
  assertthat::assert_that(type %in% c("text", "ref_mode"), msg = paste0("'type' must be 'text' or 'ref_mode', not '", type, "'."))
  if(!("description" %in% names(cld))) {
    cld$description <- NA
  }
  if(type == "text") {
    cld <- add_textual_description(cld, description)
  } else {
    str <- deparse(substitute(description), width.cutoff = 200L)
    cld <- add_ref_mode(cld, str, labs)
  }
  return(cld)
}

add_textual_description <- function(cld, description) {
  description <- data.frame(type = "description_text", description = description, group = max(cld$group), stringsAsFactors = FALSE)
  cld <- merge(cld, description, all = TRUE, sort = FALSE)
  return(cld)
}

add_ref_mode <- function(cld, str, x = "labs(title = \"\", x = \"\", y = \"\")") {
  ggplot <- ref_mode_to_ggplot(str)
  ggplot <- paste0(ggplot, " + ", x)
  description <- data.frame(type = "description_ref_mode", description = ggplot, group = max(cld$group), stringsAsFactors = FALSE)
  cld <- merge(cld, description, all = TRUE, sort = FALSE)
  return(cld)
}
