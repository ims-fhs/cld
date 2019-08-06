#' describe is used to further describe CLD elements by adding a description.
#' Descriptions can be made using text or reference modes. Reference modes are
#' specified using the special reference mode notation, see examples
#'
#' @param cld A cld
#' @param type The type of description to add, 'text' or 'ref_mode'
#' @param description
#'
#' @return An updated cld
#' @export
#'
#' @examples
#' cld <- import("tests/testthat/mdl/burnout.mdl")
#' cld %>% link(`hours` %->%`energy`) %>% describe(type = "text", description = "The more hours you work, the less energy you have.")
#' cld %>% link(`hours` %->%`energy`) %>% describe(type = "ref_mode", 0/0 %-% 10/0)
#' cld %>% link(`hours` %->%`energy`) %>% describe(type = "ref_mode", 0/0 %(% 10/0)
#' cld %>% link(`hours` %->%`energy`) %>% describe(type = "ref_mode", 0/0 %(% 10/0)
#' cld %>% link(`hours` %->%`energy`) %>% describe(type = "ref_mode", 0/0 %-% 10/0, labs = "labs(title = \"some title\", x = \"Year\", y = \"Energy\")")
describe <- function(cld, type, description, labs = "labs(title = \"\", x = \"\", y = \"\")") {
  assertthat::assert_that(type %in% c("text", "ref_mode"), msg = paste0("'type' must be 'text' or 'ref_mode', not '", type, "'."))
  assertthat::assert_that("cld" %in% class(cld))
  if(!("description" %in% names(cld))) {
    cld$description <- NA
  }
  if(type == "text") {
    cld <- add_textual_description(cld, description)
  } else {
    str <- deparse(substitute(description), width.cutoff = 200L)
    cld <- add_ref_mode(cld, str, labs)
  }
  assertthat::assert_that("cld" %in% class(cld))
  if(all(cld$division == 1)) cld$division[nrow(cld)] <- 3
  return(cld)
}

#' Internal function. Adds a description of type text to a CLD
#'
#' @param cld A cld
#' @param description A description of type text
#'
#' @return An updated cld
add_textual_description <- function(cld, description) {
  assertthat::assert_that("cld" %in% class(cld))
  description <- data.frame(type = "description_text", description = description, division = max(cld$division), stringsAsFactors = FALSE)
  cld <- merge(cld, description, all = TRUE, sort = FALSE)
  class(cld) <- c("cld", class(cld))
  assertthat::assert_that("cld" %in% class(cld))
  return(cld)
}

#' Internal function. Adds a description of type reference mode to a CLD
#'
#' @param cld A cld
#' @param description A description of type reference mode, provided as string
#'
#' @return An updated cld
add_ref_mode <- function(cld, str, x = "labs(title = \"\", x = \"\", y = \"\")") {
  assertthat::assert_that("cld" %in% class(cld))
  ggplot <- ref_mode_to_ggplot(str)
  x <- paste0("labs(title = \"\", x = \"Zeit\", y = \"", cld$label[cld$type == "var" & cld$division == max(cld$division)][1], "\")")
  ggplot <- paste0(ggplot, " + ", x)
  description <- data.frame(type = "description_ref_mode", description = ggplot, division = max(cld$division), stringsAsFactors = FALSE)
  cld <- merge(cld, description, all = TRUE, sort = FALSE)
  class(cld) <- c("cld", class(cld))
  assertthat::assert_that("cld" %in% class(cld))
  return(cld)
}
