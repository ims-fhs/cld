#' link
#'
#' @param cld a data.frame containing CLD information
#'
#' @return
#' @export
#'
#' @examples
#' library(magrittr)
#' cld <- import("tests/testthat/mdl/flexible-arbeitszeiten-part1.mdl")
#' cld %>% link(`Belastung, wegen`)
#' cld %>% link(Belastung %->% Flexibilisierung)
#' argument(g, "Belastung, wegen" %->% Flexibilisierung) %>% plot_cld()
#' argument(g, Belastung %->% Flexibilisierung %->% Möglichkeit) %>% plot_cld()
link  <- function(.data, ...) {
  browser()
  dots <- rlang::quos(...)
  in_group <- trimws(strsplit(as.character(dots[[1]])[2], "%->%")[[1]])
  in_group <- gsub("[\"|\`]", "", in_group)
  .data$selected <- grepl(paste(in_group, collapse = "|"), .data$label)
  .data <- select_links(.data)
  # .data <- igraph::delete_vertices(.data, grep(paste(in_group, collapse = "|"), igraph::V(.data)$name, invert = TRUE, value = TRUE))
  # .data[ from = igraph::V(.data)$name[length(in_group)], to = igraph::V(.data)$name[1]] <- 0
  .data
}

#' select_links
#'
#' @param .data
#'
#' @return
#'
#' @examples
select_links <- function(.data) {
  vars <- .data[.data$type == "var" & .data$selected, ]
  links <- .data[.data$type == "link", ]
  browser()
  for(i in seq_along(vars$id)) {
    var_links <- links[links$from == i, ]
    var_links <- var_links[var_links$to %in% vars$id, ]
    if ( nrow(var_links) > 0) {
      var_links$selected <- TRUE
      .data[.data$id == var_links$id, ] <- var_links

    }
  }
  return(.data)
}


#' group
#'
#' @param grouping
#' @param indexes A vector of indexes that form a new group
#'
#' @return
#'
#' @examples
#' cld:::group(c(1,1,1,1), c(2,3))
#' cld:::group(c(1,2,2,1,1), c(1,4))
#' # cld:::group(c(1,2,2,1,1), c(6))
#' # cld:::group(c(1,2,2,1,1), c(0))
#' cld:::group(c(1,2), c(1,2))
group <- function(grouping, indexes) {
  assertthat::assert_that(is.numeric(grouping))
  assertthat::assert_that(is.numeric(indexes))
  assertthat::assert_that(max(indexes) <= length(grouping))
  new_grouping <- grouping
  new_grouping[indexes] <- max(new_grouping) + 1L
  assertthat::assert_that(sum(new_grouping) > sum(grouping))
  new_grouping <- new_grouping - (min(new_grouping - 1))
  return(new_grouping)
}

#' ungroup
#'
#' @param grouping
#'
#' @return
#'
#' @examples
#' cld:::ungroup(c(1,2,3))
ungroup <- function(grouping) {
  group(grouping, 1:length(grouping))
}
