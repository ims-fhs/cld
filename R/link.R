#' link
#'
#' @param cld a data.frame containing CLD information
#'
#' @return
#' @export
#'
#' @examples
#' library(magrittr)
#' cld <- import("tests/testthat/mdl/burnout.mdl")
#' cld %>% link(`energy level`)
#' cld %>% link(`hours` %->%`energy`)
#' (cld %>% link(`hours` %->%`energy`))$group
#' cld %>% link(`hours` %->%`energy`, `perceived` %->% `energy`)
#' cld %>% link(`hours` %->%`energy`, `perceived` %->% `energy`) %>% link(`energy`)
#' (cld %>% link(`hours` %->%`energy`, `perceived` %->% `energy`) %>% link(`energy`))$group
#' cld %>% link(`hours` %->%`energy`) %>% link(`energy`)
#' (cld %>% link(`hours` %->%`energy`) %>% link(`energy`))$group
#' # cld %>% link(`hours` %->%`accomplishments` %->% `perceived` %->% `hours`)
#' cld %>% link(`hours` %->%`accomplishments per week` %->% `perceived` %->% `hours`)
#' sum((cld %>% link(`hours` %->%`accomplishments per week` %->% `perceived` %->% `hours`))$group)
link  <- function(.data, ...) {
  chains <- rlang::enexprs(...)
  indexes <- integer(0)
  for(i in 1:length(chains)) {
    chain <- gsub("\\`", "", rlang::as_label(chains[[i]]))
    indexes <- c(indexes, vars(.data, chain), links(.data, chain))
  }
  .data$group <- group(.data$group, indexes)
  return(.data)
  # in_group <- trimws(strsplit(as.character(dots[[1]])[2], "%->%")[[1]])
  # in_group <- gsub("[\"|\`]", "", in_group)
  # .data$selected <- grepl(paste(in_group, collapse = "|"), .data$label)
  # .data <- select_links(.data)
  # .data <- igraph::delete_vertices(.data, grep(paste(in_group, collapse = "|"), igraph::V(.data)$name, invert = TRUE, value = TRUE))
  # .data[ from = igraph::V(.data)$name[length(in_group)], to = igraph::V(.data)$name[1]] <- 0
}

#' vars
#'
#' @param .data
#' @param chain
#'
#' @return
#'
#' @examples
#' cld <- import("tests/testthat/mdl/burnout.mdl")
#' vars(cld, "hours %->% energy")
#' vars(cld, "energy %->% hours")
#' # vars(cld, "a %->% e")
vars <- function(.data, chain) {
  indexes <- sapply(trimws(unlist(strsplit(chain, "%->%"))), function(i) grep(i, .data$label))
  assertthat::assert_that(!class(indexes) == "list", msg = "Link chain contains ambiguous variable names. Not able to resolve.")
  indexes <- as.numeric(indexes)
  assertthat::assert_that(length(indexes) <= nrow(.data))
  assertthat::assert_that(all(.data$type[indexes] == "var"))
  return(indexes)
}


#' links
#'
#' @param .data
#' @param chain
#'
#' @return
#'
#' @examples
#' cld <- import("tests/testthat/mdl/burnout.mdl")
#' links(cld, "hours %->% energy")
#' links(cld, "perceived %->% energy")
#' links(cld, "energy %->% accomplishments per week")
links <- function(.data, chain) {
  vars <- vars(.data, chain)
  links <- data.frame(from = vars[-length(vars)], to = vars[-1], stringsAsFactors = FALSE)
  links$from <- .data$id[links$from]
  links$to <- .data$id[links$to]
  return(which(.data$from %in% links$from & .data$to %in% links$to))
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
