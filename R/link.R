#' Link cld elements along causal chains. The Causal chains to link are specified
#' usinng the special infix operator `%->%`. More than one causal chains can be
#' linked in one call, when provided comma separated.
#'
#' @param cld a CLD
#'
#' @return An updated CLD
#' @export
#'
#' @examples
#' library(magrittr)
#' cld <- import("tests/testthat/mdl/burnout.mdl")
#' cld %>% link(`energy level`)
#' cld %>% link(`hours` %->% `energy`)
#' (cld %>% link(`hours` %->% `energy`))$division
#' cld %>% link(`hours` %->% `energy`, `perceived` %->% `energy`)
#' cld %>% link(`hours` %->% `energy` %->% `accomplishments per week`)
#' cld %>% link(`hours` %->% `energy`, `perceived` %->% `energy`) %>% link(`energy`)
#' (cld %>% link(`hours` %->% `energy`, `perceived` %->% `energy`) %>% link(`energy`))$division
#' cld %>% link(`hours` %->% `energy`) %>% link(`energy`)
#' (cld %>% link(`hours` %->% `energy`) %>% link(`energy`))$division
#' # cld %>% link(`hours` %->% `accomplishments` %->% `perceived` %->% `hours`)
#' cld %>% link(`hours` %->% `accomplishments per week` %->% `perceived` %->% `hours`)
#' sum((cld %>% link(`hours` %->% `accomplishments per week` %->% `perceived` %->% `hours`))$division)
#' cld <- import("tests/testthat/mdl/flexibilisierung.mdl")
#' cld %>% link(`Belastung` %->% `Flexibilisierung der Arbeit` %->% `Private Dinge während der Arbeit` %->% `Belastung`)
#' (cld %>% link(`Belastung` %->% `Flexibilisierung der Arbeit` %->% `Private Dinge während der Arbeit` %->% `Belastung`))$division
link  <- function(.data, ...) {
  # chains <- rlang::enexprs(...)
  # chains <- substitute(...)
  chains <- match.call(expand.dots = FALSE)$...
  if(!is.null(chains)) {
    indexes <- integer(0)
    for(i in 1:length(chains)) {
      chain <- gsub("\\`", "", deparse(chains[[i]], width.cutoff = 200L))
      # chain <- gsub("\\`", "", rlang::as_label(chains[[i]]))
      indexes <- c(indexes, vars(.data, chain), links(.data, chain))
    }
    .data$division <- group(.data$division, indexes)
  } else {
    .data$division <- .data$division + 1
  }
  indexes <- integer(0)
  return(.data)
}

#' Internal function that identifies the variables present in a causal chain.
#' Used by 'link' and 'links'.
#'
#' @param .data A cld
#' @param chain A causal chain
#'
#' @return The indexes (ids) of the variables in the causal chain
#'
#' @examples
#' cld <- import("tests/testthat/mdl/burnout.mdl")
#' vars(cld, "hours %->% energy")
#' vars(cld, "energy %->% hours")
#' # vars(cld, "a %->% e")
#' cld <- import("tests/testthat/mdl/flexibilisierung.mdl")
#' vars(cld, "Belastung %->% Flexibilisierung der Arbeit %->% Private Dinge während der Arbeit %->% Belastung")
vars <- function(.data, chain) {
  indexes <- sapply(trimws(unlist(strsplit(chain, "%->%"))), function(i) grep(i, .data$label))
  print(class(indexes))
  assertthat::assert_that(!class(indexes) == "list", msg = "Link chain contains ambiguous variable names. Not able to resolve.")
  indexes <- as.numeric(indexes)
  assertthat::assert_that(length(indexes) <= nrow(.data))
  assertthat::assert_that(all(.data$type[indexes] == "var"))
  return(indexes)
}


#' Internal function that identifies the links in a causal chain. Used by 'link'.
#'
#' @param .data A cld
#' @param chain A causal chain, provided as string
#'
#' @return The indexes (ids) of the links that are present in the causal chain
#'
#' @examples
#' cld <- import("tests/testthat/mdl/burnout.mdl")
#' links(cld, "hours %->% energy")
#' links(cld, "perceived %->% energy")
#' links(cld, "energy %->% accomplishments per week")
#' links(cld, "hours %->% energy %->% accomplishments per week")
links <- function(.data, chain) {
  vars <- vars(.data, chain)
  links <- data.frame(from = vars[-length(vars)], to = vars[-1], stringsAsFactors = FALSE)
  links$from <- .data$id[links$from]
  links$to <- .data$id[links$to]
  cond <- logical(nrow(.data))
  for(i in 1:nrow(links)) {
    cond <- cond | .data$from %in% links$from[i] & .data$to %in% links$to[i] & .data$type == "link"
  }
  # matches <- sapply(1:nrow(links), function(x) {.data$from == links$from[x] & .data$to == links$to[x]})
  return(which(cond))
}


#' Internal function that updates the division argument of a cld.
#' 'group' groups the cld elemments specified by 'indexes' so that they form a
#' new division afterward.
#'
#' @param grouping The division column of a cld
#' @param indexes A vector of indexes that form a new group
#'
#' @return The updated division column
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
  assertthat::assert_that(length(new_grouping) == length(grouping))
  return(new_grouping)
}

#' Internal function that updates the division argument of a cld.
#' 'ungroup' resets the division argument for all elements to 1
#'
#' @param grouping The division column of a cld
#'
#' @return The reset division column
#'
#' @examples
#' cld:::ungroup(c(1,2,3))
#' cld:::ungroup(c(2,3))
ungroup <- function(grouping) {
  group(grouping, 1:length(grouping))
}
