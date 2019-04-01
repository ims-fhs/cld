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
  .data <- igraph::delete_vertices(.data, grep(paste(in_group, collapse = "|"), igraph::V(.data)$name, invert = TRUE, value = TRUE))
  # .data[ from = igraph::V(.data)$name[length(in_group)], to = igraph::V(.data)$name[1]] <- 0
  .data
}

select_links <- function(.data) {
  vars <- .data[.data$type == "var" & .data$selected, ]
  links <- .data[.data$type == "link", ]
  browser()
  for(i in seq_along(vars)) {
    var_links <- links[links$from == vars$id[i], ]
    var_links <- var_links[var_links$to %in% vars$id, ]
    var_links$selected <- TRUE
    .data[.data$id == var_links$id, ] <- var_links
    }
}
