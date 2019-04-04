#' vertices
#'
#' @param mdl
#'
#' @return
#' @export
#'
#' @examples
#' vertices(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl"))$label
#' vertices(read_mdl("tests/testthat/mdl/cld-adoption.mdl"))
#' vertices(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl"))
#' vertices(read_mdl("tests/testthat/mdl/flexible-arbeitszeiten.mdl"))
#' vertices(read_mdl("tests/testthat/mdl/cld-comma-and-umlaut.mdl"))$label
vertices <- function(mdl) {
  mdl <- mdl[mdl[, 1] == 10, ]
  data.frame(type = "var", id = as.numeric(mdl[, 2]), label = mdl[, 3], x = as.numeric(mdl[, 4]), y = -as.numeric(mdl[, 5]), stringsAsFactors = FALSE)
}

#' edges
#'
#' @param mdl
#'
#' @return
#' @export
#'
#' @examples
#' edges(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl"))
#' edges(read_mdl("tests/testthat/mdl/cld-adoption.mdl"))
#' edges(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl"))
#' edges(read_mdl("tests/testthat/mdl/flexible-arbeitszeiten-part1.mdl"))
#' edges(read_mdl("tests/testthat/mdl/flexible-arbeitszeiten.mdl"))
edges <- function(mdl) {
  vertices <- vertices(mdl)
  mdl <- mdl[mdl[, 1] == 1, ]
  edges <- data.frame(type = "link", id = mdl[, 2], from = mdl[, 3], to = mdl[, 4], x = mdl[,  14], y = mdl[, 15], polarity = mdl[,7], stringsAsFactors = FALSE)
  edges <- edges[edges$from %in% vertices$id & edges$to %in% vertices$id, ]
  polarity_lut <- data.frame(key = c(0,43,45), type = c("", "+", "-"), stringsAsFactors = FALSE)
  edges$polarity <- as.character(sapply(edges$polarity, function(x){polarity_lut$type[polarity_lut$key == x]}), row.names(NULL))
  edges$x <- as.numeric(sapply(edges$x, function(x){sub("1[|][(]", "", x)}))
  edges$y <- -as.numeric(sapply(edges$y, function(x){sub("[)][|]", "", x)}))
  return(edges)
}


loops <- function(mdl) {
  mdl <- mdl[mdl[, 1] == 12 & mdl[, 3] %in% c(1,2), ]
  loops <- data.frame(type = "loop", id = mdl[, 2], x = as.numeric(mdl[, 4]), y = -as.numeric(mdl[, 5]), polarity = mdl[,8], direction = mdl[,3], stringsAsFactors = FALSE)
  polarity_lut <- data.frame(key = c(4, 5), type = c("B", "R"), stringsAsFactors = FALSE)
  loops$polarity <- as.character(sapply(loops$polarity, function(x){polarity_lut$type[polarity_lut$key == x]}), row.names(NULL))
  direction_lut <- data.frame(key = c(1, 2), type = c("clock", "counter"), stringsAsFactors = FALSE)
  loops$direction <- as.character(sapply(loops$direction, function(x){direction_lut$type[direction_lut$key == x]}), row.names(NULL))
  return(loops)
}

#' read_mdl
#'
#' @param file path to a valid Vensim .mdl file containing a CLD
#'
#' @return mdl a character vector containing the relevant (sketch) information.
#' @export
#'
#' @examples
#' read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl")
#' read_mdl("tests/testthat/mdl/cld-adoption.mdl")
#' read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl")
#' read_mdl("tests/testthat/mdl/cld-comma-and-umlaut.mdl")
#' read_mdl("tests/testthat/mdl/flexible-arbeitszeiten.mdl")
read_mdl <- function(file) {
  mdl <- readLines(file, encoding = "UTF-8")
  mdl <- mdl[lapply(strsplit(mdl, ","), length) >= 13]
  return(read.table(text = mdl, sep = ",", as.is = TRUE))
}

#' import
#'
#' @param file path to a valid Vensim .mdl file containing a CLD
#'
#' @return cld
#' @export
#'
#' @examples
#' import("tests/testthat/mdl/cld-2nodes-1edge.mdl")
#' import("tests/testthat/mdl/cld-adoption.mdl")
#' import("tests/testthat/mdl/cld-shifting-the-burden.mdl")
#' import("tests/testthat/mdl/cld-comma-and-umlaut.mdl")
#' import("tests/testthat/mdl/burnout.mdl")
import <- function(file) {
  mdl <- read_mdl(file)
  vertices <- vertices(mdl)
  edges <- edges(mdl)
  cld <- merge(vertices, edges, all = TRUE)
  cld$from <- as.numeric(cld$from)
  cld$group <- 0L
  class(cld) <- c("cld", class(cld))
  assertthat::assert_that(nrow(cld) >= 3)
  assertthat::assert_that(ncol(cld) == 9)
  assertthat::assert_that(!any(grepl("\"", cld$label)))
  return(cld)
}
