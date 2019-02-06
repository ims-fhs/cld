#' vertices
#'
#' @param mdl
#'
#' @return
#' @export
#'
#' @examples
#' vertices(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl"))
#' vertices(read_mdl("tests/testthat/mdl/cld-adoption.mdl"))
#' vertices(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl"))
vertices <- function(mdl) {
  mdl <- mdl[mdl[, 1] == 10, ]
  data.frame(id = as.numeric(mdl[, 2]), label = mdl[, 3], x = as.numeric(mdl[, 4]), y = -as.numeric(mdl[, 5]), stringsAsFactors = FALSE)
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
edges <- function(mdl) {
  vertices <- vertices(mdl)
  mdl <- mdl[mdl[, 1] == 1, ]
  edges <- data.frame(id = mdl[, 2], from = mdl[, 3], to = mdl[, 4], polarity = mdl[,7], stringsAsFactors = FALSE)
  edges <- edges[edges$from %in% vertices$id & edges$to %in% vertices$id, ]
  polarity_lut <- data.frame(key = c(0,43,45), type = c("", "+", "-"), stringsAsFactors = FALSE)
  edges$polarity <- as.character(sapply(edges$polarity, function(x){polarity_lut$type[polarity_lut$key == x]}), row.names(NULL))

  return(edges)
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
read_mdl <- function(file) {
  mdl <- readLines(file, encoding = "UTF-8")
  mdl <- mdl[lapply(strsplit(mdl, ","), length) >= 13]
  return(read.table(text = mdl, sep = ",", as.is = TRUE))
}
