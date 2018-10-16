#' Title
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
  labels <- lhs(filter_functional_definitions(mdl), "= A FUNCTION OF")
  mdl <- mdl[sapply(mdl, function(x){grepl(paste(labels, collapse = "|"), x)})]
  mdl <- mdl[!grepl("A FUNCTION OF", mdl)]
  vertices <- stringr::str_split_fixed(mdl, ",", 4)[,2:3]
  data.frame(id = as.numeric(vertices[,1]), label = vertices[,2], stringsAsFactors = FALSE)
}

#' Title
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
edges <- function(mdl) {
  vertices <- vertices(mdl)
  mdl <- filter_functional_definitions(mdl)
  to <- lhs(mdl, "= A FUNCTION OF")
  to <- sapply(to, function(x){vertices$id[vertices$label == x]})

  from <- rhs(mdl, "= A FUNCTION OF")
  from <- from[from != ""]
  from <- strsplit(from, ",")
  to <- rep(to, lengths(from))
  from <- unlist(from)
  from <- sapply(from, function(x){vertices$id[vertices$label == x]})

  edges <- data.frame(from = from, to = to, stringsAsFactors = FALSE)
  edges[from != to, ]
}

read_mdl <- function(file) {
  mdl <- read.csv(file, sep = ";", stringsAsFactors = FALSE)
  return(mdl[!sapply(mdl, function(x){grepl(":", x)}), ])
}

filter_functional_definitions <- function(mdl) {
  mdl[sapply(mdl, function(x){grepl("A FUNCTION OF", x)})]
}

xhs <- function(element) {
  function(data, sep) {
    trimws(gsub("[()]", " ", sapply(strsplit(data, sep), function(x){x[element]})))
  }
}

lhs <- xhs(1)
rhs <- xhs(2)

#' Title
#'
#' @param mdl
#'
#' @return
#' @export
#'
#' @examples
#' positions(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl"))
#' positions(read_mdl("tests/testthat/mdl/cld-adoption.mdl"))
#' positions(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl"))
positions <- function(mdl) {
  vertices <- vertices(mdl)
  mdl <- mdl[sapply(mdl, function(x){grepl(paste(vertices$label, collapse = "|"), x)})]
  mdl <- mdl[!grepl("A FUNCTION OF", mdl)]
  names <- sapply(strsplit(mdl, ","), function(x)x[[3]])
  x <- as.numeric(sapply(strsplit(mdl, ","), function(x)x[[4]]))
  y <- -as.numeric(sapply(strsplit(mdl, ","), function(x)x[[5]]))
  df <- data.frame(label = names, x = x, y = y, stringsAsFactors = FALSE)
  return(df[c("x", "y")])
}

#' Title
#'
#' @param mdl
#'
#' @return
#' @export
#'
#' @examples
#' polarities(read_mdl("tests/testthat/mdl/cld-2nodes-1edge.mdl"))
#' polarities(read_mdl("tests/testthat/mdl/cld-adoption.mdl"))
#' polarities(read_mdl("tests/testthat/mdl/cld-shifting-the-burden.mdl"))
polarities <- function(mdl) {
  vertices <- vertices(mdl)
  edges <- edges(mdl)
  mdl <- mdl[as.logical(Reduce(sum, sapply(mdl, function(x){grepl("View 1", x)}), accumulate = TRUE))]
  mdl <- mdl[-c(1,2,length(mdl))]
  mdl <- mdl[!sapply(mdl, function(x){grepl(paste(vertices$label, collapse = "|"), x)})]
  mdl <- mdl[1:nrow(edges)]
  from <- sapply(strsplit(mdl, ","), function(x)x[[3]])
  to <- sapply(strsplit(mdl, ","), function(x)x[[4]])
  polarity <- sapply(strsplit(mdl, ","), function(x)x[[7]])
  polarity_lut <- data.frame(key = c(0,43,45), type = c("", "+", "-"), stringsAsFactors = FALSE)
  polarity <- as.character(sapply(polarity, function(x){polarity_lut$type[polarity_lut$key == x]}), row.names(NULL))
  df <- data.frame(from = from, to = to, polarity = polarity, stringsAsFactors = FALSE)
  return(merge(edges, df, sort = FALSE)$polarity)
}
