vertices <- function(mdl) {
  mdl <- filter_functional_definitions(mdl)
  labels <- lhs(mdl, "= A FUNCTION OF")
  data.frame(id = 1:length(labels), label = labels, stringsAsFactors = FALSE)
}

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
  edges <- edges[from != to, ]
}

read_mdl <- function(file) {
  read.csv(file, sep = ";", stringsAsFactors = FALSE)
}

filter_functional_definitions <- function(mdl) {
  mdl[as.numeric(sapply(mdl, function(x){grep("A FUNCTION OF", x)})), ]
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
  mdl <- mdl[sapply(mdl, function(x){grepl(paste(vertices$label, collapse = "|"), x)}), ]
  mdl <- mdl[!grepl("A FUNCTION OF", mdl)]
  names <- sapply(strsplit(mdl, ","), function(x)x[[3]])
  x <- as.numeric(sapply(strsplit(mdl, ","), function(x)x[[4]]))
  y <- -as.numeric(sapply(strsplit(mdl, ","), function(x)x[[5]]))
  df <- data.frame(label = names, x = x, y = y, stringsAsFactors = FALSE)
  return(merge(vertices, df)[c("x", "y")]
  )
}
