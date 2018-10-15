vertices <- function(mdl) {
  mdl <- mdl[as.numeric(sapply(mdl, function(x){grep("A FUNCTION OF", x)})), ]
  labels <- trimws(sapply(strsplit(mdl, "= A FUNCTION OF"), function(x){x[1]}))
  data.frame(id = 1:length(labels), label = labels, stringsAsFactors = FALSE)
}

edges <- function(mdl) {
  mdl <- mdl[as.numeric(sapply(mdl, function(x){grep("A FUNCTION OF", x)})), ]
  vertices <- vertices(mdl)
  to <- trimws(sapply(strsplit(mdl, "= A FUNCTION OF"), function(x){x[1]}))
  to <- sapply(to, function(x){vertices$id[vertices$label == x]})

  from <- trimws(gsub("[()]", " ", sapply(strsplit(mdl, "= A FUNCTION OF"), function(x){x[2]})))
  from <- from[from != ""]
  from <- sapply(from, function(x){vertices$id[vertices$label == x]})

  edges <- data.frame(from = from, to = to, stringsAsFactors = FALSE)
  edges <- edges[from != to, ]
}

read_mdl <- function(file) {
  read.csv(file, sep = ";", stringsAsFactors = FALSE)
}
