context("plot.R")

test_that("create_igraph", {
  path <- "mdl/flexible-arbeitszeiten.mdl"
  mdl <- read_mdl(path)
  vertices <- vertices(mdl)
  g <- create_igraph(vertices(mdl), edges(mdl))
  expect_equal(igraph::V(g)$x, vertices$x[c(1,2,3,5,4)])
  expect_equal(igraph::V(g)$y, vertices$y[c(1,2,3,5,4)])
})
