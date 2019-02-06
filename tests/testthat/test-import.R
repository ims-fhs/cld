context("import.R")

test_that("read_mdl works", {
  expect_equal(nrow(read_mdl("mdl/cld-2nodes-1edge.mdl")), 3)
  expect_equal(nrow(read_mdl("mdl/cld-shifting-the-burden.mdl")), 10)
  expect_equal(nrow(read_mdl("mdl/cld-adoption.mdl")), 11)
})

test_that("extracting vertices from cld works", {
  expect_equal(nrow(vertices(read_mdl("mdl/cld-2nodes-1edge.mdl"))), 2)
  # expect_equal(vertices(read_mdl("mdl/cld-2nodes-1edge.mdl")), data.frame(id = 1:2, label = c("variable a", "variable b"), stringsAsFactors = FALSE))
  expect_equal(nrow(vertices(read_mdl("mdl/cld-shifting-the-burden.mdl"))), 4)
  expect_equal(nrow(vertices(read_mdl("mdl/cld-adoption.mdl"))), 3)
  expect_is(vertices(read_mdl("mdl/cld-2nodes-1edge.mdl")), "data.frame")
})

test_that("extracting edges from cld works", {
  expect_equal(nrow(edges(read_mdl("mdl/cld-adoption.mdl"))), 4)
  expect_equal(nrow(edges(read_mdl("mdl/cld-shifting-the-burden.mdl"))), 6)
  expect_equal(nrow(edges(read_mdl("mdl/cld-2nodes-1edge.mdl"))), 1)
})
