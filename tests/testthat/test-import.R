context("import.R")

test_that("read_mdl works", {
  expect_equal(nrow(read_mdl("mdl/cld-2nodes-1edge.mdl")), 55)
  expect_equal(nrow(read_mdl("mdl/cld-shifting-the-burden.mdl")), 66)
  expect_equal(nrow(read_mdl("mdl/cld-adoption.mdl")), 69)
})

test_that("extracting vertices from cld works", {
  expect_equal(nrow(vertices(read_mdl("mdl/cld-2nodes-1edge.mdl"))), 2)
  expect_equal(nrow(vertices(read_mdl("mdl/cld-shifting-the-burden.mdl"))), 4)
  expect_equal(nrow(vertices(read_mdl("mdl/cld-adoption.mdl"))), 3)
})

test_that("extracting edges from cld works", {
  expect_equal(nrow(edges("mdl/cld-shifting-the-burden.mdl")), 7)
})

