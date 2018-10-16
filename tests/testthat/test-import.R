context("import.R")

test_that("read_mdl works", {
  expect_equal(length(read_mdl("mdl/cld-2nodes-1edge.mdl")), 36)
  expect_equal(length(read_mdl("mdl/cld-shifting-the-burden.mdl")), 49)
  expect_equal(length(read_mdl("mdl/cld-adoption.mdl")), 51)
})

test_that("filter_functional_definitions works", {
  expect_equal(length(filter_functional_definitions(read_mdl("mdl/cld-2nodes-1edge.mdl"))), 2)
  expect_equal(length(filter_functional_definitions(read_mdl("mdl/cld-shifting-the-burden.mdl"))), 4)
  expect_equal(length(filter_functional_definitions(read_mdl("mdl/cld-adoption.mdl"))), 3)
  expect_is(filter_functional_definitions(read_mdl("mdl/cld-2nodes-1edge.mdl")), "character")
})


test_that("extracting vertices from cld works", {
  expect_equal(nrow(vertices(read_mdl("mdl/cld-2nodes-1edge.mdl"))), 2)
  expect_equal(vertices(read_mdl("mdl/cld-2nodes-1edge.mdl")), data.frame(id = 1:2, label = c("variable a", "variable b"), stringsAsFactors = FALSE))
  expect_equal(nrow(vertices(read_mdl("mdl/cld-shifting-the-burden.mdl"))), 4)
  expect_equal(nrow(vertices(read_mdl("mdl/cld-adoption.mdl"))), 3)
  expect_is(vertices(read_mdl("mdl/cld-2nodes-1edge.mdl")), "data.frame")
})

test_that("extracting edges from cld works", {
  expect_equal(nrow(edges(read_mdl("mdl/cld-adoption.mdl"))), 4)
  expect_equal(nrow(edges(read_mdl("mdl/cld-shifting-the-burden.mdl"))), 6)
  expect_equal(nrow(edges(read_mdl("mdl/cld-2nodes-1edge.mdl"))), 1)
})

test_that("filter_functional_definitions works", {
  expect_equal(length(filter_functional_definitions(read_mdl("mdl/cld-2nodes-1edge.mdl"))), 2)
  expect_equal(length(filter_functional_definitions(read_mdl("mdl/cld-shifting-the-burden.mdl"))), 4)
  expect_equal(length(filter_functional_definitions(read_mdl("mdl/cld-adoption.mdl"))), 3)
  expect_is(filter_functional_definitions(read_mdl("mdl/cld-2nodes-1edge.mdl")), "character")
})

test_that("lhs works", {
  expect_equal(lhs("var1 = bla", sep = "="), "var1")
})

test_that("rhs works", {
  expect_equal(rhs("var1 = bla", sep = "="), "bla")
  expect_equal(rhs("var1 = (bla)", sep = "="), "bla")
})

test_that("positions works", {
  expect_equal(positions(read_mdl("mdl/cld-2nodes-1edge.mdl")), structure(list(x = c(349, 575), y = c(-187, -191)), .Names = c("x", "y"), row.names = c(NA, -2L), class = "data.frame"))
  print(positions(read_mdl("mdl/cld-adoption.mdl")))
  print(positions(read_mdl("mdl/cld-shifting-the-burden.mdl")))
})

test_that("polarities works", {
  expect_equal(polarities(read_mdl("mdl/cld-2nodes-1edge.mdl")), c("+"))
  expect_equal(polarities(read_mdl("mdl/cld-adoption.mdl")), c("+", "+", "+", "-"))
  expect_equal(polarities(read_mdl("mdl/cld-shifting-the-burden.mdl")), c("-", "+", "+", "+", "-", "-"))
})
