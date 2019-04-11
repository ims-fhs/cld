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
  expect_equal(nrow(edges(read_mdl("mdl/flexible-arbeitszeiten-part1.mdl"))), 3)
  # expect_equal(edges(read_mdl("mdl/flexible-arbeitszeiten-part1.mdl")), structure(list(id = 4:6, from = c("1", "2", "3"), to = c(2L,
                                                                                                                                 # 3L, 1L), polarity = c("+", "+", "-")), .Names = c("id", "from"                                                                                                                                                                             # "to", "polarity"), row.names = c(NA, 3L), class = "data.frame"))
})

test_that("extracting loops from cld works", {
  loops <- loops(read_mdl("mdl/burnout.mdl"))
  expect_equal(nrow(loops), 3)
  expect_equal(ncol(loops), 6)
  expect_equal(length(loops$polarity[loops$polarity == "R"]), 2)
  expect_equal(length(loops$direction[loops$direction == "counter"]), 2)
})

test_that("import works", {
  cld <- import("mdl/burnout.mdl")
  expect_equal(nrow(cld), 10)
  expect_equal(ncol(cld), 9)
  expect_equal(names(cld), c("type", "id", "x", "y", "label", "from", "to", "polarity",
                                                   "division"))
  expect_is(cld, "cld")
})
