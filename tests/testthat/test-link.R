context("link > division")

test_that("group works", {
  expect_equal(group(c(1,1,1,1), c(2,3)), c(1,2,2,1))
  expect_equal(group(c(1,2,2,1,1), c(1,4)), c(3,2,2,3,1))
  expect_error(group(c(1,2,2,1,1), c(6)))
  expect_error(group(c(1,2,2,1,1), c(0)))
  expect_equal(group(c(1,2), c(1,2)), c(1,1))
})

test_that("ungroup works", {
  expect_equal(ungroup(1:3), c(1,1,1))
})

context("link > elements")

test_that("vars works", {
  cld <- import("mdl/burnout.mdl")
  expect_equal(vars(cld, "hours"), 8)
  expect_equal(vars(cld, "hours %->% energy"), c(8,10))
  expect_equal(vars(cld, "perceived %->% energy"), c(9,10))
  expect_error(vars(cld, NA))
  expect_error(vars(cld, "a %-> e"))
})

test_that("vars matches variable names case insensitive", {
  cld <- import("mdl/test-case-insensitivity.mdl")
  expect_equal(vars(cld, "upper case"), 3)
  expect_equal(vars(cld, "Lower Case %->% upper case"), c(2,3))
})

test_that("vars matches partial variable names correctly", {
  cld <- import("mdl/test-partial-variable-names.mdl")
  expect_error(vars(cld, "some %->% some var"), NA)
  expect_equal(length(vars(cld, "some")), 1)
  expect_error(length(vars(cld, "var")))
})

test_that("links works", {
  cld <- import("mdl/burnout.mdl")
  expect_equal(links(cld, "hours %->% energy"), 6)
  expect_equal(links(cld, "perceived %->% energy"), 4)
  expect_equal(links(cld, "energy %->% accomplishments per week"), 5)
  expect_equal(links(cld, "hours %->% energy %->% accomplishments per week"), c(5, 6))
})

context("link")

test_that("link works", {
  cld <- import("mdl/burnout.mdl")
  expect_is(cld %>% link(hours %->% energy), "cld")
  expect_equal((cld %>% link(hours %->% energy))$division, c(1, 1, 1, 1, 1, 2, 1, 2, 1, 2))
  expect_equal((cld %>% link(hours %->% energy) %>% link(energy))$division, c(1, 1, 1, 1, 1, 2, 1, 2, 1, 3))
  expect_equal((cld %>% link(hours %->% energy, perceived %->% energy) %>% link(energy))$division, c(1, 1, 1, 2, 1, 2, 1, 2, 2, 3))
  expect_equal((cld %>% link(hours %->% energy %->% `accomplishments per week`))$division, c(1, 1, 1, 1, 2, 2, 2, 2, 1, 2))
  # expect_error(cld %>% link(`hours` %->% `accomplishments` %->% `perceived` %->% `hours`))
  expect_equal(sum((cld %>% link(`hours` %->%`accomplishments per week` %->% `perceived` %->% `hours`))$division), 16)
  # cld <- import("mdl/flexibilisierung.mdl")
  # expect_equal(link(cld, `Belastung` %->% `Flexibilisierung der Arbeit` %->% `Private Dinge während der Arbeit` %->% `Belastung`)$division, c(2,2,2,1,1,1,2,2,2,1,1))
  cld <- import("mdl/flexibilisierung-ae.mdl")
  expect_equal(link(cld, `Belastung` %->% `Flexibilisierung der Arbeit` %->% `Private Dinge waehrend der Arbeit` %->% `Belastung`)$division, c(2,2,2,1,1,1,2,2,2,1,1))
})
