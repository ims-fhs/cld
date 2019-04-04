context("link > group")

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
})

test_that("links works", {
  cld <- import("mdl/burnout.mdl")
  expect_equal(links(cld, "hours %->% energy"), 6)
  expect_equal(links(cld, "perceived %->% energy"), 4)
  expect_equal(links(cld, "energy %->% accomplishments per week"), 5)
})

context("link")

test_that("link works", {
  cld <- import("mdl/burnout.mdl")
  expect_equal((cld %>% link(hours %->% energy))$group, c(1, 1, 1, 1, 1, 2, 1, 2, 1, 2))
  expect_equal((cld %>% link(hours %->% energy) %>% link(energy))$group, c(1, 1, 1, 1, 1, 2, 1, 2, 1, 3))
  expect_equal((cld %>% link(hours %->% energy, perceived %->% energy) %>% link(energy))$group, c(1, 1, 1, 2, 1, 2, 1, 2, 2, 3))
})
