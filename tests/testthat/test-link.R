context("link.R: internal helper functions")

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
