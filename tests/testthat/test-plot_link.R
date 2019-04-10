context("link_coordinates")

test_that("link_coordinates works.", {
  cld <- import("mdl/burnout.mdl")
  cld1 <- link_coordinates(cld)
  expect_equal(nrow(cld1), nrow(cld))
  expect_equal(ncol(cld1), ncol(cld) + 4)
  expect_equal(cld1$from_x[1:6], c(426, 610, 569, 569, 253, 426))
  expect_equal(cld1$from_y[1:2], c(-276, -226))
  expect_equal(cld1$to_x[1:2], c(610, 569))
  expect_equal(cld1$to_y[1:2], c(-226, -364))
})
