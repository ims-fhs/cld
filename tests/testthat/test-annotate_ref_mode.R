context("annotate_ref_mode.R")

test_that("annotate_ref_mode throws errors when invoked with unsuitable data.", {
  cld <- import("mdl/burnout.mdl")
  expect_error(annotate_ref_mode(ggplot2::ggplot(), cld))
  cld <- cld %>% describe(type = "ref_mode", 0/0 %-% 1/1)
  print(cld)
  expect_error(annotate_ref_mode(ggplot2::ggplot(), cld))
})
