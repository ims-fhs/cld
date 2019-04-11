context("annotate.R")

test_that("annotate_ref_mode throws errors when invoked with unsuitable data.", {
  cld <- import("mdl/burnout.mdl")
  expect_error(annotate_ref_mode(ggplot2::ggplot(), cld))
  # cld <- cld %>% describe(type = "ref_mode", 0/0 %-% 1/1)
  # expect_success(annotate_ref_mode(ggplot2::ggplot(), cld))
})

test_that("annotate_text throws errors when invoked with unsuitable data.", {
  cld <- import("mdl/burnout.mdl")
  expect_error(annotate_text(ggplot2::ggplot(), cld))
  # cld <- cld %>% describe(type = "text", "some text that..")
  # expect_success(annotate_text(ggplot2::ggplot(), cld))
})

