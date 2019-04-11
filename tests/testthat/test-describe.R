context("describe")

test_that("add_textual_description adds textual description to a group of elements", {
  cld <- import("mdl/burnout.mdl")
  cld1 <- cld %>% link(`hours` %->%`energy`) %>% describe(type = "text", description = "The more hours you work, the less energy you have.")
  expect_is(cld, "cld")
  expect_is(cld1, "cld")
  expect_equal(nrow(cld1) - nrow(cld), 1)
  expect_equal(cld1$type[nrow(cld1)], "description_text")
  expect_equal(cld1$description[nrow(cld1)], "The more hours you work, the less energy you have.")
  expect_equal(cld1$group[nrow(cld1)], 2)
  expect_error(cld %>% describe(type = "unknown_type", description = ""))
})

test_that("add_ref_mode adds a ref mode in ggplot format to a group of elements", {
  cld <- import("mdl/burnout.mdl")
  cld1 <- cld %>% link(`hours` %->%`energy`) %>% describe(type = "ref_mode", 0/0 %-% 10/10)
  expect_is(cld, "cld")
  expect_is(cld1, "cld")
  expect_equal(nrow(cld1) - nrow(cld), 1)
  expect_equal(cld1$type[nrow(cld1)], "description_ref_mode")
  expect_equal(cld1$description[nrow(cld1)], "ggplot() + geom_segment(aes(x = 0, y =0, xend = 10, yend = 10)) + labs(title = \"\", x = \"\", y = \"\")")
  expect_equal(cld1$group[nrow(cld1)], 2)
  cld2 <- cld %>% link(`hours` %->%`energy`) %>% describe(type = "ref_mode", 0/0 %-% 10/10) %>% describe(type = "text", description = "The more hours you work, the less energy you have.")
  expect_equal(nrow(cld2) - nrow(cld), 2)
  expect_equal(ncol(cld2) - ncol(cld), 1)
})

