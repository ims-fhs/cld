context("ref_mode core translation")

test_that("ref_mode_to_ggplot works", {
  expect_equal(ref_mode_to_ggplot("0/0 %-% 10/10"), "ggplot() + geom_segment(aes(x = 0, y =0, xend = 10, yend = 10)) + theme(panel.background = element_rect(fill = cp[1]))")
  expect_equal(ref_mode_to_ggplot("0/10 %(% 1/11 %-% 2/12"), "ggplot() + geom_curve(aes(x = 0, y =10, xend = 1, yend = 11), curvature = -.2) + geom_segment(aes(x = 1, y =11, xend = 2, yend = 12)) + theme(panel.background = element_rect(fill = cp[1]))")
  expect_equal(ref_mode_to_ggplot("0/10 %s% 1/11"), "ggplot() + geom_curve(aes(x = 0, y =10, xend = 0.5, yend = 10.5), curvature = .2) + geom_curve(aes(x = 0.5, y =10.5, xend = 1, yend = 11), curvature = -.2) + theme(panel.background = element_rect(fill = cp[1]))")
  expect_equal(ref_mode_to_ggplot("0/10 %(% 1/11 %-% 2/12 %)% 3/13"), "ggplot() + geom_curve(aes(x = 0, y =10, xend = 1, yend = 11), curvature = -.2) + geom_segment(aes(x = 1, y =11, xend = 2, yend = 12)) + geom_curve(aes(x = 2, y =12, xend = 3, yend = 13), curvature = .2) + theme(panel.background = element_rect(fill = cp[1]))")
  expect_equal(ref_mode_to_ggplot("2000/100 %-% 2010/100 %-% 2010/120 %)% 2050/110"), "ggplot() + geom_segment(aes(x = 2000, y =100, xend = 2010, yend = 100)) + geom_segment(aes(x = 2010, y =100, xend = 2010, yend = 120)) + geom_curve(aes(x = 2010, y =120, xend = 2050, yend = 110), curvature = .2) + theme(panel.background = element_rect(fill = cp[1]))")
})

test_that("ref_mode_to_ggplot throws errors when expected.", {
  expect_error(ref_mode_to_ggplot("\""))
})
