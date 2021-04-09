# https://stackoverflow.com/questions/38775661/what-is-the-difference-between-geoms-and-stats-in-ggplot2
# https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
# https://rud.is/books/creating-ggplot2-extensions/demystifying-ggplot2.html

# cp <- wesanderson::wes_palette("IsleofDogs1")[c(5,1,2,4)]
wesanderson::wes_palette("Chevalier1")[c(1,4)]
# cp <- c("#C7B19C", "#446455", "#000204", "#000204") # snf palette
# cp <- c("#FFFFFF", "#C7B19C", "#446455", "#000204") # snf palette
cp <- c("#D9D0D3", "#000000", "#000000", "#D9D0D3") # printing care visions paper
# cp <- c("#D9D0D3", "#9986A5", "#79402E", "#0F0D0E") # original palette
# cp <- c("#999093", "#7976A5", "#69301E", "#0F0D0E") # palette for printing of master thesis
# image(1:4, 1, as.matrix(1:4), col = cp, ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n")
names(cp) <- 1:length(cp)
font_var <- "Permanent Marker"
font_ref_mode <- "Open Sans"
font_description <- "Open Sans"

#' plot
#'
#' @param cld a data.frame containing CLD information
#'
#' @import ggplot2
#'
#' @return
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(magrittr)
#' library(cld)
#' cld <- import("tests/testthat/mdl/burnout.mdl")
#' # vars <- cld[cld$type == "var", ]
#' # ggplot(as.data.frame(vars), aes(x, y)) + geom_text(aes(label = label)) + theme_void()
#' # ggplot(as.data.frame(cld), aes(x, y)) + geom_text(aes(label = label)) + theme_void()
#' # ggplot(as.data.frame(cld), aes(x, y)) + geom_var() + theme_void()
#' # ggplot(as.data.frame(cld), aes(x, y)) + geom_var() + geom_link() + theme_void()
#' plot(cld)
#' cld %>% describe(type = "text", "blabla") %>% plot()
#' cld %>% link(`energy`) %>% describe(type = "text", "blabla") %>% plot()
#' cld %>% link(`energy` %->% `accomplishments per`) %>% describe(type = "text", "blabla") %>% plot()
#' cld %>% link(`energy` %->% `accomplishments per` %->% `perceived`) %>% link(`perceived`) %>% plot()
#' cld %>% describe(type = "ref_mode", 0/0 %-% 1/1) %>% plot()
#' cld %>% describe(type = "text", "blabla") %>% describe(type = "ref_mode", 0/0 %-% 1/1) %>% plot()
#' import("tests/testthat/mdl/cld-adoption.mdl") %>% plot()
plot.cld <- function(cld) {
  cld <- as.data.frame(cld)
  cld <- link_coordinates(cld)
  cld <- curvature(cld)
  cld <- offset(cld)
  links <- cld[cld$type == "link", ]
  gg <- ggplot(data = cld, aes(x, y, colour = as.factor(division))) +
    lapply(split(cld, 1:nrow(cld)), function(dat) {
      geom_curve(data = dat, aes(x = from_x, y = from_y, xend = to_x, yend = to_y), curvature = dat["curvature"], ,
                 arrow = arrow(length = unit(0.03, "npc")), show.legend = FALSE, family = font_var) }
    ) +
    geom_var() + scale_colour_manual(values = cp) + theme_void() + xlim(min(cld$x, na.rm = TRUE) - 150, max(cld$x, na.rm = TRUE) + 150) + ylim(min(cld$y, na.rm = TRUE) - 150, max(cld$y, na.rm = TRUE) + 150)

    # geom_curve(aes(x = from_x, y = from_y, xend = to_x, yend = to_y), curvature = -0.3) + theme_void()
  # ggplot(data = cld, aes(x, y)) + geom_var() +
  #   lapply(1:nrow(links), function(i) {
  #     print(i)
  #     geom_curve(data = links, aes(x = from_x, y = from_y, xend = to_x, yend = to_y), curvature = i) }
  # ) + theme_void()
  gg <- gg + annotate_polarity(gg, cld)
  if("description_ref_mode" %in% cld$type) {
    gg <- gg + annotate_ref_mode(gg, cld)
  }
  if("description_text" %in% cld$type) {
    gg <- gg + annotate_text(gg, cld)
  }
  gg
}
