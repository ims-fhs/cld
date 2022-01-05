#' Internal function.
#' Determines the 'position' of a point c in accordance to the line from point a
#' to point b. If c is collinear, position returns 0; if c is on the right side,
#' position is < 0; if c is on the left side, position is > 0
#' The method is the one to determine, whether three points are collinear.
#' See for example:
#' https://stackoverflow.com/questions/3813681/checking-to-see-if-3-points-are-on-the-same-line
#' https://math.stackexchange.com/questions/701862/how-to-find-if-the-points-fall-in-a-straight-line-or-not
#'
#' @param x_a x component of first point
#' @param y_a y component of first point
#' @param x_b x component of second point
#' @param y_b y component of second point
#' @param x_c x component of the point whose 'position' needs to be determined
#' @param y_c y component of the point whose 'position' needs to be determined
#'
#' @return The position of the third point
#'
#' @examples
#' cld:::position(0,0,1,1,.5,.5)
#' # 0
#' cld:::position(0,0,1,1,1,0)
#' # -1
#' cld:::position(0,0,1,1,0,1)
#' # 1
position <- function(x_a, y_a, x_b, y_b, x_c, y_c) {
  ((x_b - x_a) * (y_c - y_a) - (y_b - y_a) * (x_c - x_a))
}

#' Internal function.
#' Determines the curvature argument of a link.
#'
#' @param cld A cld
#'
#' @return An updated cld
#'
#' @examples
#' cld <- curvature(link_coordinates(import("tests/testthat/mdl/burnout.mdl")))
curvature <- function(cld) {
  assertthat::assert_that(all(c("from_x", "from_y", "to_x", "to_y") %in% colnames(cld)))
  cld$curvature <- NA
  cld$curvature <- position(cld$from_x, cld$from_y, cld$to_x, cld$to_y, cld$x, cld$y)
  sig <- sign(cld$curvature)
  # cld$curvature <- -(cld$curvature^.1 - 2)
  cld$curvature <- -sig*(log(abs(cld$curvature)^.1) - .5)
  assertthat::assert_that(sum(is.na(cld$curvature)) == sum(is.na(cld$from)))
  return(cld)
}


offset_coord <- function(factor) {
  function(from, to) return((to - from)/factor)
}

offset_x <- offset_coord(10)
offset_y <- offset_coord(100)



angles <- function(df) {
  df$theta <- atan2((df$from_y - df$to_y), (df$from_x - df$to_x))
  df$theta_end <- df$theta + df$curvature * (pi/2)
  df$theta <- atan2((df$to_y - df$from_y), (df$to_x - df$from_x))
  df$theta_start <- df$theta - df$curvature * (pi/2)
  return(df)
}



#' Internal function. Recalculates the startpoints of a link
#'
#' @param df A data.frame with columns from_x, from_y, theta_start
#' @param r The radius around the center of the variable
#'
#' @return An updated df
#'
#' @examples
#' cld:::starts(data.frame(from_x = 0, from_y = 0, theta_start = 0), r = 10)
#' cld:::starts(data.frame(from_x = 0, from_y = 0, theta_start = pi/2), r = 10)
#' cld:::starts(data.frame(from_x = 0, from_y = 0, theta_start = pi), r = 10)
starts <- function(df, r) {
  df$from_x <- cos(df$theta_start) * r + df$from_x
  df$from_y <- sin(df$theta_start) * r + df$from_y
  return(df)
}



#' Internal function. Recalculates the endpoints of a link
#'
#' @param df A data.frame with columns to_x, to_y, theta_end
#' @param r The radius around the center of the variable
#'
#' @return An updated df
#'
#' @examples
#' cld:::ends(data.frame(to_x = 0, to_y = 0, theta_end = 0), r = 10)
#' cld:::ends(data.frame(to_x = 0, to_y = 0, theta_end = pi/2), r = 10)
#' cld:::ends(data.frame(to_x = 0, to_y = 0, theta_end = pi), r = 10)
ends <- function(df, r) {
  df$to_x <- cos(df$theta_end) * r + df$to_x
  df$to_y <- sin(df$theta_end) * r + df$to_y
  return(df)
}


offset <- function(cld) {
  assertthat::assert_that(all(c("from_x", "from_y", "to_x", "to_y") %in% colnames(cld)))
  # cld$from_x <- cld$from_x + offset_x(cld$from_x, cld$to_x)
  # cld$to_x <- cld$to_x - offset_x(cld$from_x, cld$to_x)
  # cld$from_y <- cld$from_y + offset_y(cld$from_y, cld$to_y)
  # cld$to_y <- cld$to_y - offset_y(cld$from_y, cld$to_y)
  cld <- angles(cld)
  cld <- starts(cld, 30)
  cld <- ends(cld, 60)
  cld$curvature <- cld$curvature * .8

  return(cld)
}







#' Calculates coordinates of links from the coordinates of the variables they connect.
#'
#' @param cld A cld
#'
#' @return An updated cld with columns from_x, from_y, to_x, to_y
#'
#' @examples
link_coordinates <- function(cld) {
  cld$from_x <- NA; cld$from_y <- NA; cld$to_x <- NA; cld$to_y <- NA
  for(i in 1:nrow(cld)) {
    if(cld$type[i] == "link") {
      cld$from_x[i] <- cld$x[cld$id == cld$from[i]]
      cld$from_y[i] <- cld$y[cld$id == cld$from[i]]
      cld$to_x[i] <- cld$x[cld$id == cld$to[i]]
      cld$to_y[i] <- cld$y[cld$id == cld$to[i]]
    }
  }
  return(cld)
}


# 190702 - Adrian Stämpfli - commented out; not used at the moment.
# # include type as a required aesthetic mapping in GeomLink
# GeomLink <- ggplot2::ggproto("GeomLink", ggplot2::GeomCurve,
#                             required_aes = c("id", "x", "y", "from", "to", "polarity", "type", "group"),
#                             default_aes = ggplot2::aes(id = id, x = x, y = y, from = from, to = to, polarity = polarity, type = type, group = group, colour = "black",
#                                                        angle = 0, hjust = 0.5, vjust = 0.5,
#                                                        alpha = NA, fontface = 2,
#                                                        lineheight = 1.2, length = 10),
#                             setup_data = function(data, params){
#                               data <- link_coordinates(data)
#                               data <- data[data$type == "link", ]
#                               print(data)
#                             })
#
#
# geom_link <- function(mapping = ggplot2::aes(x = from_x, y = from_y, xend = to_x, yend = to_y, id = id, from = from, to = to, polarity = polarity, type = type), data = NULL, position = "identity", stat = "identity",
#                      ..., curvature = 0.5, angle = 90, ncp = 5, arrow = NULL,
#                      arrow.fill = NULL, lineend = "butt", na.rm = FALSE, show.legend = NA,
#                      inherit.aes = TRUE)
# {
#   ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomLink,
#                  position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#                  params = list(arrow = arrow, arrow.fill = arrow.fill,
#                                curvature = curvature, angle = angle, ncp = ncp,
#                                lineend = lineend, na.rm = na.rm, ...))
# }
