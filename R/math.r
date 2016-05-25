#------------------------------------------------------------------------------#
#--------------------------------- yg::math.r ---------------------------------#
#------------------------- author: gyang274@gmail.com -------------------------#
#------------------------------------------------------------------------------#

#--------+---------+---------+---------+---------+---------+---------+---------#
#234567890123456789012345678901234567890123456789012345678901234567890123456789#

#------------------------------------------------------------------------------#
#------------------------------------ math ------------------------------------#
#------------------------------------------------------------------------------#
#' dist_wlatlot
#' dist btwn coordinate1<lat1, lot1> and coordinate2<lat2, lot2>
#' @param .measure - mi or km
dist_wlatlot <- function(.lat1, .lot1, .lat2, .lot2, .measure = "mi") {

  # earth radium in 3956 mi - 6367 km
  .r <- c(3956, 6367)[match(.measure, c("mi", "km"))]

  .dlat <- .lat2 - .lat1

  .dlot <- .lot2 - .lot1

  # Most computers require the arguments of trignometric functions to be expressed in radians. To convert lon1,lat1 and lon2,lat2 from degrees, minutes, and seconds to radians, first convert them to decimal degrees. To convert decimal degrees to radians, multiply the number of degrees by pi/180 = 0.017453293 radians/degree. <http://www.movable-type.co.uk/scripts/gis-faq-5.1.html>

  .a <- sin(.dlat/2 * pi/180)^2 + cos(.lat1 * pi/180) * cos(.lat2 * pi/180) * sin(.dlot/2 * pi/180)^2

  .c <- 2 * asin(pmin(1, sqrt(.a)))

  .d <- .r * .c

  return(.d)
}
#------------------------------------------------------------------------------#
