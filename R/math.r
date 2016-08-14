#------------------------------------------------------------------------------#
#--------------------------------- yg::math.r ---------------------------------#
#------------------------- author: gyang274@gmail.com -------------------------#
#------------------------------------------------------------------------------#

#--------+---------+---------+---------+---------+---------+---------+---------#
#234567890123456789012345678901234567890123456789012345678901234567890123456789#

#------------------------------------------------------------------------------#
#------------------------------------ math ------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#--------------------------------- geometrist ---------------------------------#
#------------------------------------------------------------------------------#

#' calculate_dist_point_point
#' @description
#' calculate dist from a point x (x1, x2) to another point y (y1, y2)
#'  dist is defined as euclidean dist sqrt((x1 - y1)^2 + (x2 - y2)^2)
#' @rdname calculate_angle_distance_projection
#' @export
calculate_dist_point_point <- function(x1, x2, y1, y2) {
  return( sqrt((x1 - y1)^2 + (x2 - y2)^2) )
}

#' calculate_dist_point_line
#' @description
#' calculate dist from a point x (x1, x2) to a line of y (y1, y2) - z (z1, z2)
#'  dist is defined as euclidean dist x to x_proj_yz, where x_proj_yz is point
#'  x's projected point on line y - z.
#' @rdname calculate_angle_distance_projection
#' @export
calculate_dist_point_line <- function(x1, x2, y1, y2, z1, z2) {
  return( abs((y2 - z2) * x1 - (y1 - z1) * x2 + y1 * z2 - y2 * z1) / sqrt((y1 - z1)^2 + (y2 - z2)^2) )
}

#' calculate_proj_point_line
#' @description
#' calculate proj from a point x (x1, x2) to a line of y (y1, y2) - z (z1, z2)
#'  projection point x_proj_yz is defined as point p on line extended by y - z
#'  which minimize euclidean dist(x, p), p argmin(dist(x, p), p on line y - z)
#' @rdname calculate_angle_distance_projection
#' @export
calculate_proj_point_line <- function(x1, x2, y1, y2, z1, z2) {
  d1 = z1 - y1
  d2 = z2 - y2
  s1 = x1 - y1
  s2 = x2 - y2
  return( (s1 * d1 + s2 * d2) / (d1^2 + d2^2) * cbind(d1, d2) + cbind(y1, y2) )
}

#' calculate_angle_point_segment
#' @description
#' calculate angle of a point x (x1, x2) to a segement y (y1, y2) - z (z1, z2)
#'  angle is defined as angleYXZ = cosine(vXY, vXZ) cosine of angle spanned at
#'  vertex X in triangle YXZ, and vXY is X -> Y: vector from X to Y.
#' an often use case:
#'  determine whether x_proj_yzLine within ygSegment or not
#'  x_proj_yzLine within ygSegment if both angle x-y-z > 0 and x-z-y > 0
#'  x_proj_yzLine within ygSegment and on exact point y if x-y-z = 0
#'  x_proj_yzLine within ygSegment and on exact point z if x-z-y = 0
#'  x_proj_ygLine outside ygSegment and close to y if x-y-z < 0 and x-z-y > 0
#'  x_proj_ygLine outside ygSegment and close to z if x-y-z > 0 and x-z-y < 0
#'  it is impossible to have both x-y-z < 0 and x-z-y < 0 a on euclidean space
#' @rdname calculate_angle_distance_projection
#' @export
calculate_angle_point_segment <- function(x1, x2, y1, y2, z1, z2) {
  s1 = y1 - x1
  s2 = y2 - x2
  t1 = z1 - x1
  t2 = z2 - x2
  return( (s1 * t1 + s2 * t2) / (sqrt(s1^2 + s2^2) * sqrt(t1^2 + t2^2)) )
}

#' check_proj_within_segment
#' @description
#' check x_proj_yz - a point x (x1, x2)'s proj on line y (y1, y2) - z (z1, z2) -
#'  x_proj_yz is within segment y - z? on point y or z? outside segment y - z?
#' this is an application of calculate_angle_point_segment
#' @return
#'  TRUE: within segment y - z, on point y or z
#'  FALSE: outside segment y - z
#' @rdname calculate_angle_distance_projection
#' @export
check_proj_within_segment <- function(x1, x2, y1, y2, z1, z2) {

  angleXYZ <- calculate_angle_point_segment(y1, y2, x1, x2, z1, z2)

  angleXZY <- calculate_angle_point_segment(z1, z2, x1, x2, y1, y2)

  # note: angle is 1 -> 0 -> -1 as triangluar angle 0 -> 90 -> 180.

  return( (angleXYZ >= 0) & (angleXZY >= 0) )
}

#' calculate_dist_point_segment
#' @description
#' calculate dist from a point x(x1, x2) to a line segment y(y1, y2) - z(z1, z2)
#'  dist is defined as dist(x, x_proj_yz), when x_proj_yz on line segment y - z,
#'  and otherwise min(dist(x, y), dist(x, z)), where point x_proj_yz is defined
#'  as point x's projected point on line segment x - y.
#' dist defined above is equivalent to min(dist(x, p), p on yzSegment).
#' calculation is vectoriezed so param z1, z2, x2, x2, y1, y2 are expected to be
#'  vectors of same length.
#' @rdname calculate_angle_distance_projection
#' @export
calculate_dist_point_segment <- function(x1, x2, y1, y2, z1, z2) {

  # init
  x_dist_yzSgmt <- numeric(length(x1))

  # check whether or not x_proj_yzLine within ygSgmt
  # didn't use the wrapper check_proj_within_segment
  # so that we can know if outside which side y or z
  # save half of calculation in the outside cases ok
  angleXYZ <- calculate_angle_point_segment(y1, y2, x1, x2, z1, z2)

  angleXZY <- calculate_angle_point_segment(z1, z2, x1, x2, y1, y2)

  # x_proj_yzLine within ygSgmt
  idX <- which((angleXYZ >= 0) & (angleXZY >= 0))

  # x_proj_yzLine outside ygSgmt and closer to y
  idY <- which( angleXYZ < 0 )

  # x_proj_yzLine outside ygSgmt and closer to z
  idZ <- which( angleXZY < 0 )

  if ( length(idX) > 0L ) {

    x_dist_yzSgmt[idX] <- calculate_dist_point_line(
      x1[idX], x2[idX], y1[idX], y2[idX], z1[idX], z2[idX]
    )

  }

  if ( length(idY) > 0L ) {

    x_dist_yzSgmt[idY] <- calculate_dist_point_point(
      x1[idY], x2[idY], y1[idY], y2[idY]
    )

  }

  if ( length(idZ) > 0L ) {

    x_dist_yzSgmt[idZ] <- calculate_dist_point_point(
      x1[idZ], x2[idZ], z1[idZ], z2[idZ]
    )

  }

  return(x_dist_yzSgmt)

}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#--------------------------------- geologist ----------------------------------#
#------------------------------------------------------------------------------#

#' calculate_dist_wlatlng
#' @description
#'  calculate dist between coordinate1<lat1, lng1> and coordinate2<lat2, lng2>
#' @param .measure - mi or km
#' @export
calculate_dist_wlatlng <- function(.lat1, .lng1, .lat2, .lng2, .measure = "mi") {

  # earth radium in 3956 mi - 6367 km
  .r <- c(3956, 6367)[match(.measure, c("mi", "km"))]

  .dlat <- .lat2 - .lat1

  .dlng <- .lng2 - .lng1

  # most computers require the arguments of trignometric functions to be expressed in radians. To convert lon1,lat1 and lon2,lat2 from degrees, minutes, and seconds to radians, first convert them to decimal degrees. To convert decimal degrees to radians, multiply the number of degrees by pi/180 = 0.017453293 radians/degree. <http://www.movable-type.co.uk/scripts/gis-faq-5.1.html>

  .a <- sin(.dlat/2 * pi/180)^2 + cos(.lat1 * pi/180) * cos(.lat2 * pi/180) * sin(.dlng/2 * pi/180)^2

  .c <- 2 * asin(pmin(1, sqrt(.a)))

  .d <- .r * .c

  return(.d)

}

#------------------------------------------------------------------------------#
