#' Angle transformations
#'
#' Transform angles from \code{radian} to \code{degree}, and from \code{degree} to \code{radian}.
#'
#' @param x An angle
#'
#' @return The transformed angle
#'
#' @examples
#'
#' # Radian to degree
#' r2d(2 * pi)
#'
#' # Degree to radian
#' d2r(360)
#'
#' @name angles
NULL

#' @rdname angles
#' @export
r2d <- function(x) {

  x * (180 / pi)

}

#' @rdname angles
#' @export
d2r <- function(x) {

  x * (pi / 180)

}