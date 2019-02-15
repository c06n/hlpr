#' Mode
#'
#' Get the modal value of a vector.
#'
#' @param x A \code{vector}.
#'
#' @return a scalar value
#'
#' @export Mode
#'
#' @examples
#'
#' x <- c(1, 2, 3, 3)
#' Mode(x)
#'
Mode <- function(x) {

  stopifnot(is.vector(x))

  rles <- rle(x)
  out <- rles$values[which(max(rles$lengths) == rles$lengths)]

  out

}
