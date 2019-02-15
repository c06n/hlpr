#' Normalize a vector between upper and lower bound
#'
#' Given a vector, this function linearly transforms the vector's values between
#' an upper and a lower bound.
#'
#' @param x A vector
#' @param lower Lower bound: the smallest value in the vector will be this value.
#' @param upper Upper bound: the largest value in the vector will be this value.
#'
#' @details See the \href{http://stats.stackexchange.com/questions/178626/how-to-normalize-data-between-1-and-1}{algorithm} for details
#'
#' @return A vector
#'
#' @export norml
#'
#' @examples
#'
#' x <- rnorm(10)
#' norml(x)
#' norml(x, -8, 50)
#'
norml <- function(x, lower = 0, upper = 1) {

  if (!is.numeric(x)) stop("argument must be numeric")

  (upper - lower) *
    ((x - min(x, na.rm = TRUE)) /
       (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) + lower

}
