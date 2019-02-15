#' Round double so that the sum stays the same
#'
#' Rounding numbers of the type \code{double} will change the sum of these
#' numbers. E.g. for a stochastic vector where the sum must be \code{1} this is
#' not acceptable. After calling \code{round}, \code{smart.round} changes the
#' data so that it preserves the sum of the input, with the minimal difference
#' to the input data.
#'
#' @param x A \code{vector}, \code{matrix}, or numerical \code{data.frame}.
#' @param digits \code{integer}: Round to the \code{nth}-decimal place
#'
#' @details
#'
#' \itemize{
#'   \item{\href{https://stackoverflow.com/questions/792460/how-to-round-floats-to-integers-while-preserving-their-sum}{algorithm}}
#'   \item{\href{https://stackoverflow.com/questions/32544646/round-vector-of-numerics-to-integer-while-preserving-their-sum}{R-implementation}}
#'   }
#'
#' @importFrom utils tail
#'
#' @export smart_round
#'
#' @return same as input
#'
#' @examples
#'
#' set.seed(42)
#' x <- rnorm(10)
#' y <- smart_round(x, 3)
#' round(sum(x), 3) == sum(y)
#'
smart_round <- function(x, digits = 0) {

  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x - y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up

}
