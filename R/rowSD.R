#' \code{standard deviation} for for rows
#'
#' Gives the \code{standard deviation} for the rows of a \code{matrix} or lists that can be coerced to a \code{matrix}.
#'
#' @param x A \code{matrix} or coercible \code{list}.
#' @param na.rm Passed to \code{sd}
#'
#' @return A vector
#'
#' @importFrom stats sd
#'
#' @export rowSD
#'
#' @examples
#'
rowSD <- function(x, na.rm = FALSE) {
  # inspired by rowMean etc.

  if (is.data.frame(x))
    x <- as.matrix(x)
  if (!is.array(x) || length(dn <- dim(x)) < 2L)
    stop("'x' must be an array of at least two dimensions")

  apply(x, 1, function(x) sd(x, na.rm = na.rm))

}