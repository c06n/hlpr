#' Zipper
#'
#' Inspired by Python's \code{zipper}. Taken two or more vectors, this function
#' arranges them like so: \code{(n11, n21), (n12, n22), (n13, n23)}. Functions
#' other than \code{c} can be used to work with the tupels.
#'
#' @param ... An arbitrary number of vectors
#' @param fun Function to be used to operate with the tupels.
#' @param vec If \code{TRUE}, a single vector is returned.
#'
#' @return Either a vector or a list of the zipped vector elements.
#' @export
#'
#' @examples
#' x <- 1:5
#' y <- 6:10
#' zip(x, y, vec = TRUE)
#' zip(x, y, fun = seq, vec = TRUE)
zipper <- function(..., fun = c, vec = FALSE) {

  if (vec == FALSE) {
    mapply(fun, ..., SIMPLIFY = FALSE)
  } else {
    unlist(mapply(fun, ..., SIMPLIFY = FALSE))
  }

}
