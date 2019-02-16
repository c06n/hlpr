#' Negative match
#'
#' While \code{match} returns the position of \code{y} where an element of \code{x} was found, \code{nmatch} and \code{\%nin\%} returns the position of \code{x} which were not found in \code{y}. It is not possible to give the position of \code{y} where something was \emph{not} found.
#'
#' For \code{\%nix\%}, the index of the non-intersecting values for \code{x} is returned.
#'
#' @param x A vector
#' @param table The data set to be queried.
#'
#' @return logical vector or index vector
#'
#' @examples
#'
#' x <- c(1, 2, 3)
#' y <- c(3, 4, 5)
#'
#' nmatch(x, y)
#' x %nix% y
#'
#' @name nmatch
NULL

#' @rdname nmatch
#' @export
nmatch <- function(x, table) match(x, table, nomatch = 0) == 0

#' @rdname nmatch
#' @export
`%nin%` <- function(x, table) match(x, table, nomatch = 0) == 0

#' @rdname nmatch
#' @export
`%nix%` <- function(x, table) which(match(x, table, nomatch = 0) == 0)



