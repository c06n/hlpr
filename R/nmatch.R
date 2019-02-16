#' Negative match
#'
#' While \code{match} returns the position of \code{x} where it was found in \code{y}, \code{nmatch} returns the position of \code{x} that was not found in \code{y}. For \code{%nix%}, the index of the non-intersecting values for \code{x} is returned.
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
#' @name
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



