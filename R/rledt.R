#' Enhanced \code{rle} for \code{data.table}s
#'
#' \code{rle} for \code{data.table}s with additional information.
#'
#' @param x A \code{data.table}
#' @param na.collapse Should \code{NA}s be indexed each, or treated as a block.
#'
#' @return A \code{data.table}
#'
#' @import data.table
#'
#' @export rledt
#'
#' @examples
#'
rledt <- function(x, na.collapse = TRUE) {

  out <- data.table(
    value = x,
    id = rleidv(x))
  out[, start.row := .I]

  if (na.collapse) out <- out[!duplicated(id)]
  out[, length := c(diff(start.row), length(x) - last(start.row) + 1)]
  out[, end.row := start.row + length - 1]

  out

}