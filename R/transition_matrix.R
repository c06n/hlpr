#' Make a transition matrix.
#'
#' @param x a vector, a matrix, or a data.frame with numerical values
#' @param conv converts the input to a matrix of \code{dim(nrow=1, ncol=length(x))}
#' @param cast.out casts the output to a data.frame/data.table/matrix.
#' @param lag looks at the nth previous element in the markov chain vector
#' @param prob if set \code{TRUE}, returns relative frequencies, otherwise they are absolute
#'
#' @return a table object, or what in \code{cast.out} was defined
#' @export
#' @examples
#' x <- sample(toupper(letters[1:5]), 100, replace = T)
#' y <- trans_matrix(x, conv=T, cast.out="matrix", prob=T)
trans_matrix <- function(x, conv=FALSE, cast.out=FALSE, lag=1, prob=FALSE) {

  if (length(x) == 1) stop("Need more than one data point.")
  if (!is.numeric(x)) stop("argument must be numeric")
  if (conv) x <- matrix(x, nrow=1)
  if (is.null(ncol(x))) stop("A data type which has at least one column is expected. Maybe set conv=T?", call. = FALSE)

  tt <- table(x[,-c((ncol(x)-lag+1):ncol(x))] , c(x[,-c(1:lag)]))
  if(prob) tt <- tt / rowSums(tt)

  if (cast.out != FALSE) {
    tt <- unclass(tt)
    if (cast.out == "data.frame") {
      tt <- data.frame(V1 = row.names(tt), tt)
      row.names(tt) <- NULL
    } else if (cast.out == "data.table") {
      tt <- data.table(V1 = row.names(tt), tt)
      row.names(tt) <- NULL
    } else if (cast.out == "matrix") {
      tt <- as.matrix(tt)
    }
  }

  tt

}