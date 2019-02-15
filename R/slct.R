#' Find column indices or names with \code{regex}
#'
#' Find column indices or names by matching them against a regular expression.
#'
#' @param pattern \code{regex}-pattern to match the column names.
#' @param names_df Names of the data.frame to be matched.
#' @param names Should the actual names be returned?
#'
#' @return Either the index of the matched column names, or their index.
#'
#' @export slct
#'
#' @examples
#'
#' x <- data.frame(a = 1, b = 2, ab = 3)
#' slct("a", names(x))
#'
slct <- function(pattern, names_df, names = T) {

  idx <- unlist(lapply(pattern, grep, names_df))

  if (names) {names_df[idx]} else {idx}

}
