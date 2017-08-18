#' Writes data to a file
#'
#' The use case for this function is to quickly write out a data.frame into a
#' test file. It uses data.table's \code{fwrite}, which is faster than \code{write.table}
#' and handels large values (floats) much better.
#'
#' @param x Data of type \code{list}
#' @param fname Name for the file. Default is "test". If not provided, \code{.csv}
#'   will be appended.
#' @param sep CSV separator
#' @param dec Decimal separator
#'
#' @export
#' @examples
#' w("my_data")
#' w("my_data", "../analysis/my_data.csv")
w <- function(x, fname = "test", sep = ";", dec = ".") {

  if (!grepl(".csv", fname)) paste0(fname, ".csv")
  data.table::fwrite(x, fname, sep = ";", dec = ".")

}