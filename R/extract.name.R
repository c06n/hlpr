#' Extraction of a name from path or file name
#'
#' Extract a name from a path or file name excluding file extension. Match anything up to but not including last dot or whole string without any dots.
#'
#' @param fname File name
#'
#' @return A string with the file name without extension.
#'
#' @export
#'
#' @examples
#'
#' extract_name("D:\\Home\\projects\\awar.d\\r-workspace\\functions.R")
extract.name <- function(fname) {

  stringr::str_extract(basename(fname), ".*(?=\\.)|[^\\.]*")

}
