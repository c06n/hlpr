#' Writing a csv with defaults
#'
#' Write a data.frame to disk using \code{data.table::fwrite} with sensible
#' defaults. The default name of the file is the name of the object itself.
#'
#' @param x Any list of same length vectors, e.g. \code{data.frame} and \code{data.table}
#' @param fname Name for the file. Default is the name of the object to be
#' written.
#' @param overwrite Should existing files of the same name be overwritten?
#' @param sep CSV separator
#' @param dec decimal separator
#'
#' @importFrom data.table fwrite
#' @export
#'
#' @examples
#'
#' df <- list(a = 1, b = 2)
#' w(df)
#'
w <- function(x = NULL, fname = NULL, sep = ";", dec = ",", overwrite = FALSE) {

  if (is.null(x)) stop("Nothing to write.")

  # if no name is given, use the name of `x` as file name
  if (is.null(fname)) fname <- deparse(substitute(x))

  # add valid file ending
  file_ending <- str_match(fname, ".*\\.([A-Za-z]{1,3})")[, 2]
  if (!file_ending %in% c("csv", "log", "txt")) fname <- paste0(fname, ".csv")

  # change file name if it already exists in target directory
  if (!overwrite) {

    file_parts <- str_match(fname, "(.*)\\.([A-Za-z]{1,3})")[, 2:3]
    increment <- 1

    while (TRUE) {

      if (!fname %in% list.files(dirname(fname))) break

      fname <- paste0(file_parts[1],
                      "-",
                      increment,
                      ".",
                      file_parts[2])

      increment <- increment + 1

    }

  }

  fwrite(x, file = fname, sep = sep, dec = dec)

}
