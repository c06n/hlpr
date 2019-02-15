#' Converts a numerical value to the format \code{HH:MM::SS.MS}
#'
#' Converts seconds to a \emph{HH:MM:SS.MS} format. It handles
#' milliseconds well, is much short than the alternatives, and returns a string
#' instead of a date class.
#'
#' @param time A numerical value.
#' @param digits Number of decimal digits of the string.
#' @return A string corresponding to x.
#' @export sec2str
#' @rdname sec2str
#'
#' @examples
#'
#' sec2str(3612.5)
#'
#' @importFrom stringr str_match
#' @importFrom stringr str_pad
sec2str <- function(time, digits = 2) {

  if (!is.numeric(time)) stop("Numerical value expected")

  hh <- time %/% 3600
  mm <- (time %% 3600) %/% 60 # avoids the '60'-problem
  # DON'T! TOUCH! A! FUCKING! THING!
  tmp1 <- round((time %% 3600) %% 60, digits)
  if (tmp1 %% 1 != 0) {
    tmp2 <- stringr::str_match(tmp1, "(.*)\\.(.*)")
    ss <- tmp2[2]
    ms <- tmp2[3]
  } else {
    ss <- tmp1
    ms <- 0
  }

  out <- paste(
    stringr::str_pad(hh, 2, "left", 0),
    stringr::str_pad(mm, 2, "left", 0),
    stringr::str_pad(ss, 2, "left", 0),
    sep = ":"
  )
  out <- paste0(out, ".", stringr::str_pad(ms, digits, "right", 0))

  out

}
