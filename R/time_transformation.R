#' Convert common time formats.
#'
#' Convert some common time formats into other common time formats.
#' Most functions use \code{data.table}, since this package handles the
#' very large floats much better than \code{base}.
#'
#' \code{win2UnixTime}: Converts the Windows time format to Unix time.
#' \emph{Windows Time} := 100 ns since 1601-01-01 at 00:00:00.0
#' \emph{Unix Time} := seconds since 1970-01-01 at 00:00:00.0
#' Note: difftime("1970-01-01", "1601-01-01", units="secs") is 692 s shorter
#' than the value below
#' Reason: The value below is the diff in UTC (extended back to 1601), while
#'         difftime() considers the time jump of 6:32 min on 1893-04-01 in
#'         Berlin when switching from local time to CET.
#'
#' \code{win2UnixTime2}: Implementation that uses lubridate. Returns a data
#' class.
#'
#' \code{uxt2hrt}: Converts a Unix time into a human readable time. Returns a
#' data class.
#'
#' \code{sec2str}: Converts seconds to a \emph{HH:MM:SS.MS} format. It handles
#' milliseconds well, is much short than the alternatives, and returns a string
#' instead of a date class.
#'
#' @param time A numerical value representing a time
#'
#' @examples
#'
#' # Convert seconds into a HH:MM:SS.MS format
#' seconds_to_string(3612.5)
#'
#' @name time_transformation
NULL

#' @export
#' @rdname time_transformation
win2UnixTime <- function(time) {

  if (!is.numeric(time)) stop("input has not a numerical value")

	timeDiffWinUnix <- 11644473600 * 1e7  # in 100 ns

	# Unix time in seconds
	(time - timeDiffWinUnix) / 1e7

}

#' @export
#' @rdname time_transformation
win2UnixTime2 <- function(time) {
  # alternative implementation

  if (!is.numeric(time)) stop("input has not a numerical value")

  time <- round(data.table::as.data.table(time) * 1e-7, 3)
  time <- lubridate::as_datetime(time, origin = "1601-01-01", tz = "Europe/Berlin")
  time <- round(as.numeric(time) * 1e2) * 10

  time

}

#' @export
#' @rdname time_transformation
uxthrt <- function(time, tz = "Europe/Berlin") {

  if (!is.numeric(time)) stop("input has not a numerical value")

  time <- round(data.table::as.data.table(time) * 1e-7, 3)
  time <- lubridate::as_datetime(time * 1e-3, tz)

  time

}

#' @export
#' @rdname time_transformation
sec2str <- function(time, digits = 2) {

  if (!is.numeric(time)) stop("input has not a numerical value")

  time <- data.table::as.data.table(time)

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
