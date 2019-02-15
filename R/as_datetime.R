#' Convert an object to a date or date-time with defaults
#'
#' Wrapper for \code{lubridate::as_datetime} with defaults for \code{unixtime} and \code{windowstime}.
#'
#' \emph{Unix Time} := seconds since 1970-01-01 at 00:00:00.0
#' \emph{Windows Time} := 100 ns since 1601-01-01 at 00:00:00.0
#'
#' Note:   difftime("1970-01-01", "1601-01-01", units="secs") is 692 s than what would be expected.
#' Reason: The value below is the diff in UTC (extended back to 1601), while
#'         difftime() considers the time jump of 6:32 min on 1893-04-01 in
#'         Berlin when switching from local time to CET.
#'
#' @param time A numerical value.
#' @param origin a Date object, or something which can be coerced by as.Date(origin, ...) to such an object (default: the Unix epoch of "1970-01-01" for \code{uxt}, and "1601-01-01" for \code{w2u}).
#' @param tz a time zone name (default: Europe/Berlin)
#' @return a vector of Date objects corresponding to x.
#'
#' @examples
#' # convert unixtime to a date object
#' uxt(1550242175.5)
#'
#' @importFrom lubridate as_datetime
#' @name as_datetime
NULL

#' @export
#' @rdname as_datetime
uxt <- function(time, origin = "1970-01-01", tz = "Europe/Berlin") {

  as_datetime(time, origin, tz)

}

#' @export
#' @rdname as_datetime
win2uxt <- function(time, origin = "1601-01-01", tz = "Europe/Berlin") {

  uxt(round(as.double(time) * 1e-7, 3), origin, tz)

}
