#' Create object of class ical
#'
#' @inheritParams ic_find
#' @param ic_attributes Calendar attributes, e.g. as provided by `ic_attributes_vec()`.
#' @export
#' @examples
#' # ical from .ics characters:
#' class(ical_example)
#' ic <- ical(ical_example)
#' attributes(ic)
#' class(ic)
#' # ical from data frame:
#' ic_df <- data.frame(ic)
#' ic2 <- ical(ic_df)
#' class(ic2)
#' attributes(ic2)
ical <- function(x, ic_attributes = NULL) {
  is_df <- is.data.frame(x)
  if(methods::is(x, "character")) {
    ical_df <- ic_dataframe(x)
    ical_tibble <- tibble::as_tibble(ical_df)
    if(is.null(ic_attributes)) {
      attr(ical_tibble, "ical") <- ic_attributes_vec(x)
    } else {
      attr(ical_tibble, "ical") <- ic_attributes
    }

  } else {
    if(!is_df) stop("x must be a data frame or charcter strings")
    n <- names(x)
    is_core = calendar::properties_core %in% n
    if(!all(is_core)) {
      stop(paste0(
        "x must contain column names: ",
        paste0(calendar::properties_core, collapse = ", ")
      ))
    }
    ical_tibble <- tibble::as_tibble(x)
    if(is.null(ic_attributes)) {
      attr(ical_tibble, "ical") <- ic_attributes_vec()
    } else {
      attr(ical_tibble, "ical") <- ic_attributes
    }
  }
  class(ical_tibble) <- c("ical", class(ical_tibble))
  ical_tibble
}
#' Extract attributes from ical text
#' @inheritParams ical
#' @export
#' @examples
#' ic_attributes_vec() # default attributes (can be changed)
#' ic_attributes_vec(ical_example)
ic_attributes_vec <- function(
  x = NULL,
  ic_attributes = c(
    BEGIN = "VCALENDAR",
    PRODID = "ATFutures/calendar",
    VERSION = "2.0",
    CALSCALE = "GREGORIAN",
    METHOD = "PUBLISH"
    )
  ) {
  if(is.null(x)) {
    return(ic_attributes)
  }
  line_first_event <- grep("BEGIN:VEVENT", x)[1]
  x_attributes <- x[1:(line_first_event - 1)]
  ic_vector(x_attributes, pattern = "*")
}
#' Convert ical object to character strings of attributes
#'
#' @param ic object of class `ical`
#' @param zulu is the datetime in Zulu time?
#' `FALSE` by default, which means the calendar's current timezone
#' is used.
#' @export
#' @examples
#' ic <- ical(ical_example)
#' ic_character(ic)
#' identical(ical_example, ic_character(ic))
ic_character <- function(ic, zulu = FALSE) {
  char_attributes <- paste(names(attributes(ic)$ical), attributes(ic)$ical, sep = ":")
  char_events <- ic_char_event(ic, zulu)
  c(char_attributes, char_events, "END:VCALENDAR")
}
#' Convert ical object to character strings of events
#'
#' @inheritParams ic_character
#' @export
#' @examples
#' ic <- ical(ical_example)
#' ic_char_event(ic)
#' ic_char_event(ic[c(1, 1), ])
ic_char_event <- function(ic, zulu = FALSE) {
  date_cols <- grepl(pattern = "VALUE=DATE", x = names(ic))
  if(any(date_cols)) {
    ic[date_cols] <- lapply(ic[, date_cols], ic_char_date)
  }
  datetime_cols <- names(ic) %in% c("DTSTART", "DTEND")
  if(any(datetime_cols)) {
    ic[datetime_cols] <- lapply(ic[, datetime_cols], ic_char_datetime, zulu)
  }
  char_names <- c(rep(c("BEGIN", names(ic), "END"), nrow(ic)))
  char_contents <-  apply(ic, 1, function(x) c("VEVENT", as.character(x), "VEVENT"))
  paste(char_names, char_contents, sep = ":")
}
