#' Read ics file
#'
#' @param file ics file to read
#' @return object of class ics
#' @export
#' @examples
#' f <- system.file("extdata", "england-and-wales.ics", package = "calendar")
#' ics_df = ic_read(f)
#' head(ics_df)
ic_read <- function(file) {
  x <- readLines(file)
  y <- c()
  for (line in x) {
    # concatenate multilines to single line
    # [A-Z]-?_?[A-Z]: pattern
    if(!grepl(pattern = "[A-Z]-?[A-Z]:|[A-Z];", x = line)) {
      y[length(y)] <- paste0(y[length(y)], "\n", line)
    } else {
      y[length(y)+1] <- line
    }
  }
  # previous approach: see https://stackoverflow.com/questions/43573982/how-to-import-ical-ics-file-in-r
  ical(y)
}
#' Write ics file
#'
#' @inheritParams ic_character
#' @param file ics file to write
#' @export
#' @examples
#' ic <- ical(ical_example)
#' ic_write(ic, file.path(tempdir(), "ic.ics"))
#' f <- system.file("extdata", "example.ics", package = "calendar")
#' identical(readLines(file.path(tempdir(), "ic.ics")), readLines(f))
#' f <- system.file("extdata", "england-and-wales.ics", package = "calendar")
#' ics_df <- ic_read(f)
#' ic_write(ics_df, file.path(tempdir(), "ic.ics"))
#' # test similarity between files with diff tool like meld - from shell:
#' # meld ic.ics inst/extdata/england-and-wales.ics
ic_write <- function(ic, file, zulu = FALSE) {
  if (!inherits(ic, "ical")) {
    warning("Coercing ic to an ical object. Use ical() first to set custom attributes.")
    ic <- ical(ic)
  }
  ic_char <- ic_character(ic, zulu)
  writeLines(ic_char, file)
}
