#' Prints a date in any locale
#'
#' Convenience function to print a date using user format and locale.
#'
#' @param x A Date object; defaults to current date.
#' @param format A character string providing the format, as recognized by
#'     \code{\link[base]{format}} (see \code{\link[base]{as.Date}} for details);
#'     defaults to \code{"\%A \%d \%B \%Y"},
#'     i.e. \code{"vendredi 06 novembre 2026"} in French.
#' @param locale A character string providing the locale, as recognized by
#'     \code{\link[base]{Sys.setlocale}} (see \code{\link[base]{locales}} for
#'     details); defaults to \code{"fr_FR.utf8"}.
#' @returns A character string with the date in user format and locale.
#' @author Mathieu Basille <\email{mathieu@@basille.org}>
#' @export
date_locale <- function(x =  Sys.Date(), format = "%A %d %B %Y",
  locale = "fr_FR.utf8") {
    current_locale <- Sys.getlocale("LC_TIME")
    ## When function exits, restore original locale
    on.exit(Sys.setlocale("LC_TIME", current_locale))
    Sys.setlocale("LC_TIME", locale)
    format(x, format = format)
}
