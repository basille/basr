#' Extract elements that appear multiple times
#'
#' Similarly to \code{unique}, \code{multiple} returns a vector with only the
#' values that appear several times, i.e. that are actually duplicated.
#'
#' @param x A vector.
#' @param single Logical, whether to actually return only values that appear a
#'     single time, i.e. the opposite of \code{multiple} (defaults to
#'     \code{FALSE}).
#' @param na.rm Logical, whether to remove \code{NA}s. Defaults to \code{FALSE},
#'     i.e. \code{NA} is considered a value of its own.
#' @return An object of the same type of \code{x}, but with only the values that
#'     appear several times.
#' @author Mathieu Basille <mathieu@@basille.org>
#' @export
#' @examples
#' (toto <- rep(c(1:4, NA), times = 1:5))
#' unique(toto)
#' multiple(toto)
#' unique(nselect(toto, 2)) # `nselect(x, 2)` keeps only values that occur at least twice
#'
#' ## Argument 'single'
#' multiple(toto, single = TRUE)
#' unique(nselect(toto, 2, comp = "<"))
#'
#' ## Argument 'na.rm'
#' unique(toto, na.rm = TRUE)
multiple <- function(x, single = FALSE, na.rm = FALSE) {
    ## Check that x is a vector
    if (!is.vector(x)) {
        stop("x should be a vector.")
    }
    ## 'na.rm = TRUE' removes all NAs
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    ## If 'unique', returns unique x that appears only once
    if (single) {
        return(x[!(x %in% x[duplicated(x)])])
    }
    ## Else return unique x that appears at least twice
    unique(x[duplicated(x)])
}
