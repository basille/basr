## extrange
##
##' Returns the range extended by a given proportion.
##'
##' If the regular range returns a single value, the proportion is
##' computed on this value itself (and not on the range).
##' @title Extended range
##' @param x A numeric vector.
##' @param percent The proportion to be added to the range.
##' @param na.rm Logical, indicating if \code{NA}'s should be omitted.
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' extrange(0:10)
##' extrange(0:10, percent = .5)
##' extrange(-10:10)
##' extrange(rep(10, 3))
extrange <- function(x, percent = 0.1, na.rm = FALSE) {
    rr <- range(x, na.rm = na.rm)
    dr <- diff(rr)
    if (dr == 0)
        dr <- abs(rr[1])
    return(c(rr[1] - dr * percent, rr[2] + dr * percent))
}
