## reclass
##
##' Reclassify given values of a vector by new values. Note that all values
##' need not to be documented, only the ones that need to be modified.
##'
##' @title Reclassify the values of a vector.
##' @param x A character or numeric vector.
##' @param from A vector describing the values to change from.
##' @param to A vector describing the values to change to.
##' @param factor Logical, whether to return a factor (\code{default =
##' FALSE}).
##' @param ... Additional arguments passed to \code{factor}.
##' @return A vector.
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' (bla <- rep(1:5, 3))
##' reclass(bla, c(3, 4), c(7, 3))
##' reclass(bla, c(3, 4), c("a", "b"))
##' reclass(bla, c(3, 4), c("a", "b"), factor = TRUE)
##' (bli <- rep(letters[1:5], 3))
##' reclass(bli, c("b", "d"), c(1, 2))
##' blu <- rpois(1e6, 10)
##' system.time(reclass(blu, c(3, 4), c(7, 3)))
reclass <- function(x, from, to, factor = FALSE, ...)
{
    ## Check that x is a vector
    if (!is.vector(x))
        stop("x must be a vector (numeric or character)")
    ## Check that from and to have same length
    if (length(from) != length(to))
        stop("from and to must have the same length")
    ## Reclassify values
    for (i in 1:length(from)) x[x == from[i]] <- to[i]
    ## If factor requested, return a factor
    if (factor)
        return(factor(x, ...))
    else return(x)
}
