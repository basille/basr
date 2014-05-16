## values
##
##' Replaces given values of a vector by new values.
##'
##' @title Change the values of a vector.
##' @param x A character or numeric vector.
##' @param from A vector describing the values to change from.
##' @param to A vector describing the values to change to.
##' @return A vector.
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' (bla <- rep(1:5, 3))
##' values(bla, c(3, 4), c(7, 3))
##' values(bla, c(3, 4), c("a", "b"))
##' (bli <- rep(letters[1:5], 3))
##' values(bli, c("b", "d"), c(1, 2))
##' blu <- rpois(1e6, 10)
##' system.time(values(blu, c(3, 4), c(7, 3)))
values <- function(x, from, to)
{
    if (!is.vector(x))
        stop("x must be a vector (numeric or character)")
    if (length(from) != length(to))
        stop("from and to must have the same length")
    for (i in 1:length(from)) x[x == from[i]] <- to[i]
    return(x)
}
