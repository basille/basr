#' Reclassify the values of a vector
#'
#' Reclassify given values of a vector by new values. Note that all values need
#' not to be documented, only the ones that need to be modified.
#'
#' @param x A character or numeric vector.
#' @param from A vector describing the values to change from, or a matrix of
#'     reclassification with two columns (from, to).
#' @param to A vector describing the values to change to, or nothing if
#'     \code{from} is a matrix.
#' @param na.fill Logical, whether to replace all values of \code{x} not
#'     specified in \code{from} by \code{NA}s (defaults to \code{FALSE}).
#' @param factor Logical, whether to return a factor (defaults to \code{FALSE}).
#' @param ... Additional arguments passed to \code{factor}.
#' @returns A vector with the same length as \code{x}.
#' @author Mathieu Basille <\email{mathieu@@basille.org}>
#' @export
#' @examples
#' (bla <- rep(1:5, 3))
#' reclass(bla, c(3, 4), c(7, 3))
#' reclass(bla, c(3, 4), c("a", "b"))
#' reclass(bla, c(3, 4), c("a", "b"), na.fill = TRUE)
#'
#' ## Conversion as a factor:
#' reclass(bla, c(3, 4), c("a", "b"), factor = TRUE)
#' (bli <- rep(letters[1:5], 3))
#' reclass(bli, c("b", "d"), c(1, 2))
#'
#' ## With a matrix of reclassification:
#' (mat <- matrix(c("b", "d", 1, 2), ncol = 2))
#' reclass(bli, mat)
#'
#' ## Fast computation time on large vectors:
#' blu <- rpois(1e6, 10)
#' system.time(reclass(blu, c(3, 4), c(7, 3)))
#'
#' ## Cases with NAs in x, from or to:
#' bla[c(2, 4)] <- NA
#' bla
#' reclass(bla, c(3, 4), c("a", NA))
#' reclass(bla, c(NA, 4), c("a", "b"))
#' reclass(bla, c(NA, 4), c("a", NA))
#'
reclass <- function(x, from, to = NULL, na.fill = FALSE, factor = FALSE, ...) {
    ## Check that x is a vector
    if (!is.vector(x))
        stop("`x` must be a vector (numeric or character).")
    ## If to is not provided, to is the second column of the matrix, while
    ## from is the first
    if (is.null(to)) {
        if (ncol(from) != 2)
            stop("`from` must have two columns.")
        to <- from[, 2]
        from <- from[, 1]
    }
    ## From must have only unique values
    if (any(duplicated(from)))
        stop("`from` can not have duplicated values")
    ## If to provided, check that from and to have same length
    else if (length(from) != length(to))
        stop("`from` and `to` must have the same length.")
    ## If 'from' includes NA
    if (any(is.na(from))) {
        x <- ifelse(is.na(x), to[which(is.na(from))], x)
        to <- to[!is.na(from)]
        from <- from[!is.na(from)]
    }
    ## New values
    new <- unlist(sapply(x, function(i) {
        ## Handles cases when x == NA
        if (is.na(i))
            NULL
        else to[from == i]
    }))
    ## Where to place them
    xfrom <- x %in% from
    ## Replace
    x <- replace(x, which(xfrom), new)
    if (na.fill)
        x <- replace(x, which(!(xfrom)), NA)
    ## If factor requested, return a factor
    if (factor)
        return(factor(x, ...))
    else return(x)
}
