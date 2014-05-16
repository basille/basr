## nselect
##
##' Select a subset of a table with at least \code{n} occurrences of a
##' category.
##'
##' @title Subsetting tables given occurrences
##' @param x A data frame or a matrix to be subsetted.
##' @param col The column on which the occurrences are counted; can be
##' the name or the number of the column.
##' @param n The minimum number of occurrences for which to keep the
##' data.
##' @param droplevels Logical. If yes, unused levels from factors in
##' the data frame are dropped.
##' @return A data frame.
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' set.seed(1)
##' bla <- data.frame(value = rnorm(100), group = sample(letters[1:4],
##'     size = 100, replace = TRUE, prob = (1:4) * 10))
##' table(bla$group)
##' bli <- nselect(bla, 2, 25, droplevels = TRUE)
##' table(bli$group)
nselect <- function(x, col, n, droplevels = FALSE) {
    if (!is.data.frame(x) & !is.matrix(x))
        stop("x should be a data frame or a matrix")
    if (droplevels)
        return(droplevels(data.frame(x[x[, col] %in% names(which(table(x[,
            col]) >= n)), ])))
    else return(data.frame(x[x[, col] %in% names(which(table(x[,
        col]) >= n)), ]))
}
