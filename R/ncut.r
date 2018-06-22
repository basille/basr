## ncut
##
##' Cut a numeric vector into k classes of (roughly) equal size.
##'
##' @title Cut into classes of equal size
##' @param x numeric vector to cut.
##' @param k the number of classes.
##' @param labels Labels for the levels of the resulting category. By
##'     default (\code{FALSE}), labels are constructed using simple
##'     integer codes; if \code{NULL}, labels are build using
##'     \code{"(a,b]"} interval notation. Alternatively, a custom
##'     vector of length \code{k} can be used.
##' @return A vector of k classes.
##' @author Mathieu Basille \email{basille@ufl.edu} and Emiel van Loon
##' @export
##' @examples
##' bla <- rnorm(100)
##' summary(bla)
##' bli <- ncut(bla)
##' table(bli)
##' head(ncut(bla, labels = NULL))
##' head(ncut(bla, labels = LETTERS[1:10]))
ncut <- function(x, k = 10, labels = FALSE) {
    ## Check that x is numeric
    if (!is.numeric(x))
        stop("'x' must be numeric.")
    ## Check that k is a single positive integer > 1
    if (!(length(k) == 1 & k >= 1 & isTRUE(all.equal(k, as.integer(k)))))
        stop("'k' must be a single integer greater than 1.")
    ## Check that labels is either FALSE, NULL, or a vector of
    ## length k
    if (!is.null(labels))
        if (length(labels) == 1) {
            if (!(labels %in% c(FALSE, 1)))
                stop("'labels' must be FALSE, NULL, or a vector of length 'k'.")
        } else if (!(length(labels) == k))
            stop("'labels' must be FALSE, NULL, or a vector of length 'k'.")
    ## Cut according to k quantiles
    cut(x, quantile(x, probs = seq(0, 1, length.out = k + 1)),
        include.lowest = TRUE, labels = labels)
}
