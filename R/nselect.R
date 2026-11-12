#' Select values based on their frequency
#'
#' Select values of an object based on the frequency of its items, and a type of
#' inequality (less than and greater than).
#'
#' @param x A vector of atomic types ('logical', 'integer', 'numeric',
#'     'complex', 'character' and 'raw') or a factor.
#' @param n Integer; either a single value or the same number of values than
#'     returned by \code{table(x)}.
#' @param comp The sign of the inequality, one of \code{">="} (default),
#'     \code{">"}, \code{"<"} or \code{"<="}.
#' @param na.rm Logical; whether NAs should be removed (default) or not (in
#'     which case, they will be considered as a value of their own).
#' @param ... Further arguments passed to or from other methods.
#' @return A vector (or a factor) of the same class as \code{x}.
#' @export
#' @author Mathieu Basille
#' @examples
#' ## Simples examples and NAs:
#' bla <- rpois(100, 4)
#' bla[(1:9)*10] <- NA
#' table(bla)
#' nselect(bla, 5)
#' nselect(bla, 5, comp = "<")
#' nselect(bla, 12)
#' nselect(bla, 5, na.rm = TRUE)
#'
#' ## Factors and n provided for each value:
#' bli <- factor(LETTERS[sample(1:10, size = 100, replace = TRUE)])
#' table(bli)
#' factor(nselect(bli, n = 7:16))
#'
#' ## Fast computation:
#' bla <- rpois(1e7, 4)
#' table(bla)
#' system.time(table(nselect(bla, 1e7)))
#' @export
nselect <- function(x, n, comp = c(">=", ">", "<", "<="), na.rm = TRUE, ...) {
    UseMethod("nselect")
}

#' @rdname nselect
#' @export
nselect.default <- function(x, n, comp = c(">=", ">", "<", "<="), na.rm = TRUE, ...) {
    ## Check that x is atomic (i.e. a vector, but not a list, or a factor)
    if (!is.atomic(x)) {
        stop("x should be a vector of atomic types ('logical', 'integer', 'numeric', 'complex', 'character' and 'raw') or a factor.")
    }
    ## Match 'comp' arguments
    comp <- match.arg(comp)
    ## Remove NAs if requested (default)
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    ## Frequency of each value
    tx <- table(x, useNA = "ifany")
    ## Check that n is either a single value or the same number of values than
    ## returned by table(x)
    if (!(length(n) == 1 | length(n) == length(tx)))
        stop("n should either be a single value, or the same number of values than returned by 'table(x)'")
    ## Check that n only contains positive integers
    if (!all(abs(n - round(n)) < .Machine$double.eps^0.5) | any(n <= 0))
        stop("n should only contain positive integers.")
    ## Does each freq validate the comparison?
    comp.res <- sapply(paste(tx, comp, n), function(t) eval(parse(text = t)))
    ## If all FALSE, return NULL
    if (all(comp.res == FALSE))
        return()
    ## Otherwise, extract only values associated with TRUE
    x[x %in% names(tx[which(comp.res)])]
}
