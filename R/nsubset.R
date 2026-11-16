#' Subset vectors, matrices and data frames based on value frequency
#'
#' Subset vectors and tables (\code{data.frames} and \code{matrix}) based on the
#' frequency of the values (of the vector or a column of the table), and a type
#' of inequality (less than and greater than).
#'
#' @param x An object to be subsetted, i.e. a vector of atomic type ('logical',
#'     'integer', 'numeric', 'complex', 'character' and 'raw'), a factor, a
#'     \code{data.frame} or a \code{matrix}.
#' @param n An integer; either a single value or the same number of values than
#'     returned by \code{table(x)} / \code{table(x[, "col"])}.
#' @param comp The sign of the inequality, one of \code{">="} (default),
#'     \code{">"}, \code{"<"} or \code{"<="}.
#' @param na.rm Logical; whether NAs should be removed (default) or not (in
#'     which case, they will be considered as a value of their own).
#' @param col Name of the column which is used to compute frequencies (with or
#'     without quotes).
#' @param select Expression, indicating columns to select from a data frame.
#' @param drop Passed on to \code{[} indexing operator.
#' @param ... Further arguments passed to or from other methods.
#' @return A subset of the same class as \code{x}.
#' @export
#' @author Mathieu Basille <mathieu@@basille.org>
#' @examples
#'
#' ## Simples examples and NAs:
#' set.seed(42)
#' bla <- rpois(100, 4)
#' bla[(1:9)*10] <- NA
#' bla
#' table(bla, useNA = "ifany")
#' nsubset(bla, 5)
#' nsubset(bla, 14)
#' nsubset(bla, 5, comp = "<")
#' nsubset(bla, 5, na.rm = FALSE)
#'
#' ## Factors and n provided for each value:
#' set.seed(42)
#' (bli <- factor(LETTERS[sample(1:10, size = 100, replace = TRUE)]))
#' table(bli)
#' factor(nsubset(bli, n = 7:16))
#'
#' ## Data frames:
#' set.seed(42)
#' bla <- data.frame(value = rnorm(100), group = sample(letters[1:4],
#'     size = 100, replace = TRUE, prob = (1:4) * 10))
#' table(bla$group)
#' bli <- nsubset(bla, group, 25)
#' head(bli)
#' table(bli$group)
#' nsubset(bla, group, 25, comp = ">", select = 1, drop = TRUE)
#' nsubset(bla, group, 25, comp = ">", select = group, drop = TRUE)
#'
#' ## Fast computation:
#' set.seed(42)
#' bla <- rpois(1e7, 4)
#' table(bla)
#' system.time(table(nsubset(bla, 1e7)))
#'
#' bla <- data.frame(value = rnorm(1e7), group = sample(letters[1:4],
#'     size = 1e7, replace = TRUE, prob = (1:4) * 10))
#' table(bla$group)
#' system.time(bli <- nsubset(bla, group, 25))
nsubset <- function(x, ...) {
    UseMethod("nsubset")
}

#' @rdname nsubset
#' @export
nsubset.default <- function(x, n, comp = c(">=", ">", "<", "<="), na.rm = TRUE, ...) {
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

#' @rdname nsubset
#' @export
nsubset.data.frame <- function(x, col, n, comp = c(">=", "<=", ">", "<", "=="),
    na.rm = TRUE, select, drop = FALSE, ...) {
    ## If 'col' is a not a character (i.e. an object/name), turn it into
    ## character:
    if (is.name(substitute(col))) {
        col <- deparse(substitute(col))
    }
    ## if (!is.character(col))
    ##     col <- deparse(substitute(col))
    ## Check the 'sign' argument
    comp <- match.arg(comp)
    ## Remove NAs if requested (default)
    if (na.rm) {
        x <- x[!is.na(x[, col]), ]
    }
    ## Get column
    xc <- x[, col]
    ## Frequency of each value
    tx <- table(xc, useNA = "ifany")
    ## Check that n is either a single value or the same number of values than
    ## returned by table(xc)
    if (!(length(n) == 1 | length(n) == length(tx))) {
        stop("n should either be a single value, or the same number of values than returned by 'table(x)'")
    }
    ## Check that n only contains positive integers
    if (!all(abs(n - round(n)) < .Machine$double.eps^0.5) | any(n <= 0)) {
        stop("n should only contain positive integers.")
    }
    ## Keep only selected variables
    vars <- if (missing(select)) {
        rep_len(TRUE, ncol(x))
    } else {
        nl <- as.list(seq_along(x))
        names(nl) <- names(x)
        eval(substitute(select), nl, parent.frame())
    }
    ## Does each freq validate the comparison?
    comp.res <- sapply(paste(tx, comp, n), function(t) eval(parse(text = t)))
    ## If all FALSE, return NULL
    if (all(comp.res == FALSE)) {
        return()
    }
    ## Otherwise, extract only values associated with TRUE
    x[x[, col] %in% names(tx[which(comp.res)]), vars, drop = drop]
}

#' @rdname nsubset
#' @export
nsubset.matrix <- function(x, col, n, comp = c(">=", "<=", ">", "<", "=="),
    na.rm = TRUE, select, drop = FALSE, ...) { # Same as nsubset.data.frame except for vars
    ## If 'col' is a not a character (i.e. an object/name), turn it into
    ## character:
    if (is.name(substitute(col))) {
        col <- deparse(substitute(col))
    }
    ## if (!is.character(col))
    ##     col <- deparse(substitute(col))
    ## Check the 'sign' argument
    comp <- match.arg(comp)
    ## Remove NAs if requested (default)
    if (na.rm) {
        x <- x[!is.na(x[, col]), ]
    }
    ## Get column
    xc <- x[, col]
    ## Frequency of each value
    tx <- table(xc, useNA = "ifany")
    ## Check that n is either a single value or the same number of values than
    ## returned by table(xc)
    if (!(length(n) == 1 | length(n) == length(tx))) {
        stop("n should either be a single value, or the same number of values than returned by 'table(x)'")
    }
    ## Check that n only contains positive integers
    if (!all(abs(n - round(n)) < .Machine$double.eps^0.5) | any(n <= 0)) {
        stop("n should only contain positive integers.")
    }
    ## Keep only selected variables
    if (missing(select))
        vars <- TRUE
    else {
        nl <- as.list(1L:ncol(x))
        names(nl) <- colnames(x)
        vars <- eval(substitute(select), nl, parent.frame())
    }
    ## Does each freq validate the comparison?
    comp.res <- sapply(paste(tx, comp, n), function(t) eval(parse(text = t)))
    ## If all FALSE, return NULL
    if (all(comp.res == FALSE)) {
        return()
    }
    ## Otherwise, extract only values associated with TRUE
    x[x[, col] %in% names(tx[which(comp.res)]), vars, drop = drop]
}
