## nsubset
##
##' Subset an object based on the frequency of a column (factor or
##' not).
##'
##' @title Subset according to frequency of a column
##' @param x object to be subsetted (e.g. \code{data.frame},
##'     \code{matrix}, \code{Spatial*DataFrame}, etc.).
##' @param col name of the column which stores the grouping factor
##'     (without quotes).
##' @param n the reference number for the frequency of the grouping
##'     factor (must be a numeric of length 1).
##' @param sign any comparison sign (\code{>=} by default, \code{<=},
##'     \code{>}, \code{<}, or \code{==}).
##' @param select expression, indicating columns to select (either
##'     name(s) without quote or numeric indicating the column
##'     number(s)).
##' @param drop passed on to '[' indexing operator.
##' @return A subset of the object (with the same class).
##' @author Mathieu Basille \email{basille@ufl.edu}
##' @export
##' @examples
##' set.seed(1)
##' bla <- data.frame(value = rnorm(100), group = sample(letters[1:4],
##'     size = 100, replace = TRUE, prob = (1:4) * 10))
##' table(bla$group)
##' bli <- nsubset(bla, group, 25)
##' table(bli$group)
##' blo <- nsubset(bla, group, 25, sign = ">", select = 1, drop = TRUE)
##' head(blo)
##' length(blo)
nsubset <- function (x, col, n, sign = c(">=", "<=", ">", "<",
    "=="), select, drop = FALSE)
{
    ## Check that the 'col' argument is a name, not a numeric or a
    ## character
    if (class(substitute(col)) %in% c("numeric", "character"))
        stop("'col' must be a name without quotes.")
    ## Check that 'n' is a single numeric:
    if (class(n) != "numeric" & length(n) != 1)
        stop("'n' must be a numeric of length 1.")
    ## Check the 'sign' argument
    sign <- match.arg(sign)
    ## Make a different call depending on the sign; substitute
    ## replaces the call by the values given in the arguments
    nsubset <- switch(sign,
        ">=" = substitute(subset(x, table(col)[col] >= n, select, drop)),
        "<=" = substitute(subset(x, table(col)[col] <= n, select, drop)),
        ">" = substitute(subset(x, table(col)[col] > n, select, drop)),
        "<" = substitute(subset(x, table(col)[col] < n, select, drop)),
        "==" = substitute(subset(x, table(col)[col] == n, select, drop))
        )
    ## Evaluate the call, let 'subset' do the work, and return the
    ## outcome
    eval(nsubset, parent.frame())
}
