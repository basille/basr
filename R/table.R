##' Modified table function to handle NAs
##'
##' A slight modification of the \code{\link[base]{table}} function, to include
##' \code{NA} values in the table by default. See \code{\link[base]{table}} for
##' details of the function.
##'
##' @param ... One or more objects which can be interpreted as factors
##'     (including numbers or character strings), or a list (such as a data
##'     frame) whose components can be so interpreted. (For ‘as.table’,
##'     arguments passed to specific methods; for ‘as.data.frame’, unused.)
##' @param exclude Levels to remove for all factors in ‘...’.  If it does not
##'     contain NA and ‘useNA’ is not specified, it implies ‘useNA = "ifany"’.
##'     See ‘Details’ for its interpretation for non-factor arguments.
##' @param useNA Whether to include \code{NA} values in the table.  Default is
##'     now \code{ifany}.
##' @param dnn The names to be given to the dimensions in the result (the
##'     _dimnames names_).
##' @param deparse.level Controls how the default ‘dnn’ is constructed. See
##'     \code{\link[base]{table}} for details.
##' @author R Core Team, modified by Mathieu Basille
##'     <\email{mathieu@@basille.org}>
##' @export
##' @examples
##' d <- factor(rep(c("A", "B", "C"), 10), levels = c("A", "B", "C",
##'     "D", "E"))
##' is.na(d) <- 3:4
##' d
##' table(d)
table <- function(..., exclude = if (useNA == "no") c(NA, NaN),
    useNA = c("ifany", "no", "always"), dnn = list.names(...),
    deparse.level = 1)
{
    list.names <- function(...) {
        l <- as.list(substitute(list(...)))[-1L]
        nm <- names(l)
        fixup <- if (is.null(nm))
            seq_along(l)
        else nm == ""
        dep <- vapply(l[fixup], function(x) switch(deparse.level +
            1, "", if (is.symbol(x)) as.character(x) else "",
            deparse(x, nlines = 1)[1L]), "")
        if (is.null(nm))
            dep
        else {
            nm[fixup] <- dep
            nm
        }
    }
    if (!missing(exclude) && is.null(exclude))
        useNA <- "always"
    useNA <- match.arg(useNA)
    args <- list(...)
    if (!length(args))
        stop("nothing to tabulate")
    if (length(args) == 1L && is.list(args[[1L]])) {
        args <- args[[1L]]
        if (length(dnn) != length(args))
            dnn <- if (!is.null(argn <- names(args)))
                argn
            else paste(dnn[1L], seq_along(args), sep = ".")
    }
    bin <- 0L
    lens <- NULL
    dims <- integer()
    pd <- 1L
    dn <- NULL
    for (a in args) {
        if (is.null(lens))
            lens <- length(a)
        else if (length(a) != lens)
            stop("all arguments must have the same length")
        cat <- if (is.factor(a)) {
            if (any(is.na(levels(a))))
                a
            else {
                if (is.null(exclude) && useNA != "no")
                  addNA(a, ifany = (useNA == "ifany"))
                else {
                  if (useNA != "no")
                    a <- addNA(a, ifany = (useNA == "ifany"))
                  ll <- levels(a)
                  a <- factor(a, levels = ll[!(ll %in% exclude)],
                    exclude = if (useNA == "no")
                      NA)
                }
            }
        }
        else {
            a <- factor(a, exclude = exclude)
            if (useNA != "no")
                addNA(a, ifany = (useNA == "ifany"))
            else a
        }
        nl <- length(ll <- levels(cat))
        dims <- c(dims, nl)
        if (prod(dims) > .Machine$integer.max)
            stop("attempt to make a table with >= 2^31 elements")
        dn <- c(dn, list(ll))
        bin <- bin + pd * (as.integer(cat) - 1L)
        pd <- pd * nl
    }
    names(dn) <- dnn
    bin <- bin[!is.na(bin)]
    if (length(bin))
        bin <- bin + 1L
    y <- array(tabulate(bin, pd), dims, dimnames = dn)
    class(y) <- "table"
    y
}
