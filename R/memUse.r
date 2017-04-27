## memUse
##
##' Show memory usage of saved objects.
##'
##' @title Memory usage of saved objects
##' @param pos Which environment to use to list the saved objects (as
##' a position in the search list). See \code{\link{ls}}.
##' @param pattern An optional regular expression. See
##' \code{\link{ls}}.
##' @return A data frame of classe \code{memUse} providing object
##' names, class and memory usage; or \code{0} if no object is present
##' in the specified environment.
##' @author Michael Hallquist, modified by Mathieu Basille
##' \email{basille@@ufl.edu}
##' @section Original URL:
##' \url{http://stackoverflow.com/a/9839949}. See also the complete
##' Stack Overflow thread:
##' \url{https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session}
##' @export
##' @examples
##' ## Create some data
##' rand <- rnorm(100)
##' data(iris)
##' foo <- function(x) print(x)
##' ##'
##' ## Check memory usage (and raw numbers in bytes)
##' memUse()
##' memUse()$bytes
memUse <- function(pos = 1, pattern) {
    ## List saved objects
    objectList <- ls(pos = pos, pattern = pattern)
    ## Return 0 with a warning if there are no objects
    if (length(objectList) == 0) {
        warning("No object in the specified environment.")
        return(0)
    }
    ## Check object sizes in bytes
    objectBytes <- sapply(objectList, function(x) object.size(eval(parse(text = x))))
    ## Use of 'format' with 'units = "auto"' for human-readable
    ## printing
    objectSize <- sapply(objectList, function(x) format(object.size(eval(parse(text = x))),
        units = "auto"))
    ## Check object classes
    objectClass <- sapply(objectList, function(x) class(eval(parse(text = x)))[1])
    ## Or mode?
    objectMode <- sapply(objectList, function(x) mode(eval(parse(text = x)))[1])
    objectClass <- ifelse(is.na(objectClass), objectMode, objectClass)
    ## Create the data frame with all information
    memListing <- data.frame(row.names = objectList, class = objectClass,
        bytes = objectBytes, size = objectSize)
    ## And class 'memUse'
    class(memListing) <- c("memUse", "data.frame")
    return(memListing)
}

## print.memUse
##
##' @param x An object of class \code{memUse}.
##' @param sort Whether to sort the object list by \code{size} or
##' \code{alphabetical} order.
##' @param decreasing Logical.  Should the sort order be increasing or
##' decreasing?
##' @param n A single integer, giving the number of objects to
##' display.
##' @param bytes Logical. Whether to display the raw size in bytes.
##' @rdname memUse
##' @export
print.memUse <- function(x, sort = c("size", "alphabetical"),
    decreasing = ifelse(sort == "size", TRUE, FALSE), n = 10,
    bytes = FALSE, ...)
{
    ## Check arguments in 'sort'
    sort <- match.arg(sort)
    ## Sort alphabetically
    if (sort == "alphabetical")
        xx <- x[order(row.names(x), decreasing = decreasing),
            ]
    ## ... or by size
    else xx <- x[order(x$bytes, decreasing = decreasing), ]
    ## Remove the column 'bytes' for pretty printing
    if (!bytes)
        xx$bytes <- NULL
    ## Reset class to "data.frame"
    class(xx) <- "data.frame"
    ## And print using 'head'
    print(head(xx, n = n))
}
