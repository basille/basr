## writeFunction
##
##' Prints a function to a file.
##'
##' @title Function output
##' @param fun A function.
##' @param file A character string naming a file. By default, write
##' the function in \code{<fun>.R} in the working directory.
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' f1 <- function(x) {
##'     ## Comment
##'     print(x)
##' }
##' writeFunction(f1)
##' rm(f1)
##' source("f1.R")
##' file.remove("f1.R")
##' f1(3)
writeFunction <- function(fun, file = NULL)
{
    if (!is.function(fun))
        stop("fun should be a function")
    name <- deparse(substitute(fun))
    if (is.null(file))
        connection <- file(paste(name, ".R", sep = ""),
            "w")
    else connection <- file(file)
    writeChar(paste(name, " <- ", sep = ""), connection, eos = NULL)
    dput(fun, connection)
    close(connection)
}
