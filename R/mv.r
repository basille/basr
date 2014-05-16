## mv
##
##' Rename an R object.
##'
##' @title Rename an R object.
##' @param from The name of an R object, with or without quotes.
##' @param to The new name, with or without quotes.
##' @author Jean Lobry
##' @export
##' @examples
##' bla <- 2
##' ls()
##' mv(bla, bli)
##' bli
##' ls()
mv <- function(from, to)
{
    noms <- as.list(match.call())
    if (noms$from != noms$to) {
        eval.parent(parse(text = paste(noms$from, "->", noms$to)))
        eval.parent(parse(text = paste("rm(", noms$from, ")")))
    }
}
