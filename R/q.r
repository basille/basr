## q
##
##' A modified version of \code{\link[base]{quit}} or its alias
##' \code{\link[base]{q}}. See \code{\link[base]{quit}} for the
##' function details.
##'
##' If \code{save = "yes"}, the list of attached packages is
##' automatically saved in a file \code{.Rpackages}. See
##' \code{\link{savepkglist}} for more details.
##' @title Terminate an R Session
##' @aliases quit
##' @author R Core Team, modified by Mathieu Basille
##' \email{basille@@ufl.edu}
##' @export
q <- function(save = "default", status = 0, runLast = TRUE)
{
    if (save == "yes")
        savepkglist()
    .Internal(quit(save, status, runLast))
}
##
## quit
##
##' @rdname q
##' @export
quit <- function(save = "default", status = 0, runLast = TRUE)
{
    if (save == "yes")
        savepkglist()
    .Internal(quit(save, status, runLast))
}
