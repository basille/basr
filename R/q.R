##' Terminate an R Session (with packages saved)
##'
##' A modified version of \code{\link[base]{q}} that allows to save the list of
##' attached packages. See \code{\link[base]{q}} for the function details.
##'
##' If \code{save = "yes"}, the list of attached packages is automatically saved
##' in a file \code{.Rpackages}. See \code{\link{savepkglist}} for more details.
##'
##' @param save A character string indicating whether the environment
##'     (workspace) should be saved, one of ‘"no"’, ‘"yes"’, ‘"ask"’ or
##'     ‘"default"’ (which is the default).
##' @param status The (numerical) error status to be returned to the operating
##'     system, where relevant.  Conventionally ‘0’ indicates successful
##'     completion.
##' @param runLast Should ‘.Last()’ be executed?
##' @author R Core Team, modified by Mathieu Basille
##'     <\email{mathieu@@basille.org}>
##' @export
q <- function(save = "default", status = 0, runLast = TRUE)
{
    if (save == "yes")
        savepkglist()
    base::quit(save, status, runLast)
}
