#' Terminate an R Session (with packages list saved)
#'
#' A modified version of \code{\link[base]{q}}/\code{\link[base]{quit}} that
#' allows to save the list of attached packages. See \code{\link[base]{quit}}
#' for the function details.
#'
#' If \code{savePackageList}, the list of attached packages is automatically
#' saved in a file \code{.Rpackages}. See \code{\link{savepkglist}} for more
#' details.
#'
#' @param save A character string indicating whether the environment (workspace)
#'     should be saved, one of \code{"no"} or \code{"yes"} (which is the
#'     default).
#' @param savePackageList Logical, whether to save the list of loaded packages
#'     (defaults to \code{TRUE}). Switched to \code{FALSE} if \code{save =
#'     "no"}.
#' @param status The (numerical) error status to be returned to the operating
#'     system, where relevant.  Conventionally ‘0’ indicates successful
#'     completion.
#' @param runLast Should ‘.Last()’ be executed?
#' @author Mathieu Basille <\email{mathieu@@basille.org}>
#' @export
#' @returns NULL
#' @examples
#' \dontrun{
#' ## Quit with package list saved:
#' q()}
q <- function(save = "yes", savePackageList = TRUE, status = 0, runLast = TRUE) {
    if (save == "no")
        savePackageList <- FALSE
    if (savePackageList)
        savepkglist()
    base::quit(save, status, runLast)
}

#' @rdname q
#' @export
quit <- q
