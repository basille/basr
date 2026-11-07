##' Generate package reference manual
##'
##' Generate package reference manual.
##'
##' @param pkg package description, can be path or package name.  See
##'     \code{\link[devtools]{as.package}} for more information
##' @param path path in which to produce package.  If \code{NULL}, defaults to
##'     the root directory of the package.
##' @param preview preview generated PDF file
##' @param overwrite overwrite output file if it exists
##' @export
##' @author Mathieu Basille \email{mathieu@@basille.org}
manual <- function(pkg = ".", path = NULL, preview = TRUE, overwrite = FALSE)
{
    pkg <- devtools::as.package(pkg)
    if (is.null(path))
        path <- paste(dirname(pkg$path), noquote(pkg$package),
            sep = "/")
    prev <- ifelse(preview, "", " --no-preview")
    over <- ifelse(overwrite, " --force", "")
    cmd <- paste("R CMD Rd2pdf ", shQuote(pkg$path), " -o ",
        path, "/", noquote(pkg$package), ".pdf", prev, over,
        sep = "")
    system(cmd)
}
