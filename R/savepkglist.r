## savepkglist
##
##' Load or save the list of attached packages.
##'
##' \code{savepkglist} saves the list of all attached packages, except base
##' packages, in a file, with one package per line.
##'
##' \code{loadpkglist} loads a list of packages from a file. The file
##' should contain one package name per line, without quotes, and no
##' empty line. If the packages are not installed, the function sends
##' a warning.
##' @title Load or save the list of attached packages
##' @param file The name of the file in which to save the list of
##' attached packages, or from which to load it. The path is relative
##' to the current working directory.
##' @note To automatically load a \code{.Rpackages} list at the start
##' of R, add this in your \code{.Rprofile}:
##'
##' \code{### Silently load 'basr' together with default packages}
##'
##' \code{options(defaultPackages = c(getOption("defaultPackages"), "basr"))}
##'
##' \code{### Load packages at the start of R if the package list exists}
##'
##' \code{if (file.exists(".Rpackages"))}
##'
##' \code{    basr::loadpkglist(".Rpackages")}
##'
##' See \code{\link[base]{Startup}} for more details about the
##' initialization at start of an R session.
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' \dontrun{savepkglist(file = "list.Rpackages")}
savepkglist <- function(file = ".Rpackages")
{
    package <- grep("^package:", search(), value = TRUE)
    keep <- sapply(package, function(x) x == "package:base" ||
        !is.null(attr(as.environment(x), "path")))
    package <- sub("^package:", "", package[keep])
    pkgDesc <- lapply(package, packageDescription)
    basePkgs <- sapply(pkgDesc, function(x) !is.null(x$Priority) &&
        x$Priority == "base")
    if (any(!basePkgs))
        cat(paste(package[!basePkgs], "\n", sep = ""), file = file,
            sep = "")
    else stop("no attached package to save")
}
##
## loadpkglist
##
##' @rdname savepkglist
##' @export
##' @examples
##' \dontrun{loadpkglist()}
loadpkglist <- function(file = ".Rpackages") {
    if (file == ".Rpackages" & !file.exists(".Rpackages"))
        return(invisible())
    else {
        lapply(readLines(file), require, character.only = TRUE)
        return(invisible())
    }
}
