## savepkglist
##
##' Display, save or load the list of attached packages.
##'
##' \code{attpkglist} simply lists all attached packages (i.e. not base
##' packages).
##'
##' \code{savepkglist} saves the list of all attached packages in a file,
##' with one package per line.
##'
##' \code{loadpkglist} loads a list of packages from a file. The file
##' should contain one package name per line, without quotes, and no empty
##' line. If the packages are not installed, the function sends a
##' warning.
##'
##' \code{.loadpkglist} automatically loads the \code{.Rpackages} file at
##' startup (see the Note below).
##' @title Load or save the list of attached packages
##' @param file The name of the file in which to save the list of
##' attached packages, or from which to load it. The path is relative
##' to the current working directory.
##' @note To automatically load a \code{.Rpackages} list at startup,
##' add this in your \code{.Rprofile}:
##'
##' \code{### Load packages at the start of R if the package list exists}
##'
##' \code{basr:::.loadpkglist()}
##'
##' Essentially, the function appends the list of packages at the end of
##' the \code{defaultPackages} option (see \code{\link[options]{}} for this
##' option; see also \code{\link[base]{Startup}} for more details about the
##' initialization at start of an R session).
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' \dontrun{savepkglist(file = "list.Rpackages")}
savepkglist <- function(file = ".Rpackages") {
    packages <- attpkglist()
    if (is.null(packages))
        cat("no attached package to save")
    else cat(paste(packages, "\n", sep = ""), file = file, sep = "")
}


## attpkglist
##
##' @rdname savepkglist
##' @export
##' @examples
##' \dontrun{attpkglist()}
attpkglist <- function()
{
    package <- grep("^package:", search(), value = TRUE)
    keep <- sapply(package, function(x) x == "package:base" ||
        !is.null(attr(as.environment(x), "path")))
    package <- sub("^package:", "", package[keep])
    pkgDesc <- lapply(package, packageDescription)
    basePkgs <- sapply(pkgDesc, function(x) !is.null(x$Priority) &&
        x$Priority == "base")
    if (any(!basePkgs))
        return(package[!basePkgs])
    else return(NULL)
}


## loadpkglist
##
##' @rdname savepkglist
##' @export
##' @examples
##' \dontrun{loadpkglist()}
loadpkglist <- function(file = ".Rpackages") {
    lapply(readLines(file), require, character.only = TRUE)
    return(invisible())
}


## .loadpkglist
##
##' @rdname savepkglist
.loadpkglist <- function() {
    if (file.exists(".Rpackages"))
        options(defaultPackages = unique(c(getOption("defaultPackages"),
            rev(readLines(".Rpackages")))))
}
