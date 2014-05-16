## save.image
##
##' A modified version of \code{\link[base]{save.image}} that allows
##' to save the commands history and the list of attached
##' packages. See \code{\link[base]{save.image}} for the function
##' details.
##'
##' @title Save the current workspace
##' @param hist Logical. Whether to save or not the commands history.
##' @param h.file The name of the file in which to save the history,
##' or from which to load it. The path is relative to the current
##' working directory.
##' @param pkglist Logical. Whether to save or not the list of
##' attached packages (default is \code{TRUE}).
##' @param p.file The name of the file in which to save the list of
##' attached packages, or from which to load it. The path is relative
##' to the current working directory.
##' @seealso \code{\link[utils]{savehistory}} to save the commands
##' history, and \code{\link{savepkglist}} to save the list of
##' attached packages.
##' @author R Core Team, modified by Mathieu Basille
##' \email{basille@@ase-research.org}
##' @export
save.image <- function(file = ".RData", version = NULL, ascii = FALSE,
    compress = !ascii, safe = TRUE, hist = TRUE, h.file = ".Rhistory",
    pkglist = TRUE, p.file = ".Rpackages") {
    if (!is.character(file) || file == "")
        stop("'file' must be non-empty string")
    opts <- getOption("save.image.defaults")
    if (is.null(opts))
        opts <- getOption("save.defaults")
    if (missing(safe) && !is.null(opts$safe))
        safe <- opts$safe
    if (missing(ascii) && !is.null(opts$ascii))
        ascii <- opts$ascii
    if (missing(compress) && !is.null(opts$compress))
        compress <- opts$compress
    if (missing(version))
        version <- opts$version
    if (safe) {
        outfile <- paste0(file, "Tmp")
        i <- 0
        while (file.exists(outfile)) {
            i <- i + 1
            outfile <- paste0(file, "Tmp", i)
        }
    }
    else outfile <- file
    on.exit(file.remove(outfile))
    save(list = ls(envir = .GlobalEnv, all.names = TRUE), file = outfile,
        version = version, ascii = ascii, compress = compress,
        envir = .GlobalEnv, precheck = FALSE)
    if (safe)
        if (!file.rename(outfile, file)) {
            on.exit()
            stop("image could not be renamed and is left in ",
                outfile)
        }
    if (hist) {
        history <- try(savehistory(file = h.file), silent = TRUE)
        if (class(history) == "try-error")
            warning(history)
    }
    if (pkglist) {
        pkglist <- try(savepkglist(file = p.file), silent = TRUE)
        if (class(pkglist) == "try-error")
            warning(pkglist)
    }
    on.exit()
}
