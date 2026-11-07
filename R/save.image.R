##' Save the current workspace (including attached packages)
##'
##' A modified version of \code{\link[base]{save.image}} that allows to save the
##' commands history and the list of attached packages. See
##' \code{\link[base]{save.image}} for the function details.
##'
##' @param file A (writable binary-mode) connection or the name of the file
##'     where the data will be saved (when tilde expansion is done). Must be a
##'     file name for ‘save.image’ or ‘version = 1’.
##' @param version The workspace format version to use.  NULL specifies the
##'     current default format (3).  Version 1 was the default from R 0.99.0 to
##'     R 1.3.1 and version 2 from R 1.4.0 to 3.5.0. Version 3 is supported from
##'     R 3.5.0.
##' @param ascii If ‘TRUE’, an ASCII representation of the data is written. The
##'     default value of ‘ascii’ is ‘FALSE’ which leads to a binary file being
##'     written.  If ‘NA’ and ‘version >= 2’, a different ASCII representation
##'     is used which writes double/complex numbers as binary fractions.
##' @param compress Logical or character string specifying whether saving to a
##'     named file is to use compression.  ‘TRUE’ corresponds to ‘gzip’
##'     compression, and character strings ‘"gzip"’, ‘"bzip2"’, ‘"xz"’ or
##'     ‘"zstd"’ specify the type of compression.  Ignored when file is a
##'     connection and for workspace format version 1.
##' @param safe Logical. If ‘TRUE’, a temporary file is used for creating the
##'     saved workspace. The temporary file is renamed to file if the save
##'     succeeds.  This preserves an existing workspace file if the save fails,
##'     but at the cost of using extra disk space during the save.
##' @param hist Logical. Whether to save or not the commands history.
##' @param h.file The name of the file in which to save the history, or from
##'     which to load it. The path is relative to the current working directory.
##' @param pkglist Logical. Whether to save or not the list of attached packages
##'     (default is \code{TRUE}).
##' @param p.file The name of the file in which to save the list of attached
##'     packages, or from which to load it. The path is relative to the current
##'     working directory.
##' @seealso \code{\link[utils]{savehistory}} to save the commands history, and
##'     \code{\link{savepkglist}} to save the list of attached packages.
##' @author R Core Team, modified by Mathieu Basille
##'     <\email{mathieu@@basille.org}>
##' @export
##' @importFrom utils savehistory
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
        if (inherits(history, "try-error"))
            warning(history)
    }
    if (pkglist) {
        pkglist <- try(savepkglist(file = p.file), silent = TRUE)
        if (inherits(pkglist, "try-error"))
            warning(pkglist)
    }
    on.exit()
}
