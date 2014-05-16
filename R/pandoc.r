## pandoc --> now use knitr::pandoc
##
## Convert a file using \code{Pandoc}, which needs to be
## pre-installed on the user's machine.

## @title Convert using \code{Pandoc}
## @param file A markdown file to convert.
## @param format A character string specifying the output format,
## e.g. \code{html} (XHTML 1), \code{html5} (HTML 5), \code{latex}
## (LaTeX), \code{beamer} (LaTeX beamer slide show), \code{odt}
## (OpenOffice text document), \code{s5} (S5 HTML and javascript
## slide show), or \code{rtf} (rich text format). See the
## documentation for the complete list.
## @param output The output file, by default \code{index.html}. If an
## empty string (\code{""}) is given, the name of the input file with
## a .html extension is used.
## @param standalone Logical. Produce output with an appropriate
## header and footer (e.g. a standalone HTML, LaTeX, or RTF file, not
## a fragment).
## @param smart Logical. Produce typographically correct output.
## @param toc Logical. Include an automatically generated table of
## contents in the output document.
## @param options Additional options to pass to Pandoc.
## @param css A character string to link to a CSS style
## sheet. Multiple files can be included in a character vector. If
## missing, use \url{http://ase-research.org/R/rmd.css}.
## @seealso See Pandoc documentation for more details about the
## different options and Pandoc mechanism:
## \url{http://johnmacfarlane.net/pandoc/README.html}.
## @author Mathieu Basille \email{basille@@ase-research.org}
## @export
## @examples
## \dontrun{
## pandoc(file = "bla.md", output = "")}
## pandoc <- function(file, format = "html5", output = "index.html",
##     standalone = TRUE, smart = TRUE, toc = TRUE, options = NULL,
##     css)
## {
##     if (paste(suppressWarnings(tryCatch(system("pandoc -v", intern = T),
##         error = function(x) "NOPANDOC")), collapse = "\n") ==
##         "NOPANDOC")
##         stop("Pandoc is not installed or path of binary is not found.")
##     if (missing(file))
##         stop("A markdown file is necessary.")
##     else if (!grepl(".md", file))
##         stop("A markdown file is necessary.")
##     format <- paste("-t", format)
##     if (output == "")
##         output <- paste("-o", gsub(".md", ".html", file))
##     else output <- paste("-o", output)
##     if (standalone)
##         standalone <- "-s"
##     else standalone <- ""
##     if (smart)
##         smart <- "-S"
##     else smart <- ""
##     if (toc)
##         toc <- "--toc"
##     else toc <- ""
##     if (missing(css))
##         css <- "-c http://ase-research.org/R/rmd.css"
##     else css <- paste("-c", css, collapse = " ")
##     options <- paste(format, smart, standalone, toc, css, options)
##     system(paste("pandoc", options, file, output))
## }
