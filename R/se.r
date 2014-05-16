## se
##
##' This function computes the standard error (i.e. \code{sd /
##' sqrt(n)}) of the values in \code{x}.  If \code{na.rm} is
##' \code{TRUE} then missing values are removed before computation
##' proceeds.
##'
##' @title Standard errors
##' @param x A numeric vector or an R object which is coercible to one
##' by \code{as.vector}.
##' @param na.rm Logical.  Should missing values be removed?
##' @seealso \code{\link[stats]{var}} and \code{\link[stats]{sd}} for
##' the variance and standard deviation.
##' @author From the \code{Writing R Extensions} manual, modified by
##' Mathieu Basille \email{basille@@ase-research.org}
##' @section Original URL:
##' \url{http://cran.r-project.org/doc/manuals/R-intro.html}
##' @export
##' @examples
##' bla <- rnorm(1000, sd = 100)
##' sd(bla)
##' sqrt(var(bla)/length(bla))
##' se(bla)
##'
##' is.na(bla) <- 200:300
##' sd(bla, na.rm = TRUE)
##' se(bla, na.rm = TRUE)
se <- function(x, na.rm = FALSE)
{
    return(sqrt(var(x, na.rm = na.rm)/sum(!is.na(x))))
}
