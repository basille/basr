## dynamitePlot
##
##' Creates dynamite plots.
##'
##' @title Dynamite Plots
##' @param height A vector of values describing the heights of the
##' rectangular bars which make up the plot.
##' @param error A vector of values indicating the length of error
##' bars.
##' @param names.arg A vector of names to be plotted below each bar or
##' group of bars.  If this argument is omitted, then the names are
##' taken from the \code{names} attribute of \code{height}.
##' @param significance A character vector giving the group
##' significance for each value.
##' @param ylim Limits for the y axis. By default, \code{ylim} uses
##' \code{c(0, maxLim)}, where \code{maxLim} is the maximum height +
##' error multiplied by a factor of 1.1.
##' @param sym Logical. Whether to draw lower error bars.
##' @param head A numeric, which gives the approximate width of the
##' head, relative to the bar width.
##' @param lwd The line width of the error bars, a _positive_ number,
##' defaulting to \code{par("lwd")} (usually 1).
##' @param cex.sig The magnification to be used for significance
##' groups relative to the current setting of \code{cex} (which
##' defaults to \code{1}).
##' @param ... Arguments to be passed to \code{barplot}.
##' @note Ben Bolker wrote an extensive discussion of the advantages
##' and disadvantages of dynamite plots here:
##' \url{http://emdbolker.wikidot.com/blog:dynamite}
##' @author Samuel Brown, modified by Mathieu Basille
##' \email{basille@@ase-research.org}
##' @section Original URL:
##' \url{http://the-praise-of-insects.blogspot.ca/2012/04/dynamite-plots-in-r.html}
##' @export
##' @examples
##' values <- c(1, 2, 5, 4)
##' errors <- c(0.25, 0.5, 0.33, 0.12)
##' names <- paste("Trial", 1:4)
##' sig <- c("a", "a", "b", "b")
##' dynamitePlot(values, errors)
##' par(mar = c(3, 5, 1, 1) + .1)
##' dynamitePlot(values, errors, names.arg = names, significance = sig,
##'     ylab = "Values", sym = TRUE, cex.lab = 1.5, cex.axis = 1.2,
##'     cex.names = 1.2, cex.sig = 1.5, space = c(0, 0.2, 0.8, 0.2),
##'     lwd = 2, head = 0, col = c(grey(0.5), "white"), border = c(NA,
##'         "black"))
dynamitePlot <- function(height, error, names.arg = NULL, significance = NA,
    ylim = c(0, maxLim), sym = FALSE, head = 0.7, lwd = par("lwd"),
    cex.sig = 1.2, ...)
{
    ## Define maxLim as max(height + error)*1.1
    maxLim <- 1.1 * max(mapply(sum, height, error))
    ## Plot and store the barplot
    bp <- barplot(height, names.arg = names.arg, ylim = ylim,
        ...)
    ## head as an approximate proportion of the bar width
    head <- 2 / length(height) * head
    ## Upper heads
    arrows(x0 = bp, y0 = height, y1 = height + error, angle = 90,
        lwd = lwd, length = head)
    ## Lower heads, if required
    if (sym)
        arrows(x0 = bp, y0 = height, y1 = height - error, angle = 90,
            lwd = lwd, length = head)
    ## Add significance group for each bar
    text(x = bp, y = height + error + 0.05 * (mean(height + error)),
        labels = significance, cex = cex.sig)
}
