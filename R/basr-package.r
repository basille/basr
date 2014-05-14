##' basr package
##'
##' This package provides a bunch of basic, but hopefully useful,
##' functions for a variety of usage. For a list of documented
##' functions, use \code{library(help = "basr")}
##'
##' @name basr
##' @title Utility functions
##' @docType package
##' @author Mathieu Basille \email{basille@@ase-research.org},
##' contributions from Samuel Brown, Marc in the box, Jean Lobry,
##' Kevin Wright
NULL


## capwords
##
##' Capitalizing - every first letter of a word is changed to upper
##' case.
##'
##' @title Capitalizing
##' @param s A character vector, or an object that can be coerced to
##' character by \code{as.character}.
##' @param strict Logical: other letters than the first are converted
##' to lower case
##' @return A character vector of the same length and with the same
##' attributes as \code{x} (after possible coercion).
##' @author From the help page of \code{\link[base]{chartr}}
##' @export
##' @examples
##' capwords(c("using AIC for model selection"))
##' ## ->  [1] "Using AIC For Model Selection"
##' capwords(c("using AIC", "for MODEL selection"), strict = TRUE)
##' ## ->  [1] "Using Aic"  "For Model Selection"
##' ##                ^^^        ^^^^^
##' ##               'bad'       'good'
capwords <- function(s, strict = FALSE)
{
    cap <- function(s) paste(toupper(substring(s, 1, 1)), {
        s <- substring(s, 2)
        if (strict)
            tolower(s)
        else s
    }, sep = "", collapse = " ")
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


## confint
##
##' Modified version of the \code{\link[stats]{confint}} function,
##' which displays the coefficients in addition to the CIs, and allows
##' for more control on display parameters. A \code{plot} argument and
##' function allow to graph the coefficients and their CIs.
##'
##' @title Confidence Intervals for Model Parameters
##' @param order Logical. If \code{TRUE}, the results are ordered by
##' descending order on the coefficient value.
##' @param groups A \code{factor} in the sense that
##' \code{as.factor(f)} defines the groups, or a list of such factors
##' in which case their interaction is used for the groups. See
##' \code{\link[base]{split}}.
##' @param plot Whether to plot the results.
##' @param ... Further arguments passed to \code{points}.
##' @return A data frame providing the CI and coefficients.
##' @seealso \code{\link[stats]{confint}} for more details on other
##' parameters.
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' ## Example of linear model
##' fit <- lm(100/mpg ~ disp + hp + wt + am, data = mtcars)
##' ## Standard 'confint' function
##' stats::confint(fit)
##' ## Same results with modified function
##' confint(fit)
##' ## Argument 'level'
##' stats::confint(fit, level = .9)
##' confint(fit, level = .9)
##' ## Argument 'order'
##' confint(fit, order = TRUE)
##' ## Argument 'groups'
##' confint(fit, groups = c(3, 1, 1, 1, 2))
##' ## Argument 'level', "'order' and 'groups' simultaneously
##' confint(fit, level = .9, order = TRUE, groups = c(3, 1, 1, 1, 2))
##' ## Argument 'parm'
##' stats::confint(fit, "am")
##' confint(fit, "am")
##'
##' ## Plot of the results
##' plot(confint(fit, order = TRUE, groups = c(3, 1, 1, 1, 2)))
##' confint(fit, order = TRUE, groups = c(3, 1, 1, 1, 2), plot = TRUE)
##' confint(fit, order = TRUE, groups = c(3, 1, 1, 1, 2), plot = TRUE,
##'     col = c("blue", "red", "green"), pch = 18, cex = 2)
##' confint(fit, order = TRUE, groups = c(3, 1, 1, 1, 2), level = 0.9,
##'     plot = TRUE)
confint <- function(object, parm, level = 0.95, order = FALSE,
    groups, plot = FALSE, ...) {
    ## Computing the CI using 'stats::confint'
    ci <- stats::confint(object = object, parm = parm, level = level,
        ...)
    ## Retrieving row and colum names
    rnci <- rownames(ci)
    cnci <- colnames(ci)
    ## Include the coefficients, and give it the name 'coef'
    ci <- data.frame(ci, coef(object)[parm])
    names(ci) <- c(cnci, "coef")
    ## If no 'groups', just check 'order'
    if (missing(groups)) {
        ## And if 'order', simply order the result by 'coef'
        if (order)
            ci <- ci[order(ci$coef, decreasing = TRUE), ]
    }
    ## If 'groups'...
    else {
        ## ... and if 'order'
        if (order) {
            ## Split the result according to the groups
            sci <- split(ci, groups)
            ## For each element, sort according to 'coef', and adjust
            ## the rownames; bind all element outputs together
            ci <- do.call("rbind", lapply(names(sci),
              function(x) {
                  tmp <- sci[[x]][order(sci[[x]]$coef, decreasing = TRUE), ]
                  rownames(tmp) <- paste(x, rownames(tmp), sep = ".")
                  return(tmp)
              }))
        }
        ## ... and no 'order'
        else {
            ## Split the result according to the groups, and bind
            ## all element outputs together
            ci <- do.call("rbind", split(ci, groups))
            ## Adjust the rownames
            rownames(ci) <- paste(groups, rnci, sep = ".")[order(groups)]
        }
        ## Set the attribute 'groups'
        attr(ci, "groups") <- groups
    }
    ## Set the class
    class(ci) <- c("confint", "data.frame")
    ## Set the attribute 'level'
    attr(ci, "level") <- level
    ## Set the attribute 'model'
    attr(ci, "model") <- as.character(substitute(object))
    ## If plot, call 'plot.confint'
    if (plot)
        plot(ci, ...)
    return(ci)
}
##' @param x A \code{confint} object.
##' @param mar The number of lines of margin, can be useful if the
##' coefficient names do not fit in the left margin. See
##' \code{\link[graphics]{par}} for more details.
##' @param col The color of each coefficient + CI; gray by default. If
##' \code{"groups"}, the color of each (sorted) group; use a hcl
##' palette by default.
##' @rdname confint
##' @export
plot.confint <- function(x, mar = c(5, 7, 3, 1) + 0.1, col = NULL,
    main = attr(x, "model"), pch = 19, ...)
{
    ## Check that 'x' is of class "confint"
    if (!inherits(x, "confint"))
        stop("x should be of class \"confint\"")
    ## Invert the order
    ci <- x[nrow(x):1, ]
    ## Define the color. If a "groups" attribute exists...
    if (!is.null(attr(ci, "groups"))) {
        ## ... and the color is not set, use a hcl palette
        if (is.null(col))
            col <- hcl(h = seq(50, 360, length.out = length(unique(attr(ci,
                "groups")))), c = 100, l = 50)
        ## In both cases, repeat the colors according to the groups
        col <- rev(rep(col, table(attr(ci, "groups"))))
    }
    ## But if there is no "groups" attribute, and no color set, use
    ## 'gray(O.3)'
    else if (is.null(col))
        col <- gray(0.3)
    ## Set 'par("mar")'
    par(mar = mar)
    ## Call a plot.default on 'ci'
    plot.default(ci[, 3], 1:nrow(ci), xlim = range(ci), axes = FALSE,
        xlab = paste("Confidence intervals, level = ", attr(ci,
            "level") * 100, "%", sep = ""), ylab = NA, main = main,
        type = "n")
    ## Add a grid
    grid()
    ## Add a vertical line at x = 0
    abline(v = 0)
    ## Add the confidence intervals
    for (i in 1:nrow(ci)) lines(x = c(ci[i, 1], ci[i, 2]), y = c(i,
        i))
    ## Add the points
    points(ci[, 3], 1:nrow(ci), col = col, pch = pch, ...)
    ## Add the axes
    axis(2, at = 1:nrow(ci), labels = FALSE)
    ## Add the row.names of the 'confint' call
    mtext(side = 2, text = row.names(ci), at = 1:nrow(ci), col = col,
        line = 1, las = 1)
    ## Add horizontal lines to visually differentiate the groups.
    if (length(unique(attr(ci, "groups"))) > 1)
        abline(h = (which(!duplicated(rev(sort(attr(ci, "groups"))))) -
            0.5)[-1], col = gray(0.3), lty = 2)
    ## Add the x axis
    axis(1)
    ## Add a bounding box
    box()
}
## See: http://christophergandrud.blogspot.ca/2012/09/graphically-comparing-confidence.html
## and: https://github.com/christophergandrud/GreenBook/blob/master/Analysis/BasicAnalysisCoefPlots.R
### Example with ggplot2:
### https://diffuseprior.wordpress.com/2012/04/23/probitlogit-marginal-effects-in-r-2/
### http://is-r.tumblr.com/post/38055963968/possibly-slightly-better-text-analysis-with-lme4


## cv
##
##' This function computes the coefficient of variation (i.e. \code{sd
##' / mean}) of the values in \code{x}.  If \code{ci} is \code{TRUE}
##' then confidence intervals are also computed.
##'
##' @title Coefficient of variation
##' @param x A numeric vector
##' @param na.rm Logical.  Should missing values be removed?
##' @param ci Logical. Should confidence intervals be computed?
##' @param conf.level Confidence level of the interval.
##' @param method The method to compute the confidence
##' intervale. Either the naive (\code{naive}), the McKay
##' (\code{mckay}) or the modified McKay (\code{mckaymod}, default)
##' approximation.
##' @return If \code{ci}, returns a list with the coefficient of
##' variation. in the first element and the confidence interval in the
##' second.
##' @author From Kevin Wright, modified by Mathieu Basille
##' \email{basille@@ase-research.org}
##' @section Original URL:
##' \url{http://tolstoy.newcastle.edu.au/R/e2/help/07/06/19043.html}
##' @references Vangel, M. G. (1996) Confidence intervals for a normal
##' coefficient of variation. The American Statistician, 50: 21-26
##' @export
##' @examples
##' xx <- 1:10
##' cv(xx)
##' sd(xx)/mean(xx)
##' cv(xx, ci = TRUE)
cv <- function(x, na.rm = FALSE, ci = FALSE, conf.level = 0.95, method = c("mckaymod",
    "mckay", "naive")) {
    if (na.rm)
        x <- na.omit(x)
    if (any(x < 0))
        warning("Coefficient of variation should only be computed on non-negative values.")
    v <- length(x) - 1
    mu <- mean(x)
    sigma <- sqrt(var(x))
    k <- sigma/mu
    if (ci) {
        alpha <- 1 - conf.level
        if (k > 0.33)
            warning("Confidence interval may be approximate.")
        method <- match.arg(method)
        t1 <- qchisq(1 - alpha/2, v)/v
        t2 <- qchisq(alpha/2, v)/v
        if (method == "naive") {
            lower <- k/sqrt(t1)
            upper <- k/sqrt(t2)
        }
        if (method == "mckay") {
            u1 <- v * t1
            u2 <- v * t2
            lower <- k/sqrt((u1/(v + 1) - 1) * k^2 + u1/v)
            upper <- k/sqrt((u2/(v + 1) - 1) * k^2 + u2/v)
        }
        if (method == "mckaymod") {
            u1 <- v * t1
            u2 <- v * t2
            lower <- k/sqrt(((u1 + 2)/(v + 1) - 1) * k^2 + u1/v)
            upper <- k/sqrt(((u2 + 2)/(v + 1) - 1) * k^2 + u2/v)
        }
        k <- list(k = k, ci = c(lower, upper))
        attr(k, "alpha") <- alpha
        attr(k, "method") <- method
    }
    return(k)
}


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



## extrange
##
##' Returns the range extended by a given proportion.
##'
##' If the regular range returns a single value, the proportion is
##' computed on this value itself (and not on the range).
##' @title Extended range
##' @param x A numeric vector.
##' @param percent The proportion to be added to the range.
##' @param na.rm Logical, indicating if \code{NA}'s should be omitted.
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' extrange(0:10)
##' extrange(0:10, percent = .5)
##' extrange(-10:10)
##' extrange(rep(10, 3))
extrange <- function(x, percent = 0.1, na.rm = FALSE) {
    rr <- range(x, na.rm = na.rm)
    dr <- diff(rr)
    if (dr == 0)
        dr <- abs(rr[1])
    return(c(rr[1] - dr * percent, rr[2] + dr * percent))
}


## getcolors
##
##' Allows for the selection of \code{n} colors by using a simplified color swatch.
##'
##' \code{getcolors} allows selection with a mouse using the
##' \code{locator} function. Following selection, a second plot opens
##' showing how these colors look next to each other and on a
##' background gradient of black to white. The function uses an RGB
##' color model: Red increases on the y-axis, Green increases on the
##' x-axis, and Blue is a repeated sequence of levels across the
##' x-axis.
##' @title Choosing colors visually
##' @param n The number of colors to choose
##' @return A character vector with elements of 7 or 9 characters,
##' \code{"#"} followed by the red, blue, green and optionally alpha
##' values in hexadecimal (after rescaling to \code{0 ... 255}).  The
##' optional alpha values range from \code{0} (fully transparent) to
##' \code{255} (opaque).
##' @author Marc in the box
##' @section Original URL:
##' \url{http://menugget.blogspot.com/2013/01/choosing-colors-visually-with-getcolors.html}
##' @export
##' @examples
##' \dontrun{
##' set.seed(1)
##' n <- 100
##' x <- seq(n)
##' y1 <- cumsum(rnorm(n))
##' y2 <- cumsum(rnorm(n))
##' y3 <- cumsum(rnorm(n))
##' y4 <- cumsum(rnorm(n))
##' ylim <- range(c(y1, y2, y3, y4))
##'
##' cols <- getcolors(4)
##'
##' plot(x, y1, ylim = ylim, t = "l", col = cols[1], lwd = 3, ylab = "")
##' lines(x, y2, col = cols[2], lwd = 3)
##' lines(x, y3, col = cols[3], lwd = 3)
##' lines(x, y4, col = cols[4], lwd = 3)
##' legend("topleft", legend = paste("y", 1:4, sep = ""), col = cols,
##'     lwd = 3)}
getcolors <- function(n)
{
    N <- 6
    X <- seq(N^2)
    Y <- seq(N)
    GRD <- expand.grid(x = X, y = Y)
    Z <- matrix(0, nrow = length(X), ncol = length(Y))
    LEV <- seq(0, 1, , N)
    R <- matrix(rep(LEV, each = N^2), nrow = length(X), ncol = length(Y))
    G <- matrix(rep(rep(LEV, each = N), N), nrow = length(X),
        ncol = length(Y))
    B <- matrix(rep(LEV, N^2), nrow = length(X), ncol = length(Y))
    x11(width = 6, height = 6)
    layout(matrix(1:2, nrow = 2, ncol = 1), widths = c(6), heights = c(1.5,
        4.5))
    op <- par(mar = c(1, 3, 2, 1))
    image(X, Y, Z, col = NA, xlab = "", ylab = "", xaxt = "n",
        yaxt = "n")
    for (i in seq(nrow(GRD))) {
        xs <- c(GRD$x[i] - 0.5, GRD$x[i] - 0.5, GRD$x[i] + 0.5,
            GRD$x[i] + 0.5)
        ys <- c(GRD$y[i] - 0.5, GRD$y[i] + 0.5, GRD$y[i] + 0.5,
            GRD$y[i] - 0.5)
        polygon(xs, ys, col = rgb(R[i], G[i], B[i]), border = NA)
    }
    mtext(paste("Click on", n, "colors [please]"), side = 3,
        line = 0.5)
    box()
    COLS <- NA * seq(n)
    for (i in seq(n)) {
        coord <- locator(1)
        COLS[i] <- rgb(R[round(coord$x), round(coord$y)], G[round(coord$x),
            round(coord$y)], B[round(coord$x), round(coord$y)])
    }
    par(mar = c(1, 3, 0, 1))
    pal <- colorRampPalette(c("black", "white"))
    image(x = 1:100, y = seq(n), z = matrix(rep(1:100, n), nrow = 100,
        ncol = n), col = pal(100), xlab = "", ylab = "", xaxt = "n",
        yaxt = "n")
    box()
    for (i in seq(n)) {
        lines(x = c(1, 100), y = c(i, i), col = COLS[i], lwd = 4)
    }
    axis(2, at = seq(n))
    par(op)
    COLS
}


## manual
##
##' Generate package reference manual. This function requires the
##' \code{devtools} package.
##'
##' @title Generate package reference manual
##' @param pkg package description, can be path or package name.  See
##' \code{\link[devtools]{as.package}} for more information
##' @param path path in which to produce package.  If \code{NULL},
##' defaults to the root directory of the package.
##' @param preview preview generated PDF file
##' @param overwrite overwrite output file if it exists
##' @export
##' @author Mathieu Basille \email{basille@@ase-research.org}
manual <- function(pkg = ".", path = NULL, preview = TRUE, overwrite = FALSE)
{
    require(devtools)
    pkg <- as.package(pkg)
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


## mv
##
##' Rename an R object.
##'
##' @title Rename an R object.
##' @param from The name of an R object, with or without quotes.
##' @param to The new name, with or without quotes.
##' @author Jean Lobry
##' @export
##' @examples
##' bla <- 2
##' ls()
##' mv(bla, bli)
##' bli
##' ls()
mv <- function(from, to)
{
    noms <- as.list(match.call())
    if (noms$from != noms$to) {
        eval.parent(parse(text = paste(noms$from, "->", noms$to)))
        eval.parent(parse(text = paste("rm(", noms$from, ")")))
    }
}


## nselect
##
##' Select a subset of a table with at least \code{n} occurrences of a
##' category.
##'
##' @title Subsetting tables given occurrences
##' @param x A data frame or a matrix to be subsetted.
##' @param col The column on which the occurrences are counted; can be
##' the name or the number of the column.
##' @param n The minimum number of occurrences for which to keep the
##' data.
##' @param droplevels Logical. If yes, unused levels from factors in
##' the data frame are dropped.
##' @return A data frame.
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' set.seed(1)
##' bla <- data.frame(value = rnorm(100), group = sample(letters[1:4],
##'     size = 100, replace = TRUE, prob = (1:4) * 10))
##' table(bla$group)
##' bli <- nselect(bla, 2, 25, droplevels = TRUE)
##' table(bli$group)
nselect <- function(x, col, n, droplevels = FALSE) {
    if (!is.data.frame(x) & !is.matrix(x))
        stop("x should be a data frame or a matrix")
    if (droplevels)
        return(droplevels(data.frame(x[x[, col] %in% names(which(table(x[,
            col]) >= n)), ])))
    else return(data.frame(x[x[, col] %in% names(which(table(x[,
        col]) >= n)), ]))
}


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


## togray
##
##' Convert a continuous variable to the corresponding levels of grey.
##'
##' @title Convert continuous variable to grey levels
##' @aliases togrey
##' @param x A numeric vector.
##' @param min The minimum grey level.
##' @param max The maximum grey level.
##' @param alpha The opacity.
##' @param inverse Logical. By default, bigger is darker. If
##' \code{inverse = TRUE}, bigger is lighter.
##' @param sqrt Logical. Applies a square root transformation to get more
##' progressive grey levels.
##' @return A vector of colors of the same length as \code{x}.
##' @author From Clement Calenge, modified by Mathieu Basille
##' \email{basille@@ase-research.org}
##' @export
##' @examples
##' bla <- runif(10000)
##' plot(bla, col = togray(bla, 0, 1), pch = 20)
##' plot(bla, col = togray(bla, 0, 1, sqrt = TRUE), pch = 20)
##' plot(bla, col = togray(bla, 0, 1, alpha = 0.5), pch = 20)
togray <- function(x, min = 0.1, max = 0.9, alpha = NULL, inverse = FALSE,
    sqrt = FALSE)
{
    ## Check that min < max
    if (min >= max)
        stop("'min' needs to be smaller than 'max'")
    ## Check that both 0 <= min/max <= 1
    if (min < 0 | max > 1)
        stop("'min' and 'max' need to be set between 0 and 1")
    ## By default, bigger is darker, i.e. the function works on the
    ## negative values of x, except if 'inverse = TRUE'
    if (!inverse)
        x <- -x
    ## Rescale x between min and max
    rg <- (max - min) * (x - min(x))/diff(range(x)) + min
    ## Remove the corner case where 0.000<0 and 1.000>1...
    rg <- ifelse(rg < 0, 0, rg)
    rg <- ifelse(rg > 1, 1, rg)
    ## Apply the square root if needed
    if (sqrt) {
        rg <- sqrt(rg)
        ## Needs to be rescaled...
        rg <- (max - min) * (rg - min(rg))/diff(range(rg)) +
            min
        ## Corner cases...
        rg <- ifelse(rg < 0, 0, rg)
        rg <- ifelse(rg > 1, 1, rg)
    }
    ## Return the conversion to grey levels
    return(grey(rg, alpha = alpha))
}
##
## togrey
##
##' @rdname togray
##' @export
togrey <- function(x, min = 0.1, max = 0.9, alpha = NULL, inverse = FALSE,
    sqrt = FALSE)
{
    ## Check that min < max
    if (min >= max)
        stop("'min' needs to be smaller than 'max'")
    ## Check that both 0 <= min/max <= 1
    if (min < 0 | max > 1)
        stop("'min' and 'max' need to be set between 0 and 1")
    ## By default, bigger is darker, i.e. the function works on the
    ## negative values of x, except if 'inverse = TRUE'
    if (!inverse)
        x <- -x
    ## Rescale x between min and max
    rg <- (max - min) * (x - min(x))/diff(range(x)) + min
    ## Remove the corner case where 0.000<0 and 1.000>1...
    rg <- ifelse(rg < 0, 0, rg)
    rg <- ifelse(rg > 1, 1, rg)
    ## Apply the square root if needed
    if (sqrt) {
        rg <- sqrt(rg)
        ## Needs to be rescaled...
        rg <- (max - min) * (rg - min(rg))/diff(range(rg)) +
            min
        ## Corner cases...
        rg <- ifelse(rg < 0, 0, rg)
        rg <- ifelse(rg > 1, 1, rg)
    }
    ## Return the conversion to grey levels
    return(grey(rg, alpha = alpha))
}


## q
##
##' A modified version of \code{\link[base]{quit}} or its alias
##' \code{\link[base]{q}}. See \code{\link[base]{quit}} for the
##' function details.
##'
##' If \code{save = "yes"}, the list of attached packages is
##' automatically saved in a file \code{.Rpackages}. See
##' \code{\link{savepkglist}} for more details.
##' @title Terminate an R Session
##' @aliases quit
##' @author R Core Team, modified by Mathieu Basille
##' \email{basille@@ase-research.org}
##' @export
q <- function(save = "default", status = 0, runLast = TRUE)
{
    if (save == "yes")
        savepkglist()
    .Internal(quit(save, status, runLast))
}
##
## quit
##
##' @rdname q
##' @export
quit <- function(save = "default", status = 0, runLast = TRUE)
{
    if (save == "yes")
        savepkglist()
    .Internal(quit(save, status, runLast))
}


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


## table
##
##' A slight modification of the \code{\link[base]{table}} function,
##' to include \code{NA} values in the table by default. See
##' \code{\link[base]{table}} for details of the function.
##'
##' @title Modified table function to handle NAs
##' @param useNA Whether to include \code{NA} values in the table.
##' Default is now \code{ifany}.
##' @author R Core Team, modified by Mathieu Basille
##' \email{basille@@ase-research.org}
##' @export
##' @examples
##' d <- factor(rep(c("A", "B", "C"), 10), levels = c("A", "B", "C",
##'     "D", "E"))
##' is.na(d) <- 3:4
##' d
##' table(d)
table <- function(..., exclude = if (useNA == "no") c(NA, NaN),
    useNA = c("ifany", "no", "always"), dnn = list.names(...),
    deparse.level = 1)
{
    list.names <- function(...) {
        l <- as.list(substitute(list(...)))[-1L]
        nm <- names(l)
        fixup <- if (is.null(nm))
            seq_along(l)
        else nm == ""
        dep <- vapply(l[fixup], function(x) switch(deparse.level +
            1, "", if (is.symbol(x)) as.character(x) else "",
            deparse(x, nlines = 1)[1L]), "")
        if (is.null(nm))
            dep
        else {
            nm[fixup] <- dep
            nm
        }
    }
    if (!missing(exclude) && is.null(exclude))
        useNA <- "always"
    useNA <- match.arg(useNA)
    args <- list(...)
    if (!length(args))
        stop("nothing to tabulate")
    if (length(args) == 1L && is.list(args[[1L]])) {
        args <- args[[1L]]
        if (length(dnn) != length(args))
            dnn <- if (!is.null(argn <- names(args)))
                argn
            else paste(dnn[1L], seq_along(args), sep = ".")
    }
    bin <- 0L
    lens <- NULL
    dims <- integer()
    pd <- 1L
    dn <- NULL
    for (a in args) {
        if (is.null(lens))
            lens <- length(a)
        else if (length(a) != lens)
            stop("all arguments must have the same length")
        cat <- if (is.factor(a)) {
            if (any(is.na(levels(a))))
                a
            else {
                if (is.null(exclude) && useNA != "no")
                  addNA(a, ifany = (useNA == "ifany"))
                else {
                  if (useNA != "no")
                    a <- addNA(a, ifany = (useNA == "ifany"))
                  ll <- levels(a)
                  a <- factor(a, levels = ll[!(ll %in% exclude)],
                    exclude = if (useNA == "no")
                      NA)
                }
            }
        }
        else {
            a <- factor(a, exclude = exclude)
            if (useNA != "no")
                addNA(a, ifany = (useNA == "ifany"))
            else a
        }
        nl <- length(ll <- levels(cat))
        dims <- c(dims, nl)
        if (prod(dims) > .Machine$integer.max)
            stop("attempt to make a table with >= 2^31 elements")
        dn <- c(dn, list(ll))
        bin <- bin + pd * (as.integer(cat) - 1L)
        pd <- pd * nl
    }
    names(dn) <- dnn
    bin <- bin[!is.na(bin)]
    if (length(bin))
        bin <- bin + 1L
    y <- array(tabulate(bin, pd), dims, dimnames = dn)
    class(y) <- "table"
    y
}


## values
##
##' Replaces given values of a vector by new values.
##'
##' @title Change the values of a vector.
##' @param x A character or numeric vector.
##' @param from A vector describing the values to change from.
##' @param to A vector describing the values to change to.
##' @return A vector.
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' (bla <- rep(1:5, 3))
##' values(bla, c(3, 4), c(7, 3))
##' values(bla, c(3, 4), c("a", "b"))
##' (bli <- rep(letters[1:5], 3))
##' values(bli, c("b", "d"), c(1, 2))
##' blu <- rpois(1e6, 10)
##' system.time(values(blu, c(3, 4), c(7, 3)))
values <- function(x, from, to)
{
    if (!is.vector(x))
        stop("x must be a vector (numeric or character)")
    if (length(from) != length(to))
        stop("from and to must have the same length")
    for (i in 1:length(from)) x[x == from[i]] <- to[i]
    return(x)
}


## writeFunction
##
##' Prints a function to a file.
##'
##' @title Function output
##' @param fun A function.
##' @param file A character string naming a file. By default, write
##' the function in \code{<fun>.R} in the working directory.
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' f1 <- function(x) {
##'     ## Comment
##'     print(x)
##' }
##' writeFunction(f1)
##' rm(f1)
##' source("f1.R")
##' file.remove("f1.R")
##' f1(3)
writeFunction <- function(fun, file = NULL)
{
    if (!is.function(fun))
        stop("fun should be a function")
    name <- deparse(substitute(fun))
    if (is.null(file))
        connection <- file(paste(name, ".R", sep = ""),
            "w")
    else connection <- file(file)
    writeChar(paste(name, " <- ", sep = ""), connection, eos = NULL)
    dput(fun, connection)
    close(connection)
}
