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
