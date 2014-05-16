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
