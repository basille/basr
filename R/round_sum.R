#' Rounding that preserve sum
#'
#' Regular rounding can break the sum of values. This can be particularly
#' annoying when working on proportions that sum up to 1. \code{round_sum} works
#' around the problem by adjusting values to preserve the sum.
#'
#' @param x A numeric vector.
#' @param digits Integer indicating the number of decimal places. Negative
#'     values round to a power of ten, so for example \code{digits = -2} will
#'     round to the nearest hundred.
#' @return A numeric vector of the same length than \code{x} with rounded
#'     values.
#' @author Initial function from josliber
#'     (\url{https://stackoverflow.com/a/32544987}), improved by Mikhail
#'     (\url{https://stackoverflow.com/a/35930285}), and further improved by
#'     Mathieu Basille <mathieu@@basille.org>
#' @export
#' @examples
#' (toto <- c(1/3, 1/3, 1/3))
#' sum(toto)
#' #'
#' ## Regular rounding:
#' round(toto, 2)
#' sum(round(toto, 2))
#' #'
#' ## Rounding preserving sum:
#' round_sum(toto, 2)
#' sum(round_sum(toto, 2))
#' #'
#' ## Be careful when pushed to the extremes:
#' round(toto, 0)
#' round_sum(toto, 0)
#' #'
#' ## Works also with negative 'digits' to round to powers of ten:
#' (toto <- c(1/3, 1/3, 1/3)*1e6)
#' round(toto, -2)
#' round_sum(toto, -2)
round_sum <- function(x, digits = 0) {
    ## Checks that x is a numeric vector
    if (!is.numeric(x)) {
        stop("'x' must be a numeric vector")
    }
    ## Check that digits is a positive integer
    if (abs(digits - round(digits)) >= .Machine$double.eps^0.5 | length(digits) != 1) {
        stop("The number of digits must be a positive integer")
    }
    ## Move the decimal separator up k digits
    x <- x * 10^digits
    ## Round down
    x_floor <- floor(x)
    ## Select the indices of the elements with the k largest
    ## fractional parts, where k is the amount that we need to
    ## increase the sum to reach our target value
    k <- round(sum(x)) - sum(x_floor)
    indices <- order(x - x_floor)[(length(x) - k + 1):length(x)]
    ## Increment the output value in each of these indices bx_floor 1
    x_floor[indices] <- x_floor[indices] + 1
    ## Move the decimal separator back in place
    x_floor / 10^digits
}
