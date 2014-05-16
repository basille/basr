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
