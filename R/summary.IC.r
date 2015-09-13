## summary.IC
##
##' Summarizes IC differences to identify plausible models (models
##' with highest empirical support).
##'
##' @title Summary of AIC/BIC
##' @param x A data frame with one row per model, and one column
##'     giving an information-theoretic index (AIC or BIC), such as
##'     returned by \code{\link[stats]{AIC}} or
##'     \code{\link[stats]{BIC}} with several models.
##' @param delta The difference threshold to identify models with
##'     similar support.
##' @return The input data frame with additional columns \code{delta}
##'     giving IC differences with the best model, and \code{best}
##'     identifying the best model(s) (\code{*} for the absolute best
##'     model, \code{+} for models within the threshold).
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' ## Prepare two models:
##' lm1 <- lm(Fertility ~ . , data = swiss)
##' lm2 <- update(lm1, . ~ . -Examination)
##'
##' ## Check AIC:
##' AIC(lm1, lm2)
##'
##' ## Summary of AIC and BIC:
##' summary.IC(AIC(lm1, lm2))
##' summary.IC(BIC(lm1, lm2))
summary.IC <- function(x, delta = 2) {
    if (!inherits(x, "data.frame"))
        stop("Object of class 'data.frame' expected.")
    if (!(any(names(x) %in% c("AIC", "BIC"))))
        stop("x should contain a column AIC or BIC.")
    ic <- which(names(x) %in% c("AIC", "BIC"))
    deltax <- x[, ic] - min(x[, ic])
    best <- ifelse(deltax == 0, "*", ifelse(deltax <= delta, "+", ""))
    return(data.frame(x, delta = deltax, best))
}
