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
