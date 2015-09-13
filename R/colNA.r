## colNA
##
##' Returns the number of NAs for each column of a data frame.
##'
##' @title Number of NAs by column
##' @param x A data frame.
##' @return A numeric vector
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' set.seed(123)
##' (df <- data.frame(matrix(sample(c(NA, 1:10), 100, TRUE), ncol = 4)))
##' colNA(df)
colNA <- function(x) {
    if (!inherits(x, "data.frame"))
        stop("Object of class 'data.frame' expected.")
    colSums(is.na(x))
}
