##' Number of NAs by column
##'
##' Returns the number of NAs for each column of a data frame.
##'
##' @param x A data frame.
##' @returns A numeric vector.
##' @author Mathieu Basille <\email{mathieu@@basille.org}>
##' @export
##' @examples
##' set.seed(123)
##' (df <- data.frame(matrix(sample(c(NA, 1:10), 100, TRUE), ncol = 4)))
##' colNAs(df)
colNAs <- function(x) {
    if (!inherits(x, "data.frame"))
        stop("Object of class 'data.frame' expected.")
    colSums(is.na(x))
}
