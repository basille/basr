* multiple

multiple <- function(x, unique = FALSE, na.rm = FALSE)
{
    ## Check that x is a vector
    if (!is.vector(x))
        stop("x should be a vector.")
    ## 'na.rm = TRUE' removes all NAs
    if (na.rm)
        x <- na.omit(x)
    ## If 'unique', returns unique x that appears only once
    if (unique)
        return(x[!(x %in% x[duplicated(x)])])
    ## Else return unique x that appears at least twice
    unique(x[duplicated(x)])
}
(toto <- rep(1:4, times = 1:4))
unique(toto)
multiple(toto)
unique(nselect(toto, 2))
multiple(toto, unique = TRUE)
unique(nselect(toto, 2, comp = "<"))

* counts

counts <- function(x, exclude = if (useNA == "no") c(NA, NaN),
    useNA = c("no", "ifany", "always"), ...)
{
    ## Match 'useNA' arguments
    useNA <- match.arg(useNA)
    ## Simply count the frequency of each frequency
    table(table(x, exclude = exclude, useNA = useNA, ...))
}
(toto <- rep(letters[1:10], times = rep(4:1, times = 1:4)))
counts(toto)

* Rounding that preserve sum

From josliber: https://stackoverflow.com/a/32544987
Improved by Mikhail: https://stackoverflow.com/a/35930285

round_sum <- function(x, digits = 0) {
    ## Check that digits is a positive integer
    if (digits%%1 != 0 | digits < 0) 
        stop("The number of digits must be a positive integer")
    ## Move the decimal separator up k digits
    up <- 10^digits
    x <- x * up
    ## Round down
    y <- floor(x)
    ## Select the indices of the elements with the k largest
    ## fractional parts, where k is the amount that we need to
    ## increase the sum to reach our target value
    indices <- tail(order(x - y), round(sum(x)) - sum(y))
    ## Increment the output value in each of these indices by 1
    y[indices] <- y[indices] + 1
    ## Move the decimal separator back in place
    y/up
}
round_sum(c(0.333, 0.333, 0.334), 2)
round(c(0.336, 0.336, 0.335), 2)
sum(c(0.336, 0.336, 0.335))
round_sum(c(0.336, 0.336, 0.335), 2)
sum(round(c(0.336, 0.336, 0.335), 2))
sum(round_sum(c(0.336, 0.336, 0.335), 2))

* geometric/harmonic means
See http://economistatlarge.com/r-guide/arithmetic-harmonic-geometric-means-r

mean.default <- function(x, trim = 0, na.rm = FALSE, type = c("arithmetic", "geometric", "harmonic"), ...) {
    type <- match.arg(type)
    if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(NA_real_)
    }
    if (na.rm) 
        x <- x[!is.na(x)]
    if (!is.numeric(trim) || length(trim) != 1L) 
        stop("'trim' must be numeric of length one")
    n <- length(x)
    if (trim > 0 && n) {
        if (is.complex(x)) 
            stop("trimmed means are not defined for complex data")
        if (anyNA(x)) 
            return(NA_real_)
        if (trim >= 0.5) 
            return(stats::median(x, na.rm = FALSE))
        lo <- floor(n * trim) + 1
        hi <- n + 1 - lo
        x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
    }
    if (type == "arithmetic") {
        .Internal(mean(x))
    } else if (type == "geometric") {
        prod(x)^(1/n)
    } else if (type == "harmonic") {
        n/sum(1/x)
    }
}
## See also:
geom_mean <- function(x, na.rm = TRUE, zero.propagate = FALSE) {
    if (any(x < 0, na.rm = TRUE)) {
        return(NaN)
    }
    if (zero.propagate) {
        if (any(x == 0, na.rm = TRUE)) {
            return(0)
        }
        exp(mean(log(x), na.rm = na.rm))
    } else {
        exp(sum(log(x[x > 0]), na.rm = na.rm)/length(x))
    }
}

* Scale a matrix globally (not by column)

(note: cannot be scale.matrix because scale.default does not have 'bycol' argument

scale.mat <- function(mat, center = TRUE, scale = TRUE, bycol = FALSE)
{
    if (bycol) {
        scale(mat, center = center, scale = scale)
    } else {
        matrix(scale(as.vector(mat), center = center, scale = scale),
            nrow = nrow(mat), ncol = ncol(mat))
    }
}
## (mm <- matrix(1:20, nrow = 4))
## scale.mat(mm)
## scale.mat(mm, bycol = TRUE)
