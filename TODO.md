## Geometric/harmonic means

- See: http://economistatlarge.com/r-guide/arithmetic-harmonic-geometric-means-r

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

- See also:

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

## Scale a matrix globally (not by column)

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

(mm <- matrix(1:20, nrow = 4))
scale.mat(mm)
scale.mat(mm, bycol = TRUE)
