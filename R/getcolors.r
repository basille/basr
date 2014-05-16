## getcolors
##
##' Allows for the selection of \code{n} colors by using a simplified color swatch.
##'
##' \code{getcolors} allows selection with a mouse using the
##' \code{locator} function. Following selection, a second plot opens
##' showing how these colors look next to each other and on a
##' background gradient of black to white. The function uses an RGB
##' color model: Red increases on the y-axis, Green increases on the
##' x-axis, and Blue is a repeated sequence of levels across the
##' x-axis.
##' @title Choosing colors visually
##' @param n The number of colors to choose
##' @return A character vector with elements of 7 or 9 characters,
##' \code{"#"} followed by the red, blue, green and optionally alpha
##' values in hexadecimal (after rescaling to \code{0 ... 255}).  The
##' optional alpha values range from \code{0} (fully transparent) to
##' \code{255} (opaque).
##' @author Marc in the box
##' @section Original URL:
##' \url{http://menugget.blogspot.com/2013/01/choosing-colors-visually-with-getcolors.html}
##' @export
##' @examples
##' \dontrun{
##' set.seed(1)
##' n <- 100
##' x <- seq(n)
##' y1 <- cumsum(rnorm(n))
##' y2 <- cumsum(rnorm(n))
##' y3 <- cumsum(rnorm(n))
##' y4 <- cumsum(rnorm(n))
##' ylim <- range(c(y1, y2, y3, y4))
##'
##' cols <- getcolors(4)
##'
##' plot(x, y1, ylim = ylim, t = "l", col = cols[1], lwd = 3, ylab = "")
##' lines(x, y2, col = cols[2], lwd = 3)
##' lines(x, y3, col = cols[3], lwd = 3)
##' lines(x, y4, col = cols[4], lwd = 3)
##' legend("topleft", legend = paste("y", 1:4, sep = ""), col = cols,
##'     lwd = 3)}
getcolors <- function(n)
{
    N <- 6
    X <- seq(N^2)
    Y <- seq(N)
    GRD <- expand.grid(x = X, y = Y)
    Z <- matrix(0, nrow = length(X), ncol = length(Y))
    LEV <- seq(0, 1, , N)
    R <- matrix(rep(LEV, each = N^2), nrow = length(X), ncol = length(Y))
    G <- matrix(rep(rep(LEV, each = N), N), nrow = length(X),
        ncol = length(Y))
    B <- matrix(rep(LEV, N^2), nrow = length(X), ncol = length(Y))
    x11(width = 6, height = 6)
    layout(matrix(1:2, nrow = 2, ncol = 1), widths = c(6), heights = c(1.5,
        4.5))
    op <- par(mar = c(1, 3, 2, 1))
    image(X, Y, Z, col = NA, xlab = "", ylab = "", xaxt = "n",
        yaxt = "n")
    for (i in seq(nrow(GRD))) {
        xs <- c(GRD$x[i] - 0.5, GRD$x[i] - 0.5, GRD$x[i] + 0.5,
            GRD$x[i] + 0.5)
        ys <- c(GRD$y[i] - 0.5, GRD$y[i] + 0.5, GRD$y[i] + 0.5,
            GRD$y[i] - 0.5)
        polygon(xs, ys, col = rgb(R[i], G[i], B[i]), border = NA)
    }
    mtext(paste("Click on", n, "colors [please]"), side = 3,
        line = 0.5)
    box()
    COLS <- NA * seq(n)
    for (i in seq(n)) {
        coord <- locator(1)
        COLS[i] <- rgb(R[round(coord$x), round(coord$y)], G[round(coord$x),
            round(coord$y)], B[round(coord$x), round(coord$y)])
    }
    par(mar = c(1, 3, 0, 1))
    pal <- colorRampPalette(c("black", "white"))
    image(x = 1:100, y = seq(n), z = matrix(rep(1:100, n), nrow = 100,
        ncol = n), col = pal(100), xlab = "", ylab = "", xaxt = "n",
        yaxt = "n")
    box()
    for (i in seq(n)) {
        lines(x = c(1, 100), y = c(i, i), col = COLS[i], lwd = 4)
    }
    axis(2, at = seq(n))
    par(op)
    COLS
}
