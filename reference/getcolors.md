# Choosing colors visually

Allows for the selection of `n` colors by using a simplified color
swatch.

## Usage

``` r
getcolors(n)
```

## Arguments

- n:

  The number of colors to choose

## Value

A character vector with elements of 7 or 9 characters, `"#"` followed by
the red, blue, green and optionally alpha values in hexadecimal (after
rescaling to `0 ... 255`). The optional alpha values range from `0`
(fully transparent) to `255` (opaque).

## Details

`getcolors` allows selection with a mouse using the `locator` function.
Following selection, a second plot opens showing how these colors look
next to each other and on a background gradient of black to white. The
function uses an RGB color model: Red increases on the y-axis, Green
increases on the x-axis, and Blue is a repeated sequence of levels
across the x-axis.

## Original URL

[http://menugget.blogspot.com/2013/01/choosing-colors-visually-with-getcolors.html](http://menugget.blogspot.com/2013/01/choosing-colors-visually-with-getcolors.md)

## Author

Marc in the box

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(1)
n <- 100
x <- seq(n)
y1 <- cumsum(rnorm(n))
y2 <- cumsum(rnorm(n))
y3 <- cumsum(rnorm(n))
y4 <- cumsum(rnorm(n))
ylim <- range(c(y1, y2, y3, y4))

cols <- getcolors(4)

plot(x, y1, ylim = ylim, t = "l", col = cols[1], lwd = 3, ylab = "")
lines(x, y2, col = cols[2], lwd = 3)
lines(x, y3, col = cols[3], lwd = 3)
lines(x, y4, col = cols[4], lwd = 3)
legend("topleft", legend = paste("y", 1:4, sep = ""), col = cols,
    lwd = 3)} # }
```
