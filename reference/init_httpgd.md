# Init a web graphic device from httpgd

Init a web graphic device from httpgd.

## Usage

``` r
init_httpgd(
  width = getOption("httpgd.width", 720),
  height = getOption("httpgd.height", 576),
  asp = NA
)
```

## Arguments

- width:

  Initial plot width (pixels); defaults to `getOption("httpgd.width")`
  or 720 pixels if its not available.

- height:

  Initial plot height (pixels); defaults to `getOption("httpgd.height")`
  or 720 pixels if its not available..

- asp:

  If provided, forces height to be equal to `width*asp`.

## Note

Requires package httpgd.

## Author

Mathieu Basille \<\email{mathieu@basille.org}\>

## Examples

``` r
if (FALSE) { # \dontrun{
## Simply open a web graphic device and checks it in your browser:
init_httpgd()
plot(1:10)} # }
```
