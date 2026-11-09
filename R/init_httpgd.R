#' Init a web graphic device from httpgd
#'
#' Init a web graphic device from httpgd.
#'
#' @param width Initial plot width (pixels); defaults to
#'     \code{getOption("httpgd.width")} or 720 pixels if its not available.
#' @param height Initial plot height (pixels); defaults to
#'     \code{getOption("httpgd.height")} or 720 pixels if its not available..
#' @param asp If provided, forces height to be equal to \code{width*asp}.
#' @note Requires package \pkg{httpgd}.
#' @author Mathieu Basille <\email{mathieu@@basille.org}>
#' @export
init_httpgd <- function(width = getOption("httpgd.width", 720),
  height = getOption("httpgd.height", 576), asp = NA) {
    ## `httpgd` is required
    if (!requireNamespace("httpgd", quietly = TRUE)) {
        stop(
            "Package \"httpgd\" must be installed to use this function.",
            call. = FALSE
        )
    }
    if (!is.na(asp))
        height <- asp * width
    httpgd::hgd(width = width, height = height)
    httpgd::hgd_browse()
}
