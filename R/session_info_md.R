#' sessionInfo() adapted for Markdown output
#'
#' Displays session information adapted for Markdown output.
#'
#' The function can be used with something like this in a RMarkdown file:
#'
#' \preformatted{```{r r-session, echo = FALSE, results = "asis", cache = FALSE}
#' session_info_md(language = "FR", unnumbered = TRUE)
#' ```}
#'
#' @param language The output language; currently supports English ("EN",
#'     default) and French ("FR").
#' @param header_level The header level to use for (sub)sections of the output.
#' @param unnumbered Logical; should the (sub)sections be numbered or not?
#'     Defaults to \code{"FALSE"}.
#' @param sort_attached Logical; should attached packages be listed by
#'     alphabetical order? Defaults to \code{"TRUE"}.
#' @author Mathieu Basille <\email{mathieu@@basille.org}>
#' @export
session_info_md <- function(language = c("EN", "FR"), header_level = 2,
    unnumbered = FALSE, sort_attached =  TRUE)
{
    ## `sessioninfo` is required
    if (!requireNamespace("sessioninfo", quietly = TRUE)) {
        stop(
            "Package \"sessioninfo\" must be installed to use this function.",
            call. = FALSE
        )
    }
    ## `knitr` is required
    if (!requireNamespace("knitr", quietly = TRUE)) {
        stop(
            "Package \"knitr\" must be installed to use this function.",
            call. = FALSE
        )
    }
    if (!(header_level %in% 1:6))
        stop("`header_level` must be an integer between 1 and 6")
    header_main <- paste(rep("#", header_level), collapse = "")
    header_second <- paste(rep("#", header_level + 1), collapse = "")
    header_numb <- ifelse(unnumbered, " {-}", "")
    language <- match.arg(language)
    switch(language,
        "EN" = {
            session_header <- "R session information"
            platform_header <- "Platform"
            packages_header <- "Packages"
            library_header <- "Library paths"
            setting_name <- "Setting"
            value_name <- "Value"
            package_name <- "Package"
            attached_name <- "Attached"
            version_name <- "Version"
            date_name <- "Date"
            lib_name <- "lib"
            source_name <- "Source"
        },
        "FR" = {
            session_header <- "Informations de session R"
            platform_header <- "Plateforme"
            packages_header <- "Packages"
            library_header <- "Chemin des librairies"
            setting_name <- "Param\u00E8tre"
            value_name <- "Valeur"
            package_name <- "Package"
            attached_name <- "Attach\u00E9"
            version_name <- "Version"
            date_name <- "Date"
            lib_name <- "lib"
            source_name <- "Source"
        })
    cat(paste0(header_main, " ", session_header, header_numb, "\n\n"))
    sess <-sessioninfo::session_info()
    cat(paste0(header_second, " ", platform_header, header_numb))
    platform <- data.frame(setting = names(sess$platform ),
        value = unlist(sess$platform))
    print(knitr::kable(platform, row.names = FALSE,
        col.names = c(setting_name, value_name)))
    cat(paste0("\n", header_second, " ", packages_header, header_numb))
    flib <- function(x) ifelse(is.na(x), "?", as.integer(x))
    packages <- data.frame(package = sess$packages$package,
        attached = ifelse(sess$packages$attached, "*", ""),
        version = ifelse(is.na(sess$packages$loadedversion),
            sess$packages$ondiskversion, sess$packages$loadedversion),
        `date (UTC)` = sess$packages$date,
        lib = paste0("[", flib(sess$packages$library), "]"),
        source = sub("([0-9a-f]{7})[0-9a-f]{33}", "\\1", sess$packages$source),
        stringsAsFactors = FALSE, check.names = FALSE)
    if (sort_attached)
        packages <- packages[order(packages$attached, packages$package,
            decreasing = c(TRUE, FALSE), method = "radix"), ]
    print(knitr::kable(packages, row.names = FALSE, align = "lcrrrr",
        col.names = c(package_name, attached_name, version_name,
            date_name, lib_name, source_name)))
    cat(paste0("\n**", library_header, "**\n\n"))
    cat(paste(sapply(seq_along(levels(sess$packages$library)), function(i) {
        paste0("* [", i, "] `", levels(sess$packages$library)[i], "`")
    }), collapse = "\n"))
    cat("\n")
}
