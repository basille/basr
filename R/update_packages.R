#' Update user packages (CRAN and GitHub)
#'
#' Checks the list of user-installed packages (on CRAN, BioConductor or GitHub)
#' and updates them where required.
#'
#' @note Requires package \pkg{pak}.
#' @author Mathieu Basille <\email{mathieu@@basille.org}>
#' @export
#' @returns (Invisibly) A data frame with information about the updated
#'     package(s).
#' @examples
##' \dontrun{
#' ## Simply update all user packages:
#' update_packages()}
#' @importFrom utils installed.packages packageDescription
update_packages <- function() {
    ## `pak` is required
    if (!requireNamespace("pak", quietly = TRUE)) {
        stop(
            "Package \"pak\" must be installed to use this function.",
            call. = FALSE
        )
    }
    ## All user installed packages
    pkg.list <- installed.packages(lib.loc = grep("home",
        .libPaths(), value = TRUE))[, "Package"]
    ## Their descriptions
    pkg.desc <- lapply(installed.packages(lib.loc = grep("home",
        .libPaths(), value = TRUE))[, "Package"], function(ll) {
        packageDescription(ll)
    })
    ## Extraction of package full names, accounting for GitHub
    pkg.repo <- data.frame(
        package = unlist(lapply(pkg.desc, function(ll) ll$Package)),
        repository = unlist(lapply(pkg.desc, function(ll)
            if (is.null(ll$Repository)) {
                NA
            } else ll$Repository)),
        source_on_github = unlist(lapply(pkg.desc, function(ll)
            if (is.null(ll$RemoteType)) {
                NA
            } else ll$RemoteType)),
        github_username = unlist(lapply(pkg.desc, function(ll)
            if (is.null(ll$GithubUsername)) {
                NA
            } else ll$GithubUsername)),
        github_pkgname = unlist(lapply(pkg.desc, function(ll)
            if (is.null(ll$GithubRepo)) {
                NA
            } else ll$GithubRepo)),
        github_repo = unlist(lapply(pkg.desc, function(ll)
            if (is.null(ll$RemotePkgRef)) {
                NA
            } else ll$RemotePkgRef)))
    pkg.repo$repository <- ifelse(is.na(pkg.repo$repository) &
        pkg.repo$source_on_github == "github", "GitHub", pkg.repo$repository)
    pkg.repo$full_name <- ifelse(pkg.repo$repository == "GitHub",
        paste(pkg.repo$github_username, pkg.repo$github_pkgname, sep = "/"),
        pkg.repo$package)
    pkg.repo$full_name <- ifelse(is.na(pkg.repo$full_name), pkg.repo$package,
        pkg.repo$full_name)
    ## Update packages through 'pak'
    pak::pkg_install(pkg.repo$full_name)
}
