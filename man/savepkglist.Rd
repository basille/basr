\name{savepkglist}
\alias{loadpkglist}
\alias{savepkglist}
\title{Load or save the list of attached packages}
\usage{
savepkglist(file = ".Rpackages")

loadpkglist(file = ".Rpackages")
}
\arguments{
  \item{file}{The name of the file in which to save the
  list of attached packages, or from which to load it. The
  path is relative to the current working directory.}
}
\description{
Load or save the list of attached packages.
}
\details{
\code{savepkglist} saves the list of all attached packages,
except base packages, in a file, with one package per line.

\code{loadpkglist} loads a list of packages from a file.
The file should contain one package name per line, without
quotes, and no empty line. If the packages are not
installed, the function sends a warning.
}
\note{
To automatically load a \code{.Rpackages} list at the start
of R, add this in your \code{.Rprofile}:

\code{### Silently load 'basr' together with default
packages}

\code{options(defaultPackages =
c(getOption("defaultPackages"), "basr"))}

\code{### Load packages at the start of R if the package
list exists}

\code{if (file.exists(".Rpackages"))}

\code{ basr::loadpkglist(".Rpackages")}

See \code{\link[base]{Startup}} for more details about the
initialization at start of an R session.
}
\examples{
\dontrun{savepkglist(file = "list.Rpackages")}
\dontrun{loadpkglist()}
}
\author{
Mathieu Basille \email{basille@ase-research.org}
}

