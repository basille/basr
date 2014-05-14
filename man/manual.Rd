\name{manual}
\alias{manual}
\title{Generate package reference manual}
\usage{
manual(pkg = ".", path = NULL, preview = TRUE, overwrite = FALSE)
}
\arguments{
  \item{pkg}{package description, can be path or package
  name.  See \code{\link[devtools]{as.package}} for more
  information}

  \item{path}{path in which to produce package.  If
  \code{NULL}, defaults to the root directory of the
  package.}

  \item{preview}{preview generated PDF file}

  \item{overwrite}{overwrite output file if it exists}
}
\description{
Generate package reference manual. This function requires
the \code{devtools} package.
}
\author{
Mathieu Basille \email{basille@ase-research.org}
}

