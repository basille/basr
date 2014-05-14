\name{extrange}
\alias{extrange}
\title{Extended range}
\usage{
extrange(x, percent = 0.1, na.rm = FALSE)
}
\arguments{
  \item{x}{A numeric vector.}

  \item{percent}{The proportion to be added to the range.}

  \item{na.rm}{Logical, indicating if \code{NA}'s should be
  omitted.}
}
\description{
Returns the range extended by a given proportion.
}
\details{
If the regular range returns a single value, the proportion
is computed on this value itself (and not on the range).
}
\examples{
extrange(0:10)
extrange(0:10, percent = .5)
extrange(-10:10)
extrange(rep(10, 3))
}
\author{
Mathieu Basille \email{basille@ase-research.org}
}

