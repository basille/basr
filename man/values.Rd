\name{values}
\alias{values}
\title{Change the values of a vector.}
\usage{
values(x, from, to)
}
\arguments{
  \item{x}{A character or numeric vector.}

  \item{from}{A vector describing the values to change
  from.}

  \item{to}{A vector describing the values to change to.}
}
\value{
A vector.
}
\description{
Replaces given values of a vector by new values.
}
\examples{
(bla <- rep(1:5, 3))
values(bla, c(3, 4), c(7, 3))
values(bla, c(3, 4), c("a", "b"))
(bli <- rep(letters[1:5], 3))
values(bli, c("b", "d"), c(1, 2))
blu <- rpois(1e6, 10)
system.time(values(blu, c(3, 4), c(7, 3)))
}
\author{
Mathieu Basille \email{basille@ase-research.org}
}

