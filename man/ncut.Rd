% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ncut.r
\name{ncut}
\alias{ncut}
\title{Cut into classes of equal size}
\usage{
ncut(x, k = 10, labels = FALSE)
}
\arguments{
\item{x}{numeric vector to cut.}

\item{k}{the number of classes.}

\item{labels}{Labels for the levels of the resulting category. By
default (\code{FALSE}), labels are constructed using simple
integer codes; if \code{NULL}, labels are build using
\code{"(a,b]"} interval notation. Alternatively, a custom
vector of length \code{k} can be used.}
}
\value{
A vector of k classes.
}
\description{
Cut a numeric vector into k classes of (roughly) equal size.
}
\examples{
bla <- rnorm(100)
summary(bla)
bli <- ncut(bla)
table(bli)
head(ncut(bla, labels = NULL))
head(ncut(bla, labels = LETTERS[1:10]))
}
\author{
Mathieu Basille \email{basille@ufl.edu} and Emiel van Loon
}
