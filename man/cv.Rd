% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv.r
\name{cv}
\alias{cv}
\title{Coefficient of variation}
\usage{
cv(x, na.rm = FALSE, ci = FALSE, conf.level = 0.95,
  method = c("mckaymod", "mckay", "naive"))
}
\arguments{
\item{x}{A numeric vector}

\item{na.rm}{Logical.  Should missing values be removed?}

\item{ci}{Logical. Should confidence intervals be computed?}

\item{conf.level}{Confidence level of the interval.}

\item{method}{The method to compute the confidence
intervale. Either the naive (\code{naive}), the McKay
(\code{mckay}) or the modified McKay (\code{mckaymod}, default)
approximation.}
}
\value{
If \code{ci}, returns a list with the coefficient of
variation. in the first element and the confidence interval in the
second.
}
\description{
This function computes the coefficient of variation (i.e. \code{sd
/ mean}) of the values in \code{x}.  If \code{ci} is \code{TRUE}
then confidence intervals are also computed.
}
\section{Original URL}{

\url{http://tolstoy.newcastle.edu.au/R/e2/help/07/06/19043.html}
}

\examples{
xx <- 1:10
cv(xx)
sd(xx)/mean(xx)
cv(xx, ci = TRUE)
}
\references{
Vangel, M. G. (1996) Confidence intervals for a normal
coefficient of variation. The American Statistician, 50: 21-26
}
\author{
From Kevin Wright, modified by Mathieu Basille
\email{basille@ufl.edu}
}
