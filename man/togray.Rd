\name{togray}
\alias{togray}
\alias{togrey}
\title{Convert continuous variable to grey levels}
\usage{
togray(x, min = 0.1, max = 0.9, alpha = NULL, inverse = FALSE,
  sqrt = FALSE)

togrey(x, min = 0.1, max = 0.9, alpha = NULL, inverse = FALSE,
  sqrt = FALSE)
}
\arguments{
  \item{x}{A numeric vector.}

  \item{min}{The minimum grey level.}

  \item{max}{The maximum grey level.}

  \item{alpha}{The opacity.}

  \item{inverse}{Logical. By default, bigger is darker. If
  \code{inverse = TRUE}, bigger is lighter.}

  \item{sqrt}{Logical. Applies a square root transformation
  to get more progressive grey levels.}
}
\value{
A vector of colors of the same length as \code{x}.
}
\description{
Convert a continuous variable to the corresponding levels
of grey.
}
\examples{
bla <- runif(10000)
plot(bla, col = togray(bla, 0, 1), pch = 20)
plot(bla, col = togray(bla, 0, 1, sqrt = TRUE), pch = 20)
plot(bla, col = togray(bla, 0, 1, alpha = 0.5), pch = 20)
}
\author{
From Clement Calenge, modified by Mathieu Basille
\email{basille@ase-research.org}
}

