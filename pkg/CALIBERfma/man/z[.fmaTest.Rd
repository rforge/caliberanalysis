\name{[.fmaTest}
\alias{[.fmaTest}
\alias{[[.fmaTest}

\title{
Extract a subset of a fmaTest object
}

\description{
Returns the selected element(s) of a fmaTest object. Brackets can be used subsetting, e.g. \code{x[y]}
or \code{x[[y]]}.
}

\usage{
\method{[}{fmaTest}(x, y)
\method{[[}{fmaTest}(x, y)
}

\arguments{
  \item{x}{
a \code{\link{fmaTest}} object, storing the results of an analysis by the Freetext Matching Algorithm
}
  \item{y}{
an integer or integer range of fmaTest elements to extract
}
}


\value{
A fmaTest object containing only the selected elements, or if it is a single element, a fmaTestElement object.
}

\author{
Anoop Shah
}

\seealso{
\code{\link{fmaTest}}, \code{link{print.fmaTestElement}}
}
