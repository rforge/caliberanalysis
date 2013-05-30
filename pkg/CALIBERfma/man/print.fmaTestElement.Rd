\name{print.fmaTestElement}
\alias{print.fmaTestElement}

\title{
S3 method to print the result of one test of the Freetext Matching Algorithm
}


\description{
The S3 class \link{fmaTest} is a list containing free text and the results of analysis. Each item in the list is an fmaTestElement.
}

\usage{
\method{print}{fmaTestElement}(x, ...)
}

\arguments{
  \item{x}{
a fmaTestElement, obtained by subsetting a fmfaTest object using \link{[.fmaTest} or \link{[[.fmaTest}
}
  \item{\dots}{other arguments to pass to print.data.frame to specify how the structured output should be displayed.}
}

\value{
Prints the fmaTestElement.
}


\author{
Anoop Shah
}

\seealso{
\code{\link{fmaTest}}
}

