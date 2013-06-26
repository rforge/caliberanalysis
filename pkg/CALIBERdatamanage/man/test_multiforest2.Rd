\name{test_multiforest2}
\alias{test_multiforest2}
\docType{data}

\title{
Test data for multiforest function
}

\description{
A slightly more complex test forest plot than \code{\link{test_multiforest}} for use with the 
\code{\link{multiforest}} function.
}

\usage{data(test_multiforest2)}

\format{
  A data frame with 9 observations on the following 9 variables.
  \describe{
    \item{\code{a1text}}{a factor with levels \code{< 60} \code{60-74} \code{75-90} \code{> 90} \code{Heart rate} \code{Sample graph} \code{Sex}}
    \item{\code{a1fontface}}{a factor with levels \code{bold} \code{italic}}
    \item{\code{a1just}}{a factor with levels \code{centre} \code{right}}
    \item{\code{a1numcols}}{a numeric vector}
    \item{\code{a1fontsize}}{a numeric vector}
    \item{\code{a2text}}{imported as a logical vector, should contain spaces}
    \item{\code{g1graphwidth}}{a numeric vector}
    \item{\code{g1mean}}{a numeric vector}
    \item{\code{g1se}}{a numeric vector}
  }
}


\examples{
data(test_multiforest2)
# Create spaces in column 2 (these were lost when the text file was
# loaded using 'data', but wil be retained if loaded directly by multiforest)
test_multiforest2$a2text <- "   "
multiforest(test_multiforest2)
}

\keyword{datasets}
