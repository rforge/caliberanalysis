\name{test_data}
\alias{test_data}
\docType{data}
\title{
Sample CPRD GOLD format data
}
\description{
A sample section of CPRD GOLD format data, for testing the \code{\link{extractEntity}} function.
}

\usage{data(test_data)}
\format{
  A data frame with 15 observations on the following 10 variables.
  \describe{
    \item{\code{anonpatid}}{a numeric vector (patient ID)}
    \item{\code{medcode}}{a numeric vector (CPRD Read term identifier)}
    \item{\code{enttype}}{a numeric vector (entity type)}
    \item{\code{eventdate}}{a factor with levels \code{1998-08-09} \code{1999-05-06} \code{1999-12-12} \code{2001-01-02} \code{2003-07-12} \code{2011-02-01} \code{2013-01-01}}
    \item{\code{data1}}{a numeric vector}
    \item{\code{data2}}{a numeric vector}
    \item{\code{data3}}{a numeric vector}
    \item{\code{data4}}{a numeric vector}
    \item{\code{data5}}{a numeric vector}
    \item{\code{data6}}{a numeric vector}
    \item{\code{data7}}{a numeric vector}
  }
}


\examples{
data(test_data)
}

\keyword{datasets}
