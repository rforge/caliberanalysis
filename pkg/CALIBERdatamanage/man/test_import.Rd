\name{test_import}
\alias{test_import}
\docType{data}

\title{
Test dataset for import functions
}

\description{
A sample dataset with different column types, to test the functions \code{\link{importDT}} and \code{\link{importFFDF}}.
}

\usage{data(test_import)}
\format{
  A data frame with 6 observations on the following 10 variables.
  \describe{
    \item{\code{logical}}{a logical vector}
    \item{\code{bitnum}}{a numeric vector}
    \item{\code{bitlogic}}{a logical vector}
    \item{\code{intnotbit}}{a numeric vector}
    \item{\code{integer}}{a numeric vector}
    \item{\code{intnot}}{a numeric vector}
    \item{\code{dateok}}{a factor with levels \code{2003-01-01} \code{2003-03-04} \code{2004-02-01} \code{2009-02-03}}
    \item{\code{datemiss}}{a factor with levels \code{} \code{0000-00-00} \code{2003-01-01} \code{2003-03-04} \code{2009-02-03}}
    \item{\code{datenotok}}{a factor with levels \code{} \code{2003-01-01} \code{2003-03-04} \code{2004-02-01} \code{2009-02-03} \code{error}}
    \item{\code{character}}{a factor with levels \code{another} \code{other} \code{that} \code{thing} \code{this}}
  }
}


\examples{
data(test_import)
tempfile <- paste(tempdir(), 'test_import.csv', sep='')
write.csv(test_import, tempfile, row.names = FALSE)

a <- importDT(tempfile)
str(a)
print(a)
unlink(tempfile)
}
\keyword{datasets}
