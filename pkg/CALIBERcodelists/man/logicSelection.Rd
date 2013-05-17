\name{logicSelection}
\alias{!.selection}
\alias{&.selection}
\alias{|.selection}

\title{
Boolean operators for the S3 selection class
}

\description{
Selection objects are physically stored as a logical (Boolean) vector as
long as the number of rows in \code{\link{CALIBER_DICT}}, stating whether each term
is included or excluded from the current selection. Selection objects can be
combined using Boolean operators, with the result returned as a selection object.
}

\usage{
\method{!}{selection}(x)
\method{&}{selection}(e1, e2)
\method{|}{selection}(e1, e2)
}

\arguments{
  \item{x}{
a selection object
}
  \item{e1}{
a selection object
}
  \item{e2}{
a selection object
}
}

\value{
x & y returns terms included in both x and y

x | y returns terms included in either x or y

!x returns the selection of terms not included in x
}

\seealso{
\code{\link{\%AND\%}}, \code{\link{\%OR\%}}, \code{\link{NOT}}
}

\examples{
setdictionary('icd10')
exposure <- termhas('exposure')
tb <- termhas('tuberculosis')
exposure & tb
exposure | tb
}
