\name{medcodeis}
\alias{medcodeis}

\title{
Select Read codes with particular medcode(s)
}

\description{
Selects Read codes with a particular medcode(s).
}

\usage{
medcodeis(medcodes = integer(0))
}


\arguments{
  \item{medcodes}{
A vector of medcodes to extract.
}
}

\value{
A \code{\link{selection}} object.
}

\seealso{
\code{\link{as.codelist}},  \code{\link{assigncat}}, \code{\link{dictis}}, \code{\link{explode}}, \code{\link{getdictionary}}, \code{\link{setdictionary}}, \code{\link{termhas}}

To convert codelists from one dictionary to another: \code{\link{convert}}
}

\examples{
## Selecting a particular medcode or list of medcodes
setdictionary('read')
print(medcodeis(1:10))
}
