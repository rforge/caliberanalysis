\name{lookups}
\alias{lookups}
\docType{data}
\title{
Lookups table
}
\description{
A lookup table of pre-analysed dosage texts to speed up the processing of new texts. Texts which are difficult to interpret or which cause errors can also be added to this table.
}
\usage{data(lookups)}
\format{
  A data frame with 3 observations on the following 12 variables.
  \describe{
    \item{\code{words}}{character; the dosage text}
    \item{\code{qty}}{numeric; quantity per dose}
    \item{\code{units}}{character; dose units}
    \item{\code{freq}}{numeric; }
    \item{\code{tot}}{numeric; }
    \item{\code{max}}{a logical vector}
    \item{\code{time}}{numeric; time interval in number of days}
    \item{\code{change}}{a logical vector}
    \item{\code{choice}}{a logical vector}
    \item{\code{duration}}{a logical vector}
    \item{\code{daily_dose}}{numeric; dose units per day}
  }
}

\details{
Only a single row is permitted per dosage text.
Users can specify a 'customlookups' table in the same format to provide additional 
}

\references{
Shah AD, Martinez C. An algorithm to derive a numerical daily dose from unstructured text dosage instructions.
Pharmacoepidemiology and Drug Safety 2006; 15(3): 161–166. doi: 10.1002/pds.1151
\url{http://onlinelibrary.wiley.com/doi/10.1002/pds.1151/}
}

\examples{
# Import the raw table
data(lookups)

# Convert into the lookup table format
as.drugdose_lookups(lookups)
}
\keyword{datasets}
