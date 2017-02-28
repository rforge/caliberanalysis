\name{lookups}
\alias{lookups}
\docType{data}
\title{
Lookups Table
}
\description{
A lookup table of pre-analysed dosage texts to speed up the processing of new texts. Texts which are difficult to interpret or which cause errors can also be added to this table.
}
\usage{data(lookups)}
\format{
  A data frame with the following 12 variables.
  \describe{
	\item{\code{textid}}{numeric; lookup line identifier}
    \item{\code{text}}{character; the dosage text}
    \item{\code{qty}}{numeric; quantity per dose}
	\item{\code{units}}{character; dose units}
	\item{\code{freq}}{numeric; dose frequency per time period}
	\item{\code{tot}}{numeric; total dose per time period}
	\item{\code{max}}{character: 'max', 'average', or 'exact'}
	\item{\code{time}}{numeric; time interval in number of days}
	\item{\code{change}}{character: 'first', 'second', or 'nochange'.
	If doses for different time periods combined using simplify=TRUE,
	\code{change} states which dose contributes to the output.}
	\item{\code{choice}}{character; 'choice', 'asneeded', or 'nochoice'}
	\item{\code{duration}}{numeric; duration of prescription}
	\item{\code{daily_dose}}{numeric; calculated daily dose}
  }
}

\details{
Only a single row is permitted per dosage text.
Users can specify a 'customlookups' table in the same format to provide
alternative lookups. The lookups table is processed by
\code{\link{as.drugdose_lookups}}.
}

\references{
Shah AD, Martinez C. An algorithm to derive a numerical daily dose
from unstructured text dosage instructions.
Pharmacoepidemiol Drug Saf 2006; 15(3): 161-166. doi: 10.1002/pds.1151
\url{http://onlinelibrary.wiley.com/doi/10.1002/pds.1151/}
}

\seealso{
\code{\link{as.drugdose_singlewords}}, \code{\link{as.drugdose_multiwords}}
\code{\link{as.drugdose_patterns}}, \code{\link{as.drugdose_lookups}},
\code{\link{export}}, \code{\link{addterm}}
}

\examples{
# Import the raw table
data(lookups)

# Convert into the lookup table format
lookups <- as.drugdose_lookups(lookups)
}
\keyword{datasets}
