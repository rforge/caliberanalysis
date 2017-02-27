\name{as.drugdose_singlewords}
\alias{as.drugdose_singlewords}
\alias{drugdose_singlewords}

\title{
Import or convert a drugdose_singlewords table
}

\description{
Converts its argument into an object of the class
drugdose_singlewords for use by the dose conversion algorithm.
}

\usage{
as.drugdose_singlewords(singlewords)
}

\arguments{
  \item{singlewords}{
Either a drugdose_singlewords object (in which case it is returned unchanged),
a data.frame, data.table or filename of a csv file in the appropriate format,
see 'Details' below.
}
}

\details{
The data.frame, data.table or file should have the following columns:
  \describe{
    \item{\code{replacement}}{character; replacement for \code{words}.
    If missing (NA), the word is left as it is (equivalent to setting
    \code{replacement} equal to \code{words} .}
    \item{\code{words}}{character; a single word}
  }
}


\value{
A named character vector with class \code{drugdose_singlewords}.
}

\references{
Shah AD, Martinez C. An algorithm to derive a numerical daily dose
from unstructured text dosage instructions.
Pharmacoepidemiol Drug Saf 2006; 15(3): 161-166. doi: 10.1002/pds.1151
\url{http://onlinelibrary.wiley.com/doi/10.1002/pds.1151/}
}

\author{
Anoop Shah
}


\seealso{
\code{\link{as.drugdose_singlewords}}, \code{\link{as.drugdose_multiwords}}
\code{\link{as.drugdose_patterns}}, \code{\link{as.drugdose_lookups}},
\code{\link{export}}, \code{\link{addterm}}
}

\examples{
data(singlewords)

# Convert to singlewords dictionary for analysis
singlewords <- as.drugdose_singlewords(singlewords)
}
