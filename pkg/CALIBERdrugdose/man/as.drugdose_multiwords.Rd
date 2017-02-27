\name{as.drugdose_multiwords}
\alias{as.drugdose_multiwords}
\alias{drugdose_multiwords}

\title{
Import or convert a drugdose_multiwords table
}

\description{
Converts its argument into an object of the class
drugdose_multiwords for use by the dose conversion algorithm.
}

\usage{
as.drugdose_multiwords(multiwords)
}

\arguments{
  \item{multiwords}{
Either a drugdose_multiwords object (in which case it is returned unchanged),
a data.frame, data.table or filename of a csv file in the appropriate format,
see 'Details' below.
}
}

\details{
The data.frame, data.table or file should have the following columns:
  \describe{
    \item{\code{words}}{character; regular expression to match dosage
    text in a standardised format (one space between words,
    no capitals)}
    \item{\code{replacement}}{character; replacement text}
    \item{\code{comments}}{character; comments}
    \item{\code{order}}{numeric; order in which the rules are applied}
  }
}


\value{
A data.frame with class \code{drugdose_multiwords}.
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
data(multiwords)

# Convert to multiwords dictionary for analysis
multiwords <- as.drugdose_multiwords(multiwords)
}
