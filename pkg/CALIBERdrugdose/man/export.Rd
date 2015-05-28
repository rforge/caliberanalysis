\name{export}
\alias{export.drugdose_singlewords}
\alias{export.drugdose_multiwords}
\alias{export.drugdose_patterns}
\alias{export.drugdose_lookups}

\title{
Export dictionary lookups
}

\description{
Exports the singlewords, multiwords, patterns and lookups dictionaries to
.csv files.
}

\usage{
export(x, \dots)
\method{export}{drugdose_singlewords}(x, filename)
\method{export}{drugdose_multiwords}(x, filename)
\method{export}{drugdose_patterns}(x, filename)
\method{export}{drugdose_lookups}(x, filename)
}

\arguments{
  \item{x}{
An R object of class \code{\link{drugdose_singlewords}},
\code{\link{drugdose_multiwords}},
\code{\link{drugdose_patterns}} or \code{\link{drugdose_lookups}}.
}
  \item{filename}{
Name of .csv file to export to.
}
  \item{\dots}{
Generic arguments for \code{export} method. 
}
}

\details{
Exports the dictionary as a comma separated values text file.
}


\references{
Shah AD, Martinez C. An algorithm to derive a numerical daily dose
from unstructured text dosage instructions.
Pharmacoepidemiol Drug Saf 2006; 15(3): 161–166. doi: 10.1002/pds.1151
\url{http://onlinelibrary.wiley.com/doi/10.1002/pds.1151/}
}

\author{
Anoop Shah
}


\seealso{
\code{\link{importSinglewords}}, \code{\link{importMultiwords}},
\code{\link{importPatterns}},    \code{\link{importLookups}}

\code{\link{addterm}}
}

\examples{
}
