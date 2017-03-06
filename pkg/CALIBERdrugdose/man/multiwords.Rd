\name{multiwords}
\alias{multiwords}
\docType{data}
\title{
Multiwords Lookup Table for Simplifying a Dosage Text
}

\description{
Lookup table for converting 
}

\usage{data(multiwords)}
\format{
  A data frame with observations on the following 4 variables.
  \describe{
    \item{\code{words}}{character; regular expression to match dosage
    text in a standardised format (one space between words,
    no capitals)}
    \item{\code{replacement}}{character; replacement text}
    \item{\code{comments}}{character; comments}
    \item{\code{order}}{numeric; order in which the rules are applied}
  }
}

\details{
The rules are applied in order, and each subsequent rule acts on text which has
been modified by previous rules.
}

\references{
Shah AD, Martinez C. An algorithm to derive a numerical daily dose
from unstructured text dosage instructions.
Pharmacoepidemiol Drug Saf 2006; 15(3): 161-166. doi: 10.1002/pds.1151
\url{http://onlinelibrary.wiley.com/doi/10.1002/pds.1151/}
}

\examples{
data("multiwords")

# Convert to multiwords dictionary for analysis
multiwords <- as.drugdose_multiwords(multiwords)
}
\keyword{datasets}
