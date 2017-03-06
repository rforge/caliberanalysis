\name{singlewords}
\alias{singlewords}
\docType{data}
\title{
Lookup Table for Replacing Individual Words
}

\description{
A table of replacements for words that may be found in dosage texts.
This enables different forms of the same word (e.g. tab, tablet, tabs)
to be converted to a standardised form, to facilitate downstream
processing. The table is converted to a named character vector
by \code{\link{as.drugdose_singlewords}} for use in analysis.
}

\usage{data(singlewords)}
\format{
  A data frame with observations on the following 2 variables.
  \describe{
    \item{\code{replacement}}{character; replacement for \code{words}.
    If missing (NA), the word is left as it is (equivalent to setting
    \code{replacement} equal to \code{words} .}
    \item{\code{words}}{character; a single word}
  }
}

\details{
Each entry in the 'words' column must be unique and must be a single word or punctuation character.
}


\references{
Shah AD, Martinez C. An algorithm to derive a numerical daily dose
from unstructured text dosage instructions.
Pharmacoepidemiol Drug Saf 2006; 15(3): 161-166. doi: 10.1002/pds.1151
\url{http://onlinelibrary.wiley.com/doi/10.1002/pds.1151/}
}

\examples{
data("singlewords")

# Convert to singlewords dictionary for analysis
singlewords <- as.drugdose_singlewords(singlewords)
}

\keyword{datasets}
