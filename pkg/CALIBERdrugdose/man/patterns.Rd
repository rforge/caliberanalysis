\name{patterns}
\alias{patterns}
\docType{data}
\title{
Patterns lookup table for extracting components of the dosage instruction
}

\description{
This lookup table is used in the final stage of standardising dosage text
and extracting structured information. It is used after \code{\link{multiwords}}
but instead of replacing a phrase, it extracts components of the dose.
The rules in this table are applied one after the other in ascending order of
the \code{order} column.
}

\usage{data(patterns)}
\format{
  A data frame with observations on the following 15 variables.
  \describe{
    \item{\code{words}}{character; pattern phrase}
    \item{\code{qty}}{character; dose quantity or regular expression
    component for extracting quantity from the matched text.}
    \item{\code{units}}{a character vector}
    \item{\code{freq}}{a character vector}
    \item{\code{tot}}{a character vector}
    \item{\code{priority}}{a numeric vector}
    \item{\code{max}}{a character vector}
    \item{\code{time}}{a character vector}
    \item{\code{change}}{a logical vector}
    \item{\code{choice}}{a character vector}
    \item{\code{doubledose}}{a character vector}
    \item{\code{duration}}{a character vector}
    \item{\code{comment}}{a character vector}
    \item{\code{category}}{a character vector}
    \item{\code{order}}{a numeric vector}
  }
}

\details{
The structured elements can be extracted using regular expression symbols, e.g. \\1
}

\references{
Shah AD, Martinez C. An algorithm to derive a numerical daily dose
from unstructured text dosage instructions.
Pharmacoepidemiol Drug Saf 2006; 15(3): 161-166. doi: 10.1002/pds.1151
\url{http://onlinelibrary.wiley.com/doi/10.1002/pds.1151/}
}

\examples{
data(patterns)

# Convert to patterns dictionary for analysis
patterns <- as.drugdose_patterns(patterns)
}
\keyword{datasets}
