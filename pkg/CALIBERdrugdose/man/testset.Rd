\name{testset}
\docType{data}
\alias{testset}

\title{
Testset: Gold Standard Interpreted Drug Dosages for Testing
}

\description{
A table of dosage texts with gold standard interpretation, used for testing
the algorithm.
}
\usage{data(testset)}
\format{
  A data frame with the following 12 variables.
  \describe{
    \item{\code{textid}}{numeric; text identifier}
    \item{\code{order}}{numeric; order of dose line (a single dosage text
    can generate more than one dose line)}
    \item{\code{text}}{character; the dosage text}
    \item{\code{qty}}{numeric; quantity per dose}
    \item{\code{units}}{character; dose units}
    \item{\code{freq}}{numeric; dose frequency}
    \item{\code{tot}}{numeric; total dose per time period}
    \item{\code{max}}{factor with 3 levels: max, average, exact}
    \item{\code{time}}{numeric; time interval in number of days}
    \item{\code{change}}{factor with 4 levels: first, second, nochange, combined}
    \item{\code{choice}}{factor with 3 levels: choice, asneeded, nochoice}
    \item{\code{duration}}{numeric; duration of prescription}
    \item{\code{daily_dose}}{numeric; dose units per day}
  }
}

\references{
Shah AD, Martinez C. An algorithm to derive a numerical daily dose
from unstructured text dosage instructions.
Pharmacoepidemiol Drug Saf 2006; 15(3): 161-166. doi: 10.1002/pds.1151
\url{http://onlinelibrary.wiley.com/doi/10.1002/pds.1151/}
}

\seealso{
\code{\link{testdoseconvert}}, \code{\link{lookups}}
}

\examples{
# Import the testset table
data("testset")

# Analyse the testset
results <- testdoseconvert(goldstandard = testset)

# Check there are no incorrect results
if (any(results$outcome == 'actual')){
    print(results[outcome == 'actual' | outcome == 'intended'])
    stop('\nError in analysis of test set.\n')
}

}
\keyword{datasets}
