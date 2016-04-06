\name{interpret}
\alias{interpret}

\title{
Convert a single dosage text into structured dosage information
}

\description{
Extracts structured information such as quantity, frequency and units
from a dosage text.
}

\usage{
interpret(instring, singlewords, multiwords, patterns,
    noisy = FALSE, simplify = FALSE, id = NULL)
}

\arguments{
  \item{instring}{
Dosage string to interpret.
}
  \item{singlewords}{
A \code{\link{drugdose_singlewords}} object.
}
  \item{multiwords}{
A \code{\link{drugdose_multiwords}} object.
}
  \item{patterns}{
A \code{\link{drugdose_patterns}} object.
}
  \item{noisy}{
Whether to print debug information to the console.
}
  \item{simplify}{
Whether multiple periods with different doses should be returned as separate
doses or combined into a single dose.
}
  \item{id}{
Unique identifier for the text being interpreted.
}
}

\value{
Data frame with the following columns:

\item{qty}{dose quantity (numeric)}
\item{units}{dose units (character)}
\item{freq}{dose frequency per time interval (numeric)}
\item{tot}{total dose per time period (numeric)}
\item{max}{factor with 3 levels: max, average, exact}
\item{time}{time period in days (numeric)}
\item{change}{factor with 3 levels: first, second, nochange.
If doses for different time periods are combined using simplify=TRUE,
\code{change} states which dose contributes to the output.}
\item{choice}{factor with 3 levels: nochoice, choice, asneeded}
\item{daily_dose}{interpreted daily dose (numeric)}

If \code{simplify} = TRUE, the row.names are the \code{ids} if supplied, or equal to the row numbers otherwise.
If \code{simplify} = FALSE, the row.names are \code{ids}.X where X (=1, 2, 3 etc.) is the order of the partial
dose if \code{ids} are supplied, or equal to the row numbers otherwise.
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
\code{\link{doseconvert}}
}

\examples{
data(singlewords)
data(multiwords)
data(patterns)
interpret('one daily for 1 week then two daily', id = 2, 
    singlewords = as.drugdose_singlewords(singlewords),
    multiwords = as.drugdose_multiwords(multiwords),
    patterns = as.drugdose_patterns(patterns),
    noisy = TRUE)
}
