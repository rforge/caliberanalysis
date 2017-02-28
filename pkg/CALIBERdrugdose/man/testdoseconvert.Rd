\name{testdoseconvert}
\alias{testdoseconvert}

\title{
Test the Dose Conversion Program Against a Set of Texts With Gold Standard Interpretation
}

\description{
Calls \code{\link{doseconvert}} on each individual dose string, to
extract structured information such as quantity, frequency and units.
}


\usage{
testdoseconvert(text = goldstandard$text, textid = NULL,
    simplify = TRUE, singlewords = NULL, multiwords = NULL,
    patterns = NULL, maxifchoice = TRUE, usebuiltinlookups = FALSE,
    customlookups = NULL, cores = 1,
    noisy = ifelse(length(text) == 1, TRUE, FALSE),
    goldstandard = NULL)
}

\arguments{
  \item{text}{
Dosage strings to be interpreted.
}
  \item{textid}{
Optional vector of text or numeric identifiers for the dosage strings.
}
  \item{simplify}{
FALSE if multiple periods with different doses should be returned as separate
doses; TRUE if they should be combined into a single dose (default),
thus returning one row per original dosage text.
}
\item{singlewords}{
A \code{\link{drugdose_singlewords}} object or file path to the singlewords
dictionary. If NULL it is loaded from the package data directory.
}
  \item{multiwords}{
A \code{\link{drugdose_multiwords}} object or file path to the multiwords
dictionary. If NULL it is loaded from the package data directory.
}
  \item{patterns}{
A \code{\link{drugdose_patterns}} object or file path to the patterns
dictionary. If NULL it is loaded from the package data directory.
}
  \item{maxifchoice}{
Whether to return the maximum dose if there is a choice of dose (e.g.
1 or 2 tablets daily). If FALSE, the average is returned. 
}
  \item{usebuiltinlookups}{
Whether to use the lookup table of exact dose strings and their conversion
included in the package.
}
  \item{customlookups}{
A \code{\link{drugdose_lookups}} object or file path to the a custom
lookups table. This may consist of errors that you have found and corrected,
for which you would like the algorithm to use the correct interpretation
in the future.
}
  \item{cores}{
Number of cores to use, for multiprocessor machines running R on Linux.
The 'parallel' package must be loaded in order to use more than one core.
}
  \item{noisy}{
TRUE or FALSE - whether to display each text and the results of
interpretation to the console. By default, noisy = TRUE if only one
text is being analysed.
}
  \item{goldstandard}{
A data.frame or data.table containing textid and interpreted results (i.e. in the
same format as the output) against which the results can be compared. This should
have the following columns: words (optional), qty, units, freq, tot, max,
time, change, choice, duration, daily_dose
}
}

\details{
This function can be used to test the interpretation of a single text,
or test a number of texts against a gold standard interpretation.
}

\value{
Data.table with the following columns:

\item{\code{textid}}{text identifier}
\item{\code{order}}{if \code{simplify} = TRUE, this column is not included.
If \code{simplify} = FALSE, there may be multiple result lines per text
entry, and this column states the order of the result lines.}
\item{\code{outcome}}{outcome of comparison with gold standard; factor with 3
levels: 'actual', 'intended' or 'correct'. If there is no gold standard,
all rows have the outcome 'actual'. If there is a goldstandard and the
interpretation is correct, the outcome is 'correct', otherwise there are
2 or rows of output, one with outcome 'actual' and the other 'intended'.}
\item{\code{qty}}{numeric; dose quantity}
\item{\code{units}}{character; dose units}
\item{\code{freq}}{numeric; dose frequency per time period}
\item{\code{tot}}{numeric; total dose per time period}
\item{\code{max}}{factor with 3 levels: max, average, exact}
\item{\code{time}}{numeric; time interval in number of days}
\item{\code{change}}{factor with 4 levels: first, second, nochange, combined.
If doses for different time periods combined using \code{simplify} = TRUE,
\code{change} states which dose contributes to the output.}
\item{\code{choice}}{factor with 3 levels: choice, asneeded, nochoice}
\item{\code{duration}}{numeric; duration of prescription}
\item{\code{daily_dose}}{numeric; calculated daily dose}
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

\note{
Converted from the original Visual Basic algorithm.
Used as part of the testing process for this package.
}


\seealso{
\code{\link{interpret}}, \code{\link{doseconvert}}
}

\examples{
data(testset)

results <- as.data.table(testdoseconvert(
    testset$text, goldstandard = testset, noisy = FALSE))
if (all(results$outcome == 'correct')){
    cat('\nAll correct.')
} else {
    cat('\nErrors in analysis:\n')
    print(results[(outcome == 'intended' | outcome == 'actual')][
        order(textid, order, outcome)])
}
}
