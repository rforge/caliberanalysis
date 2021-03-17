\name{doseconvert}
\alias{doseconvert}

\title{
Convert a Set of Dosage Texts to Structured Dosage Information
}

\description{
Calls \code{\link{interpret}} on each individual dose string, to
extract structured information such as quantity, frequency and units.
}

\usage{
doseconvert(text, textid = seq_along(text), dosage_mg = NULL,
    simplify = TRUE, singlewords = NULL, multiwords = NULL,
    patterns = NULL, maxifchoice = TRUE, usebuiltinlookups = TRUE,
    customlookups = NULL, cores = 1, noisy = FALSE)
}

\arguments{
  \item{text}{
Dosage strings to be interpreted.
}
  \item{textid}{
Optional vector of text or numeric identifiers for the dosage strings.
}
  \item{dosage_mg}{
Optional vector of numeric milligram dose quantities for each
prescription item.
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
1 or 2 tablets daily). If FALSE, the average is returned. This does
not apply for a choice between dose phrases (e.g. '2 tab 3 times daily,
max 12 daily'), where the first dose phrase is returned with max = 'average'
if it less than the maximum, otherwise the maximum dose is returned with
max = 'max'.
}
  \item{usebuiltinlookups}{
Whether to use the lookup table of exact dose strings and their conversion
included in the package.
}
  \item{customlookups}{
A \code{\link{drugdose_lookups}} object or file path to the a custom
lookups table. This may consist of errors that you have found and corrected,
for which you would like the algorithm to use the correct interpretation
in the future. This lookup table is in the same format as the output 
}
  \item{cores}{
Number of cores to use, for multiprocessor machines running R on Linux.
The 'parallel' package must be loaded in order to use more than one core.
}
  \item{noisy}{
TRUE or FALSE - whether to print debug information to console.
}
}

\details{
This function calls \code{\link{interpret}} repeatedly to analyse
individual dosage texts. First it compiles a list of unique dosage texts
to be analysed, so that each is interpreted only once.
If usebuiltinlookups = TRUE
and a text is found in the lookups table, or if it is in the customlookups
table, the pre-analysed output is
returned, otherwise the \code{\link{interpret}} function is called to
analyse the text. 
}

\value{
Data frame with the following columns:

\item{\code{textid}}{numeric or character; text identifier}
\item{\code{order}}{numeric; if \code{simplify} = TRUE, this column contains '1'.
If \code{simplify} = FALSE, there may be multiple result lines per text
entry, and this column states the order of the result lines.}
\item{\code{qty}}{numeric; dose quantity}
\item{\code{units}}{numeric; dose units}
\item{\code{units}}{character; dose units}
\item{\code{freq}}{numeric; dose frequency per time period}
\item{\code{tot}}{numeric; total dose per time period}
\item{\code{max}}{factor with 3 levels: max, average, exact}
\item{\code{time}}{numeric; time interval in number of days}
\item{\code{change}}{factor with 3 levels: first, second, nochange, combined.
If doses for different time periods combined using \code{simplify} = TRUE,
\code{change} states which dose contributes to the output.}
\item{\code{choice}}{factor with 3 levels: choice, asneeded, nochoice}
\item{\code{duration}}{numeric; duration of prescription}
\item{\code{daily_dose}}{numeric; calculated daily dose (0 = missing)}

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

\note{
Converted from the original Visual Basic algorithm.
}

\seealso{
\code{\link{interpret}}, \code{\link{testdoseconvert}}
}

\examples{
# Using default dictionaries (loaded each time by function)
doseconvert(c('2 tab twice daily',
    'one daily for 1 week then two daily'),
    textid = 1:2, simplify = FALSE)
doseconvert(c('2 tab twice daily', '2 drops in left eye qds',
    '5mcg daily'), textid = 1:3, dosage_mg = c(5, 5, 5))
doseconvert('one daily for 1 week then two daily',
    textid = 2, simplify = TRUE)

# Timing test
system.time(doseconvert(text = '2 tablets daily', textid = 3))
    
# Pre-loading dictionaries for faster analysis
# This is particularly useful when running in server mode for on-demand
# dosage text conversion
data(singlewords)
the_singlewords <- as.drugdose_singlewords(singlewords)
data(multiwords)
the_multiwords <- as.drugdose_multiwords(multiwords)
data(patterns)
the_patterns <- as.drugdose_patterns(patterns)
data(lookups)
the_lookups <- as.drugdose_lookups(lookups)

# Write a function to use pre-loaded dictionaries
doseconvert_fast <- function(text, textid){
    doseconvert(text = text, textid = textid, simplify = TRUE,
    singlewords = the_singlewords, multiwords = the_multiwords,
    patterns = the_patterns, usebuiltinlookups = FALSE,
    customlookups = the_lookups, cores = 1)
}

system.time(doseconvert_fast(text = '2 tablets daily', textid = 3))
}
