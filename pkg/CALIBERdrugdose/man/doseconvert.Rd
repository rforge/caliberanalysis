\name{doseconvert}
\alias{doseconvert}

\title{
Convert a set of dosage texts into structured dosage information
}

\description{
Calls \code{\link{interpret}} on each individual dose string, to
extract structured information such as quantity, frequency and units.
}

\usage{
doseconvert(text, textid = NULL, simplify = TRUE, singlewords = NULL,
    multiwords = NULL, patterns = NULL, uselookups = TRUE,
    lookups = NULL, customlookups = NULL, cores = 1, ...)
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
  \item{uselookups}{
A \code{\link{drugdose_lookups}} object or file path to the lookups
dictionary. If NULL it is loaded from the package data directory.
}
  \item{lookups}{
A \code{\link{drugdose_lookups}} object or file path to the lookups
dictionary. If NULL it is loaded from the package data directory.
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
}

\details{
This function calls \code{\link{interpret}} repeatedly to analyse
individual dosage texts. First it compiles a list of unique dosage texts
to be analysed, so that each is interpreted only once. If uselookups = TRUE
and a text is found in the lookups table, the pre-analysed output is
returned, otherwise the \code{\link{interpret}} function is called to
analyse the text. 
}

\value{
Data frame with the following columns:

\item{qty}{Description of 'comp2'}
\item{units}{Description of 'comp2'}
\item{freq}{dose frequency per time interval}
\item{tot}{total dose per time period}
\item{priority}{[not relevant - to be removed]}
\item{max}{factor with 3 levels: max, average, exact}
\item{time}{Description of 'comp2'}
\item{change}{factor with 3 levels: first, second, nochange.
If doses for different time periods combined using simplify=TRUE,
\code{change} states which dose contributes to the output.}
\item{choice}{whether there is a choice of dose.}

If \code{simplify} = TRUE, the row.names are the \code{ids} if supplied, or equal to the row numbers otherwise.
If \code{simplify} = FALSE, the row.names are \code{ids}.X where X (=1, 2, 3 etc.) is the order of the partial
dose if \code{ids} are supplied, or equal to the row numbers otherwise.
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

\note{
Converted from the original Visual Basic algorithm.
}

\seealso{
\code{\link{interpret}}, \code{\link{testdoseconvert}}
}

\examples{
doseconvert(c('2 tab twice daily',
    'one daily for 1 week then two daily'),
    textid = 1:2, simplify = FALSE)
doseconvert('one daily for 1 week then two daily',
    textid = 2, simplify = TRUE)
}
