\name{testdoseconvert}
\alias{testdoseconvert}

\title{
Test the dose conversion program against a set of texts with gold standard interpretation
}

\description{
Calls \code{\link{doseconvert}} on each individual dose string, to
extract structured information such as quantity, frequency and units.
}

\usage{
testdoseconvert(text = NULL, infile = NULL, outfile = NULL,
    outfileerrors = NULL, singlewords = NULL, multiwords = NULL,
    patterns = NULL, uselookups = FALSE, lookups = NULL,
    customlookups = NULL, noisy = TRUE, tolerance = 0.001, ...)
}

\arguments{
  \item{text}{
A single dosage text to interpret.
}
  \item{infile}{
A delimited text file which can be read by fread, with columns
"text" and (optionally) "textid".
}
  \item{outfile}{
File name for output .csv file.
}
  \item{outfileerrors}{
File name for errors compared to the gold standard; .csv format.
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
  \item{noisy}{
TRUE to write each text and the results of interpretation to the console,
with a detailed analysis report for the first text analysed.
}
  \item{tolerance}{
The tolerance for comparing daily doses (to allow for rounding errors).
}
  \item{\dots}{
Other arguments to pass to \code{\link{doseconvert}}.
}
}

\details{
This function can take input from a variety of sources, which have the
following priority: 1. text, 2. infile, 3. customlookups, 4. lookups.
In 'noisy' mode each text and the results of interpretation are written
to the console, with a detailed analysis report for the first text analysed.
}

\value{
Data frame with the following columns:

\item{qty}{Description of 'comp2'}
\item{units}{Description of 'comp2'}
\item{freq}{dose frequency per time interval}
\item{tot}{total dose per time period}
\item{max}{factor with 3 levels: max, average, exact}
\item{time}{Description of 'comp2'}
\item{change}{factor with 3 levels: first, second, nochange.
If doses for different dime periods combined using simplify=TRUE,
\code{change} states which dose contributes to the output.}
\item{choice}{whether there is a choice of dose.}

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
\code{\link{interpret}}, \code{\link{doseconvert}}
}

