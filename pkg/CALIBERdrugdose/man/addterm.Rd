\name{addterm}
\alias{addterm}
\alias{addterm.drugdose_singlewords}
\alias{addterm.drugdose_multiwords}
\alias{addterm.drugdose_patterns}
\alias{addterm.drugdose_lookups}

\title{
Add a Single Row to a Lookup Table
}

\description{
Returns an updated version of one of the dictionaries / lookup tables
with an additional row.
}

\usage{
addterm(x, \dots)
\method{addterm}{drugdose_singlewords}(x, words = "", replacement = "")
\method{addterm}{drugdose_multiwords}(x, order = NA, words = "", replacement = "",
    comment = "")
\method{addterm}{drugdose_patterns}(x, order = NA, words = "", qty = NA, units = "",
    freq = NA, tot = NA, priority = 0, max = "exact", time = NA,
    change = "nochange", choice = "nochoice", doubledose = FALSE,
    duration = NA, category = "", comment = "")
\method{addterm}{drugdose_lookups}(x, textid = 0, text = "", qty = NA, units = "",
    freq = NA, tot = NA, max = "exact", time = NA,
    change = "nochange", choice = "nochoice", duration = NA,
    daily_dose)
}

\arguments{
  \item{x}{
An R object of class \code{\link{drugdose_singlewords}},
\code{\link{drugdose_multiwords}},
\code{\link{drugdose_patterns}} or \code{\link{drugdose_lookups}}.
}
  \item{textid}{
Identifier for lookup (numeric).
}
  \item{words}{
Single word for singlewords, regular expression for multiwords and
patterns.
}
  \item{text}{
Complete dosage text for lookup table.
}
  \item{replacement}{
Replacement for words.
}
  \item{order}{
A number to denote the order in which pattern matching rules are 
applied (for patterns and multiwords dictionaries).
}

  \item{qty}{
Quantity per dose.
}
  \item{units}{
Dose units.
}
  \item{freq}{
Frequency of dosing per time period.
}
  \item{tot}{
Total dose per time period.
}
  \item{priority}{
Whether this pattern can override other patterns (higher number = greater priority) 
}
  \item{max}{
"max", "average" or "exact" (default). 
}
  \item{time}{
Time period in days.
}
  \item{change}{
"nochange", "first" (there is a change of dose, and the extracted
daily dose refers to the dose for the first period of the prescrription),
"second" (second dose is reported) or "combined" (multiple dose lines
with defined durations are combined). "nochange" is default.
}
  \item{choice}{
"nochoice", "choice" or "asneeded". "nochoice" is default.
}
  \item{doubledose}{
TRUE if the pattern implies that the dose should be doubled because
it is used in both eyes, ears or nostrils.
}
  \item{duration}{
Prescription duration
}
  \item{daily_dose}{
Total number of dose units per day.
}
  \item{category}{
Category of pattern, for reference only, not used in analysis.
}
  \item{comment}{
Comment on the multiwords or patterns entry.
}
  \item{\dots}{
Generic arguments for \code{addterm} method.   
}
}

\value{
Updated version of the object x, containing the extra row
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
\code{\link{as.drugdose_singlewords}}, \code{\link{as.drugdose_multiwords}},
\code{\link{as.drugdose_patterns}},    \code{\link{as.drugdose_lookups}},
\code{\link{export}}
}

\examples{
data(singlewords)
singlewords <- as.drugdose_singlewords(singlewords)

mydict <- addterm(singlewords, words = 'tabletes', replacement = 'tab')
}
