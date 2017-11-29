\name{explode}
\alias{explode}

\title{
Show terms not included in the selection but sharing the first few characters of the Read, OPCS or ICD10 code
}

\description{
Returns the selection of terms whose code shares the first few characters with a selected term but which are not included in the selection.
}

\usage{
explode(x, level = 3, keep = FALSE, minFreqGPRD = 100, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a selection object or something which can be coerced to a selection (e.g. a codelist)
}
  \item{level}{
how many characters of the code should be the same, default is 3 so that, for example, ICD-10 codes A000 and A001 are selected if A009 is in the original selection.
}
  \item{keep}{
TRUE or FALSE, whether the original selection should be included in the output.
}
  \item{minFreqGPRD}{
the Read dictionary contains many terms which are used infrequently. This argument is the minimum total number of events for a Read term GPRD for it to be included in the \code{explode} selection
}
  \item{\dots}{
other arguments to pass to \code{\link{selection}}, if x needs to be coerced to a selection.
}

}

\details{
This function is called by \code{\link{showhtml}} to show non-selected but related terms in the HTML documentation of the codelist.
}

\value{
A selection object containing terms are selected, excluding the original terms supplied.
}

\seealso{
\code{\link{as.codelist}},  \code{\link{assigncat}}, \code{\link{codematch}}, \code{\link{dictis}}, \code{\link{getdictionary}}, \code{\link{setdictionary}}, \code{\link{termhas}}
}

\examples{
setdictionary('read')
assigncat(1, 'UA|Unstable angina', codematch('I200', 'icd10'))
assigncat(2, 'Angina|Other forms of angina', explode(codematch('I200', 'icd10')))
print(as.codelist())

angina <- selection('angina')
explode(angina)
stopifnot('Preinfarction syndrome' \%in\% as.codelist(explode(angina))[, term])

setdictionary('icd10')
mi <- codematch('I213', 'icd10')
explode(mi)
}
