\name{codematch}
\alias{codematch}

\title{
Select ICD10, OPCS or Read codes matching a particular pattern
}

\description{
Selects ICD10, OPCS or Read codes matching a particular pattern together with mapped terms in other dictionaries.
Returns a selection object. This can be combined with the output from functions such as \code{\link{termhas}}
to generate a complex query for selecting terms of interest.
}

\usage{
codematch(regexpr, dictionary = ifelse(length(getdictionary()) == 1,
    getdictionary(), ""), mapStatus = NULL, exact = FALSE)
}


\arguments{
  \item{regexpr}{
Read / ICD10 / OPCS code to match. This can be a regular expression if EXACT=FALSE, otherwise it should be the exact code.
}
  \item{dictionary}{
One of 'icd10', 'opcs' or 'read'. If not given, and if only one dictionary is in use, this dictionary is taken as the set of codes to match.
}
  \item{mapStatus}{
Character vector stating which terms are acceptable to include according to the match characteristics. \code{D} is the default map, \code{A} is the alternative map, \code{E} is an exact match, and \code{G} is a map where the OPCS or ICD-10 term is more general than the Read term. \code{T} is not supplied by the NHS but is a mapping between identical terms (non-case-sensitive). \code{R} is for a term which must be ruled out before allocating a specified ICD-10 term, for example myocardial infarction must be ruled out in order to code to the ICD-10 term for essential hypertension, but myocardial infarction is not a map for essential hypertension. The default \code{mapStatus} is A, D, E or T when searching Read codes, and A, D, G, E or T when searching ICD-10 or OPCS codes.
}
  \item{exact}{
Whether the \code{regexpr} should be treated as a code for exact matching or a regular expression.
}
}

\details{
The maps are provided in the NHS Read Clinical Terms Version 3, and are in the \code{\link{CALIBER_DICTMAPS}} table.
}
\value{
A \code{\link{selection}} object.
}
\references{
NHS Information Centre 
}
\author{
Anoop Shah
}

\seealso{
\code{\link{as.codelist}},  \code{\link{assigncat}}, \code{\link{dictis}}, \code{\link{explode}}, \code{\link{getdictionary}}, \code{\link{setdictionary}}, \code{\link{termhas}}

To convert codelists from one dictionary to another: \code{\link{convert}}
}

\examples{
## Selecting Read terms linked to ICD-10 terms for myocardial infarction
setdictionary('read')
print(codematch('I2[123]', 'icd10'))

# Look for `exact' matches and identical terms only.
print(codematch('I2[123]', 'icd10', mapStatus=c('E', 'T')))
}
