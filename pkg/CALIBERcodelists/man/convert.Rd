\name{convert}
\alias{convert}

\title{
Convert codelists between ICD-10, Read and OPCS
}

\description{
Converts codelists or selection objects from one dictionary to another, using
the NHS mapping or if the terms are identical in the two dictionaries.
}

\usage{
convert(x, toDictionary = NULL, fromDictionary = NULL, ...)
}

\arguments{
  \item{x}{
a \code{\link{codelist}} or \code{\link{selection}} object.
}
  \item{toDictionary}{
dictionary to convert to.
}
  \item{fromDictionary}{
dictionary to convert from, optional if it is obvious (e.g. if x is a codelist).
}
  \item{\dots}{
other arguments, e.g. mapStatus (see \code{\link{codematch}}).
}
}

\details{
Diagnostic ICD-10 terms are only mapped to the diagnostic Read chapters, not to
codes in other chapters which might give clues as to a diagnosis (e.g. MI on ECG).
Some maps are missing if there is no corresponding ICD-10 term for a Read term (e.g. for STEMI)
ICD-10 asterisk terms are permitted for ICD-10 to Read maps, but not the other way round, and
dagger terms are not permitted for mapping.
}

\value{
a codelist or selection converted into the new dictionary.
}


\note{
ICD-10 terms cannot be converted to OPCS because they are different types of term:
ICD-10 terms are for diagnoses and OPCS are procedures.
}

\examples{
mihes <- codelist(data.frame(icd_code='I21'))
mihes
migprd <- convert(mihes, 'read')
migprd

# Conversion to and from ICD-9
anginaicd10 <- codelist(data.frame(icd_code='I20'))
anginaicd9 <- convert(anginaicd10, 'icd9')
anginaicd9
convert(anginaicd9, 'read')
convert(anginaicd9, 'icd10')
}
