\name{dictis}
\alias{dictis}

\title{
Which terms are in a particular dictionary
}

\description{
This function is intended for use when selecting terms to include in a codelist, if you want to filter by terms in a particular dictionary without altering the global dictionary selection of \code{\link{setdictionary}}.
}

\usage{
dictis(dictName1, dictName2 = NULL, dictName3 = NULL)
}

\arguments{
  \item{dictName1}{
Name of the first dictionary to include, or a vector of dictionary names (from read, icd10, opcs).
}
  \item{dictName2}{
Name of another dictionary to include
}
  \item{dictName3}{
Name of another dictionary to include
}
}


\value{
A selection object.
}

\seealso{
Get and set dictionary: \code{\link{getdictionary}}, \code{\link{setdictionary}}
Other functions for selecting terms in master dictionary: \code{\link{assigncat}}, \code{\link{codematch}}, \code{\link{explode}}, \code{\link{termhas}}
}

\examples{
setdictionary('icd10', 'read')

# Select a specific ICD-10 code without including mapped Read terms
assigncat(1, 'direct', codematch('I200', 'icd10', exact=TRUE) & dictis('icd10'))
as.codelist('icd10')

# Select only mapped Read terms but not the original ICD-10 terms
assigncat(2, 'mapped', codematch('I200', 'icd10', exact=TRUE) & dictis('read'))
as.codelist('read')
}
