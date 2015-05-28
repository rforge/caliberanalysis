\name{contractCodelist}
\alias{contractCodelist}
\alias{expandCodelist}

\title{
Contract and expand ICD-10 codelists according to the hierarchy
}

\description{
\code{contractCodelist} returns a codelist with ICD-10 codes grouped into category headings if possible. \code{expandCodelist} expands a contracted codelist to include lower level ICD-10 codes.
}

\usage{
expandCodelist(codelist, ...)
contractCodelist(codelist)
}

\arguments{
  \item{codelist}{
The ICD-10 codelist to expand or contract. It must be a codelist object.
}
  \item{\dots}{
Currently the only optional argument is \code{allow5char} for \code{expandCodelist} - whether to expand 5-character ICD codes (the default setting is to only expand to 4 character codes).
}
}

\details{
ICD-10 codelists are in the expanded form when displayed in HTML, for comparing codelists and when the codelist is selected using the master dictionary. The codelist is contracted for export, to create a shorter codelist which is easier to understand and describe. This happens automatically; these functions do not usually need to be called by the user.
}

\value{
Object of type 'codelist' with an attribute \code{Expanded} which is TRUE or FALSE. Codelists which have been expanded contain an extra character column 'hierarchy', which is 'parent' for and ICD-10 heading, 'child' for a child code and 'normal' for a code without a parent or child with the same category. The codelist can be 'contracted' by removing all the children, because they are all included in another code.
}


\seealso{
Functions which automatically expand ICD-10 codelists: \code{\link{as.codelist}}, \code{\link{compare}}, \code{\link{showhtml}}
Functions which automatically contract ICD-10 codelists: \code{\link{export}}
}

\examples{
setdictionary('icd10')
assigncat(1, 'MI|Myocardial infarction', codematch('I210'))
assigncat(2, 'MI NOS', explode(codematch('I210')))
assigncat(3, 'Recent MI', codematch('I2[23]'))
print(as.codelist())
print(contractCodelist(as.codelist()))
print(expandCodelist(as.codelist()))
print(expandCodelist(contractCodelist(as.codelist())))
}

