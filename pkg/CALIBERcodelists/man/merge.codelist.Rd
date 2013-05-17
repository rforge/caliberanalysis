\name{merge.codelist}
\alias{merge.codelist}

\title{
Merge two codelists
}

\description{
Merge two codelists, overwriting categories and metadata using the second codelist.
}

\usage{
\method{merge}{codelist}(x, y = NULL, ...)
}

\arguments{
  \item{x}{
a codelist
}
  \item{y}{
a codelist from the source dictionary as \code{x}.
If NULL, it is extracted from the codelist under construction in the master dictionary.
}
  \item{\dots}{not used}
}

\value{
A codelist containing all terms in \code{x} and \code{y}, with merged
metadata. Priority is always given to \code{y} if any information differs
between the codelists.
}

\note{
The timestamp of the resultant codelist is the time when the function was run.
}

\seealso{
\code{\link{codelist}}, \code{\link{convert}}, \code{\link{compare}}
}

\examples{
setdictionary('icd10') 
assigncat(2, 'MI|Myocardial infarction', termhas('myocardial infarction'))
setMetadata(Name='mi')
mi <- as.codelist()

setdictionary('icd10') 
chd <- as.codelist(codematch('I2[012345]', 'icd10')) 
# select codes mapping to ICD-10 CHD
addCategory(1, 'Coronary heart disease', codelist=chd)
setattr(chd, 'Name', 'chd')

# Merge the codelists, with priority for CHD
merge(mi, chd)
# Merge the codelists, giving priority to MI
merge(chd, mi)
}
