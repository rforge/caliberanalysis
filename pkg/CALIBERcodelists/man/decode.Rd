\name{decode}
\alias{decode}

\title{
Show term descriptions for ICD, OPCS or Read codes
}

\description{
Decodes Read, OPCS, ICD-9 or ICD-10 codes, returning a vector of terms.
}

\usage{
decode(codes, dictionary)
}

\arguments{
  \item{codes}{
either a numeric vector of medcodes or a character vector of ICD-9, ICD-10, OPCS or Read codes.
}
  \item{dictionary}{
either a single dictionary (if all codes are in the same dictionary) or a vector of dictionaries,
'icd9', 'icd10', 'opcs' or 'read'.
}
}

\value{
a character vector of terms
}


\examples{
decode('I210', 'icd10')
decode(c(31421, 11347), 'read')
decode(c('E282', 'C165.00'), c('opcs', 'read'))
}
