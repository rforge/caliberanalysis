\name{CALIBER_GEM}
\alias{CALIBER_GEM}

\title{
General Equivalence Mappings from ICD-9-CM to ICD-10-CM and vice versa
}

\description{
These two data.tables contain the mapping between the 9th and 10th versions of the Clinical Modification of the International Classification of Diseases.
}

\details{
  A data.table with observations on the following 11 variables.
  \describe{
    \item{\code{icd9cm}}{ICD-9 CM code}
    \item{\code{icd10cm}}{ICD-10 CM code}
    \item{\code{approximate}}{logical, whether the map is approximate instead of exact}
    \item{\code{no_map}}{logical, whether there is no map for a particular code}
    \item{\code{combination}}{logical, whether a combination of two or more mapped terms is required to convey the meaning of the source term}
    \item{\code{scenario}}{integer, alternative map (0 if no alternatives)}
    \item{\code{choice_list}}{integer, component of a multi-term map (0 if single-term map)}
    \item{\code{icd9}}{3 or 4 character ICD-9 code}
    \item{\code{icd10}}{3 or 4 character ICD-9 code}
    \item{\code{use}}{logical, whether to use this mapping in CALIBERcodelists. We use maps if no_map is FALSE and choice_list is 0 or 1.}
    \item{\code{from9to10}}{logical, TRUE for ICD-9 CM to ICD-10 CM mapping; FALSE for ICD-10 CM to ICD-9 CM mapping}
  }
}

\note{
For CALIBER users, the tables are included in the CALIBERlookups package.

If there is no CALIBER_GEM table in the global environment, CALIBERcodelists
functions will create one containing a small sample of terms. This is used for
testing the examples.
}


\source{
2013 release of the National Center for Health Statistics (NCHS/CDC)
public domain diagnosis code reference mappings of the International
Classification of Diseases 10th Revision Clinical Modification (ICD-10-CM) and the
International Classification of Diseases 9th Revision (ICD-9-CM) Volumes 1 & 2. 
}

\references{
\url{http://www.cms.gov/Medicare/Coding/ICD10/2013-ICD-10-CM-and-GEMs.html}
}

\seealso{
\code{\link{codematch}}, \code{\link{convert}}
}
