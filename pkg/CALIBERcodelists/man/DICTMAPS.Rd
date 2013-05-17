\name{CALIBER_DICTMAPS}
\alias{CALIBER_DICTMAPS}

\title{
Mapping between OPCS / ICD-10 and Read terms
}

\description{
Mapping from the UK Clinical Terminology Centre. Can be used to
select a set of Read codes mapping to an ICD-10 code. 

Additional maps have been added where the Read term and ICD-10/OPCS term are identical; these
have map status T. The mapping is via Read terms so that identical Read terms have identical
maps even if the NHS-supplied mapping only covers one of several Read codes with the same term, i.e.

ICD-10/OPCS <-(NHS mapping)-> 5-char Read code <--> Read term <--> 7-char Read code <--> medcode

Dagger ICD-10 terms are not used for matching.
}

\details{
A data table with observations on the following 8 variables.
  \describe{
    \item{\code{code}}{OPCS or ICD-10 code, a character vector}
    \item{\code{map_stat}}{Map status, a character vector. Map status, a character vector.
D is the default map, A is the alternative map, E is an exact match, and
G is a map where the OPCS or ICD-10 term is more general than the Read term.
R is for a term which must be ruled out before allocating a specified ICD-10 term,
for example myocardial infarction must be ruled out in order to code to the ICD-10 term
for essential hypertension, but myocardial infarction is not a map for essential hypertension.
The default values of map_stat allowed by this package are A, D, E or T when searching
Read codes and looking for ICD-10 and OPCS codes that will select corresponding patients,
and A, D, G, E or T when searching for Read codes that map to ICD-10 or OPCS codes for patient selection.}
    \item{\code{ref_flag}}{a character vector, not currently used)}
    \item{\code{add_flag}}{a character vector, not currently used}
    \item{\code{elem_num}}{element number within the block, a numeric vector (currently not used)}
    \item{\code{block_num}}{block number, a numeric vector (currently not used)}
    \item{\code{medcode}}{CPRD medcode (key column)}
    \item{\code{dict}}{a character vector: 'icd10', 'icdhead' or 'opcs'}
  }
}

\note{
For CALIBER users, the tables are included in the CALIBERlookups package.
External users need to obtain their own copies of the dictionaries
and generate the data file using the supplied script \code{demo(loadnewdicts)}.

If there is no CALIBER_DICTMAPS table in the global environment, CALIBERcodelists
functions will create one containing a small sample of terms. This is used for
testing the examples.
}

\source{
UK Terminology Centre
}

\examples{
# data(CALIBER_DICTMAPS)
# print(attr(CALIBER_DICTMAPS, 'VERSION'))
}

\seealso{
\code{\link{codematch}}
}
