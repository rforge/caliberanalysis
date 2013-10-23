\name{CALIBER_DICT}
\alias{CALIBER_DICT}

\title{
The master dictionary containing the current selection of Read, ICD and OPCS terms
}

\description{
The CALIBER_DICT master dictionary contains all the disease and procedure codes in the CALIBER database.
}

\details{
  A data frame with observations on the following 6 variables.
  \describe{
    \item{\code{code}}{Read, OPCS, ICD-9 or ICD-10 code}
    \item{\code{term}}{Read, OPCS, ICD-9 or ICD-10 term}
    \item{\code{dict}}{one of 'read', 'opcs', 'icd9', 'icd9head', 'icd10' or 'icdhead'. 'icd10' is for 4 or 5 character ICD-10 codes which are allowable in HES data. 'icdhead' contains the 3-character ICD-10 headers which encompass one or more codes in 'icd10'. ICD-10 headers with no subdivisions have a corresponding 4-character code in the icd10 section ending in X, e.g. I48 and I48X (atrial fibrillation and flutter).}
    \item{\code{medcode}}{GPRD GOLD medcode for Read terms. Zero for ICD-10 and OPCS terms.}
    \item{\code{events}}{number of events in GPRD for Read terms}
    \item{\code{termlc}}{lower case version of term without prefix ([X], [SO] etc.) for term matching.}
    \item{\code{category}}{categories for codelist development}
  }

The CALIBERcodelists functions work whether CALIBER_DICT is sealed within the namespace or is a separate data file loaded into the user's workspace.

The attributes VERSION_READ, VERSION_OPCS, VERSION_ICD9 and VERSION_ICD10 contain information about the version of these dictionaries that are contained within CALIBER_DICT, and should be updated whenever CALIBER_DICT is updated.

The key columns are \code{dict}, \code{code}.
}

\note{
For CALIBER users, the tables are included in the CALIBERlookups package.
This package cannot be distributed outside CALIBER investigators because
the ICD-10 dictionary is sublicenced to the National Health Service by the
World Health Organization for use in the NHS and UK academic research only.
External users need to obtain their own copies of the dictionaries
and generate the data file using the supplied script \code{demo(loadnewdicts)}.

If there is no CALIBER_DICT table in the global environment, CALIBERcodelists
functions will create one containing a small sample of terms. This is used for
testing the examples.
}


\source{
Read dictionary from GPRD Pegasus Medical Dictionary, database build Feb 2010. ICD-10 and OPCS codes from UK Terminology Service.
}

