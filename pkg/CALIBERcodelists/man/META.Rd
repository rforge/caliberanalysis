\name{META}
\alias{META}

\title{
Metadata for the master dictionary, stored in a data.table
}

\description{
This is a data.table for storing metadata for any codelists being developed using the master dictionary (\code{\link{CALIBER_DICT}}). It is hidden in the CALIBERcodelists namespace, and values can be set by \code{\link{setMetadata}}.
}


\format{
A data.table with 12 observations on the following 2 variables.
  \describe{
    \item{\code{item}}{a character vector, key column}
    \item{\code{value}}{a character vector, value of the item}
  }

The data.table contains the following data (item, value):
  \describe{
    \item{\code{Author}}{User-supplied codelist name (without the _gprd, _hes or _opcs suffix)}
    \item{\code{category}}{Category numbers, stored as a pipe separated string}
    \item{\code{Date}}{User-supplied date to record for the codelist}
    \item{\code{description}}{Category descriptions, stored as a pipe separated string}
    \item{\code{icd10}}{Whether the ICD-10 dictionary is in use, TRUE or FALSE as a character string}
    \item{\code{message}}{Used to temporarily store the output message of \code{\link{compare}}}
    \item{\code{Name}}{User-supplied codelist name (without the _gprd, _hes or _opcs suffix)}
    \item{\code{opcs}}{Whether the OPCS dictionary is in use, TRUE or FALSE as a character string}
    \item{\code{read}}{Whether the Read dictionary is in use, TRUE or FALSE as a character string}
    \item{\code{shortname}}{Brief category descriptions, stored as a pipe separated string}
    \item{\code{spreadsheet}}{(Not relevant for Windows) User-supplied name of spreadsheet program for opening .CSV files, e.g. libreoffice}  
  }
}

\details{
The META table can only be modified by changing the values of a cell by reference. It is sealed inside the namespace so the number of rows and columns, or the type of data in columns cannot be altered while the package is loaded.

If a codelist is generated from the master dictionary using \code{\link{as.codelist}}, the metadata for the codelist is drawn from the META table.
}

\seealso{
\code{\link{setMetadata}}
}
