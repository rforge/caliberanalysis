\name{extractEntity}
\alias{extractEntity}

\title{
Extract and decode data from a CPRD GOLD format dataset
}

\description{
Decodes additional clinical details (data1, data2, ... columns) in raw CPRD data
using the lookups. CALIBER users should use the
CALIBERlookups package, but it is also possible to supply the lookups
directly.
}

\usage{
extractEntity(data, enttype, CALIBER_ENTITY = NULL,
    CALIBER_LOOKUPS = NULL, ...)
}

\arguments{
  \item{data}{
data.table or FFDF data frame containing the CPRD GOLD format data.
}
  \item{enttype}{
entity type(s) to extract, either a single integer or a vector.
If extracting multiple entity types, the data specification for each
entity type must be identical.
}
  \item{CALIBER_ENTITY}{
a table with columns enttype, data_fields, data1, data1_lkup, \dots.
This argument can be omitted if the CALIBERlookups package is installed.
}
  \item{CALIBER_LOOKUPS}{
a table with columns lookup, category, description. This argument can be
omitted if the CALIBERlookups package is installed.
}
  \item{\dots}{
other arguments to pass to \code{\link{YYYYMMDDtoDate}}
for extracting dates from YYYYMMDD format.
}
}

\details{
If the CALIBERlookups package is not installed, CALIBER_ENTITY
and CALIBER_LOOKUPS must be supplied. 

CALIBER_ENTITY states what each of the data entries contains for a
particular entity type, and is a data.table with columns:

\describe{
\item{\code{enttype}}{integer vector, key column, link to test or clinical table}
\item{\code{description}}{a character vector}
\item{\code{filetype}}{a character vector, `Clinical' or `Test'}
\item{\code{category}}{a character vector}
\item{\code{data_fields}}{number of data fields used}
\item{\code{data1}}{definition of data1}
\item{\code{data1_lkup}}{lookup table for data1}
\item{\code{data2}}{definition of data2}
\item{\code{data2_lkup}}{lookup table for data2}
\item{\code{data3}}{definition of data3}
\item{\code{data3_lkup}}{lookup table for data3}
\item{\code{data4}}{definition of data4}
\item{\code{data4_lkup}}{lookup table for data4}
\item{\code{data5}}{definition of data5}
\item{\code{data5_lkup}}{lookup table for data5}
\item{\code{data6}}{definition of data6}
\item{\code{data6_lkup}}{lookup table for data6}
\item{\code{data7}}{definition of data7}
\item{\code{data7_lkup}}{lookup table for data7}
\item{\code{data8}}{definition of data8}
\item{\code{data8_lkup}}{lookup table for data8}
}

If there are fewer than 8 data fields, the additional data columns are not required.

CALIBER_LOOKUPS contains the interpretation of each of the lookup categories,
and is a data.table with columns:

\describe{
\item{\code{lookup}}{character vector, first key column}
\item{\code{category}}{integer vector, second key column}
\item{\code{description}}{character vector}
}

}

\value{
\code{extractEntity} returns a data.table with interpreted entity data. The exact columns depend on
the entity type.

\code{extractMedcodes} returns a data.table or ffdf depending on the format of the original data.
The new column named varname is a factor with levels given by the category labels (shortnames) in the codelist.
}


\author{
Anoop Shah
}


\seealso{
\code{\link{addCodelistToCohort}}, \code{\link{addToCohort}}, \code{\link{extractCodes}}
}

\examples{
data(test_data)
TESTDT <- data.table(test_data)
convertDates(TESTDT)
TESTFFDF <- as.ffdf(TESTDT)

data(test_entity)
ENTITY <- as.data.table(test_entity)

data(test_lookups)
LOOKUPS <- as.data.table(test_lookups)
LOOKUPS[, description := as.character(description)]
LOOKUPS[, lookup := as.character(lookup)]

extractEntity(TESTFFDF, 1, ENTITY, LOOKUPS)
extractEntity(TESTDT, 1, ENTITY, LOOKUPS)

extractEntity(TESTFFDF, 4, ENTITY, LOOKUPS)
extractEntity(TESTDT, 4, ENTITY, LOOKUPS)

extractEntity(TESTFFDF, 5, ENTITY, LOOKUPS)
extractEntity(TESTDT, 5, ENTITY, LOOKUPS)

extractEntity(TESTFFDF, 151, ENTITY, LOOKUPS)
extractEntity(TESTDT, 151, ENTITY, LOOKUPS)
}
