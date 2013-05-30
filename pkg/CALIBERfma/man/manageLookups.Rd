\name{manageLookups}
\alias{importLookup}
\alias{exportLookup}
\alias{addtoLookup}

\title{
Manage the Freetext Matching Algorithm lookup tables
}

\description{
Import, export and modify lookup tables for the Freetext Matching Algorithm. Currently these functions do not verify that the format of the lookup is correct, but this feature may be added in the future. The format of the lookup tables is specified here: \url{https://github.com/anoopshah/freetext-matching-algorithm-lookups/blob/master/README.md}
}

\usage{
importLookup(filename, use_CALIBERfma_dir_R)
exportLookup(lookup, filename)
addtoLookup(lookup, valuelist)
}

\arguments{
  \item{lookup}{
a \code{data.table} containing a lookup table for the Freetext Matching Algorithm
}
  \item{valuelist}{
a list or \code{data.frame} containing the additional row(s) to append to the end of the lookup table
}
  \item{filename}{
file name to import or export lookup (either a full file path, or the file name alone if
importing / exporting to the current directory or \code{use_CALIBERfma_dir_R} = TRUE)
}
  \item{use_CALIBERfma_dir_R}{
TRUE if the lookup table should be sought in the folder specified in the option
'CALIBERfma_dir_R'.
}
}


\value{
\code{importLookup} and \code{addtoLookup} return the lookup as a data.table. \code{exportLookup} has no return value.
}

\references{
\url{https://github.com/anoopshah/freetext-matching-algorithm-lookups/}
}

\author{
Anoop Shah
}

\seealso{
\code{\link{fmaTest}}, \code{\link{doAnalysis}}, \code{\link{downloadFMA}}
}

\examples{
mylookup <- data.table(medcode=1:2, readcode=c('246..00', '22A..00'))
addtoLookup(mylookup, list(medcode=3:4, readcode=c('229..00', '423..00')))
addtoLookup(mylookup,
	data.frame(medcode=3:4, readcode=c('229..00', '423..00')))
# any column not supplied is considered to be missing
addtoLookup(mylookup, data.table(medcode=3:4))
}
