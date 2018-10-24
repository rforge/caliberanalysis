\name{importFFDF}
\alias{importFFDF}

\title{
Import a file to flat file data.frame (FFDF)
}

\description{
Imports a a text file or multiple files to FFDF, automatically converting dates.
}

\usage{
importFFDF(filename, datecolnames = NULL,
    verbose = TRUE, sep = NULL, zipname = NULL,
    nrowcheck = 30, fread = FALSE, ...)
}

\arguments{
 \item{filename}{
a single filename or vector of filenames
or path to a text file, gz compressed text file
or a zip file containing one file. If multiple files are to
be loaded, they must all have columns in the same order
with the same data types and the same column names.
}
  \item{datecolnames}{
which columns to cenvert to Date or IDate.
It can be either a vector of date column names, or
an empty string (for no dates to be converted) or NULL if an attempt
should be made to convert all columns. Columns which cannot be
coerced to date are left unchanged. Non-missing dates are not permitted.
}
  \item{verbose}{
whether to print information about the file loaded.
}
  \item{sep}{
a single delimiter character. If NULL, the function will try to
detect it (, ; or TAB) from the first line.
}
  \item{zipname}{
path to a zip file. This is only required if the zip file
contains more than one file, otherwise the path to the zip file
can be supplied as the \code{filename} argument. If \code{multi} = TRUE,
zipname can be vector of paths to multiple zip files.
}
  \item{nrowcheck}{
number of rows checked to determine the data type when loading chunkwise
  }
  \item{fread}{
whether to use data.table's fread function for loading data
  }
  \item{\dots}{
other arguments to pass to \code{read.delim}
}
}

\value{
a FFDF (flat file data frame) object. This is a dataset stored physically
on the hard drive, with a hybrid index in RAM so that elements can be 
accessed quickly.
}

\author{
Anoop Shah
}

\seealso{
\code{\link{importDT}}, \code{\link{extractEntity}}, \code{\link{as.data.table.ffdf}}
}

\examples{
# Import a test dataset
data(test_data)
tempfile <- paste(tempdir(), '/tmp.csv', sep='')
write.csv(test_data, tempfile, row.names = FALSE)
importFFDF(tempfile)

# Convert to data.table
as.data.table(importFFDF(tempfile))

# Importing multiple datasets
for (enttype in c(1, 4, 5, 21, 151)){
    write.csv(test_data[test_data$enttype == enttype, ],
        paste(tempfile, enttype, sep=''), row.names = FALSE)
}
importFFDF(paste(tempfile, c(1, 4, 5, 21, 151), sep = ''))

# Clean up
unlink(tempfile)
}
