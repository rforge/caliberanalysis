\name{importDT}
\alias{importDT}

\title{
Import a file to data.table
}

\description{
Imports a text or Stata file to data.table, automatically converting dates
}

\usage{
importDT(filename, datecolnames = NULL,
    verbose = TRUE, sep = NULL, zipname = NULL,
    key = NULL, convertLogical = TRUE, fread = TRUE,
    nrows = NULL, ...)
}

\arguments{
  \item{filename}{
a single filename or path to a text file, Stata file,
gz compressed text file or a zip file containing one file.
}
  \item{datecolnames}{
an argument passed to \code{\link{convertDates}} to convert relevant
columns to IDate. It can be either a vector of date column names, or
an empty string (for no dates to be converted) or NULL if an attempt
should be made to convert all columns. Columns which cannot be
coerced to date are left unchanged.
}
  \item{verbose}{
whether to print information about the file loaded
}
  \item{sep}{
a single delimiter character. If NULL, the function will try to
detect it (, ; or TAB) from the first line.
}
  \item{zipname}{
path to a zip file. This is only required if the zip file
contains more than one file, otherwise the path to the zip file
can be supplied as the \code{filename} argument.
}
  \item{key}{
a character vector of key column names, to set the data.table key
}
  \item{convertLogical}{
whether to convert 1 / 0 columns to logical
}
  \item{fread}{
whether to use the new \code{fread} fast file
loader in data.table. It is not used if \code{nrows} is
supplied, because \code{fread} loads the entire file.
This function cannot currently handle escaped quotes, so if the
input file contains escaped quotes, set \code{fread} to FALSE.
}
  \item{nrows}{
number of rows to import, default (NULL) is to import all rows.
}
  \item{\dots}{
other arguments to pass to \code{read.dta},
\code{fread} or \code{read.delim}.
}
}

\details{
A convenience function for importing a text file, including
handling zip compressed files, automatically detecting the 
delimiter and converting dates.
}

\value{
A data.table containing the imported data.
}


\author{
Anoop Shah
}

\note{
In the future this function may be updated to take advantage of
the new fread function in the data.table package.
}

\seealso{
\code{\link{importFFDF}}
}

\examples{
# Import a test dataset
data(test_data)
tempfile <- paste(tempdir(), '/tmp.csv', sep = '')
write.csv(test_data, tempfile, row.names = FALSE)

# Default (fread = TRUE)
importDT(tempfile)
# Using the older read.delim function
importDT(tempfile, fread = FALSE)
# Limiting the number of rows
importDT(tempfile, nrows = 3)

# Convert to FFDF
as.ffdf(importDT(tempfile))

# Importing another test dataset
data(test_import)
write.csv(test_import, tempfile, row.names = FALSE)
importDT(tempfile)
# datenotok is not converted because of an error
str(importDT(tempfile))

# Importing only one row (nrows is an argument to read.delim)
importDT(tempfile, nrows = 1)
unlink(tempfile)

# Importing Stata file
tempstata <- paste(tempdir(), '/tmp.dta', sep = '')
write.dta(test_data, tempstata)
importDT(tempstata)
unlink(tempstata)
}
