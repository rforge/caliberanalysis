\name{transferVariables}
\alias{transferVariables}

\title{
Transfer variables from one dataset to another
}

\description{
If you have multiple data.tables with different sets of variables,
this function can be used to transfer variables from one data.table to another.
However, the key column (usually anonpatid) must be unique in the data.table
being transferred from.
}

\usage{
transferVariables(fromdata, todata, varnames,
    by = 'anonpatid', keep = TRUE, description = NULL)
}

\arguments{
  \item{fromdata}{
data.table from which variables are to be taken.
}
  \item{todata}{
data.table to which variables are to be transferred.
}
  \item{varnames}{
character vector of variables to transfer.
}
  \item{by}{
key column to merge by. Must be present in \code{from} and
\code{to} data.tables and must be the key column.
}
  \item{keep}{
logical, whether to keep the columns in the \code{from} data.table.
}
  \item{description}{
character vector containing a description for each new variable,
recycled as necessary. This is used to update the description
table if \code{todata} is a cohort object, and is ignored
otherwise. If NULL, the description is
'transferVariable' with date and time.
}
}

\details{
Some calculations on repeat measures may require patient-level variables
such as date of birth. This is a convenience function to replicate such
variables for all rows in a dataset pertaining to a particular patient.
The data.table is modified by reference. Both data.tables must have a 
column named \code{anonpatid}, on which they are joined.

The data.tables are modified by reference. A warning is given if a column
is over-written or if a column is not found in the \code{fromdata} data.table.
If the key column for either data.table is different from \code{by}, it is
changed temporarily and restored at the end of the function.
}

\value{
Character vector of column names in todata data.table, returned invisibly.
}

\seealso{
\code{\link{cohort}}, \code{\link{merge.cohort}}
}

\examples{
##
mycohort <- data.table(anonpatid=1:2,
    indexdate=as.IDate(c('2010-01-01', '2009-01-01')),
    ethnic_hes=c('Black', 'White'))
mydata <- data.table(anonpatid=c(2, 2, 3),
    eventdate=as.IDate(c('2006-01-01', '2008-01-01', '2005-01-01')))

# A common use of this function might be to copy the index dates
# to a repeated measures file.
transferVariables(mycohort, mydata, 'indexdate')
print(mydata)

# Over-writing variables. Transferring without copying.
transferVariables(mycohort, mydata, c('indexdate', 'ethnic_hes'), keep=FALSE)
print(mydata)
print(mycohort)
}
