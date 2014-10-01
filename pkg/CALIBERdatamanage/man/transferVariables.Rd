\name{transferVariables}
\alias{transferVariables}
\alias{transferColumns}

\title{
Transfer variables from one dataset to another
}

\description{
Copy variables from one dataset (ffdf or data.table)
to another. The key column must
be unique in the data.table being transferred from.
}

\usage{
transferVariables(fromdata, todata, varnames,
    by = NULL, description = NULL, drop = FALSE)
transferColumns(fromdata, todata, varnames,
    by = NULL, description = NULL, drop = FALSE)
}

\arguments{
  \item{fromdata}{
ffdf or data.table from which variables are to be taken.
}
  \item{todata}{
ffdf or data.table to which variables are to be copied.
}
  \item{varnames}{
character vector of variables to transfer.
}
  \item{by}{
key column to merge by, default is the ID column of
fromdata or todata.
}
  \item{description}{
character vector containing a description for each new variable,
recycled as necessary. This is used to update the description
table if \code{todata} is a cohort object, and is ignored
otherwise. If NULL, the description is taken from fromdata
(if it is a cohort), otherwise it defaults to 
'transferVariable' with date and time.
}
  \item{drop}{
TRUE or FALSE, whether to delete the variables from fromdata
(default = FALSE). This is only possible if fromdata is a data.table.
}
}

\details{
Some calculations on repeat measures may require patient-level variables
such as date of birth. This is a convenience function to replicate such
variables for all rows in a dataset pertaining to a particular patient.

A warning is given if a column is over-written or if a column
is not found in the \code{fromdata} dataset. If \code{todata} is a
data.table it is modified by reference.
}

\value{
The modified dataset \code{todata} is returned invisibly. If it is
a data.table it is also modified by reference.
}

\seealso{
\code{\link{cohort}}, \code{\link{merge.cohort}}
}

\examples{
##
mycohort <- data.table(anonpatid=1:2,
    indexdate=as.IDate(c('2010-01-01', '2009-01-01')),
    ethnic_hes=c('Black', 'White'))
mycohort <- as.cohort(mycohort)
modifyDescription(mycohort, c('indexdate', 'ethnic_hes'),
    c('Study entry date', 'Ethnicity as recorded in HES'))
mydata <- data.table(anonpatid=c(2, 2, 3),
    eventdate=as.IDate(c('2006-01-01', '2008-01-01', '2005-01-01')))
mycohort2 <- as.cohort(mydata[2:3])

# A common use of this function might be to copy the index dates
# to a repeated measures file.
transferVariables(mycohort, mydata, 'indexdate')

# Assignment is optional using data.table because the original
# data.table is also updated by reference. This has the
# same result as the previous command.
mydata <- transferVariables(mycohort, mydata, 'indexdate')
print(mydata)

# Check that the dates are correct
stopifnot(identical(as.IDate(mydata$indexdate),
	as.IDate(c('2009-01-01', '2009-01-01', NA))))

# Over-writing variables.
transferVariables(mycohort, mydata, c('indexdate', 'ethnic_hes'))
print(mydata)
print(mycohort)

# Transferring using FFDF
mycohortffdf <- as.ffdf(mycohort)
mydataffdf <- as.ffdf(mydata)

mydataffdf <- transferVariables(mycohortffdf, mydata, 'indexdate')
print(mydataffdf)

mydataffdf <- removeColumns(mydataffdf, 'indexdate')

# FFDF objects are not updated by reference, so it is necessary
# to use assignment. Without assignment, the dataset is not updated.
transferVariables(mycohort, mydataffdf, 'indexdate')
print(mydataffdf)

# Assignment (<-) using ffdf. This is quick because only the
# R object is copied, not the underlying flat files.
mydataffdf <- transferVariables(mycohort, mydataffdf, 'indexdate')
print(mydataffdf)

# Transferring from cohort to cohort, and dropping in the 
# source data.table
transferVariables(mycohort, mycohort2, 'indexdate', drop = TRUE)
print(mycohort2)
}
