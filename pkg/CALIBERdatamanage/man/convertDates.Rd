\name{convertDates}
\alias{convertDates}

\title{
Converts all date columns in a data.table from character to IDate
}

\description{
Applies \code{textToDate} in the CALIBERcodelists package
repeatedly to convert each relevant column from
character to IDate (integer date), returning the final
data types of all columns. The data.table is modified by reference. 
}

\usage{
convertDates(data, datecolnames = NULL)
}

\arguments{
  \item{data}{
data.table which contains the columns to be converted.
}
  \item{datecolnames}{
optional vector of names of date columns. If some of these columns
are not in data, they are ignored.
}
}

\details{
The data.table is modified by reference. If date conversion is unsuccessful,
the column is left in its original form (character or factor).
}

\value{
A named character vector of the data types of all variables in \code{data}.
}

\seealso{
\code{\link{textToDate}}
}

\examples{
##
mycohort <- data.table(anonpatid=1:3,
	indexdate=c('2010-01-01', '2009-03-05', '2008-05-06'),
	deathdate=c(NA, '', '2009-09-08'))
convertDates(mycohort)
print(mycohort)
}
