\name{convertDates}
\alias{convertDates}

\title{
Converts all date columns in a data.table from character to IDate
}

\description{
Applies \code{textToDate} in the CALIBERcodelists package
repeatedly to convert each relevant column from
character to IDate (integer date). 
}

\usage{
convertDates(data, datecolnames = NULL, verbose = TRUE)
}

\arguments{
  \item{data}{
data.table which contains the columns to be converted.
}
  \item{datecolnames}{
optional vector of names of date columns. If some of these columns
are not in data, they are ignored.
}
  \item{verbose}{
whether to print a message stating which columns were converted.
}
}

\details{
If date conversion is unsuccessful,
the column is left in its original form (character or factor).
}

\value{
The modified data.table or ffdf. If \code{data} is a data.table, it is also updated by reference.
}

\seealso{
\code{\link{textToDate}}
}

\examples{
# Create a cohort for testing
mydata <- data.table(anonpatid=1:3,
	indexdate=c('2010-01-01', '2009-03-05', '2008-05-06'),
	deathdate=c(NA, '', '2009-09-08'))
# Convert dates in ffdf
myffdf <- as.ffdf(mydata)
myffdf <- convertDates(myffdf)
print(myffdf)

# Convert dates in data.table
# (updated by reference so no assignment is needed)
convertDates(mydata) # is the same as mydata <- convertDates(mydata)
print(mydata)
}
