\name{textToDate}
\alias{textToDate}


\title{
Convert dates in a wide variety of formats to IDate
}

\description{
Automatically detects date format (including UK vs. US dates) and converts
dates to IDate. Uses the pvec function of the parallel package if loaded.
}

\usage{
textToDate(x, missingformat = c('0000-00-00',
    '0000-00-00 00:00:00', '', 'NA'))
}

\arguments{
  \item{x}{
a string representing a date
}
  \item{missingformat}{
strings which represent missing dates
}
}

\details{
The function first tries out different formats on the non-missing values,
including US and UK formats (UK is default). It chooses the format which
results in valid dates. All dates in the datestring must have the same format.
}

\value{
a vector of class IDate if successful, or the original character string if not.
}

\examples{
##
print(textToDate(c('13/6/1970'))) # UK format
print(textToDate(c('6/13/1970'))) # US format
print(textToDate(c('13/6/1970', '6/13/1970'))) # conflict
print(textToDate(c('2009-07-08', '0000-00-00'))) # with blank
print(textToDate(c('5/6/1970', NA, '7/8/1980'))) # missing
print(textToDate(c('5/6/1970', '', '7/8/1980'))) # blank date
print(textToDate(c('5/6/1970', 'NA', NA, '7/8/1980'))) # missing
print(textToDate(c('5/6/1970', '0000-00-00', NA, '7/8/1980'))) # missing
print(textToDate(c('5/6/1970', '70/5/1980', '7/8/1980'))) # illegal 
}
