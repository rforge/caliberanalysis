\name{removeColumns}
\alias{removeColumns}

\title{
Remove columns from a cohort object
}

\description{
Deletes one or more columns and also deletes their description. 
If the cohort object is a data.table, it is updated by reference,
if a flat file data frame the modified object is returned.
}

\usage{
removeColumns(x, colnames)
}

\arguments{
  \item{x}{
a \code{\link{cohort}} object
}
  \item{colnames}{
a character vector of the column names to delete
}
}

\value{
Returns the modified ffdf or data.table, invisibly.
}

\author{
Anoop Shah
}


\seealso{
\code{\link{cohort}}, \code{\link{subset.cohort}}, \code{\link{modifyDescription}}
}

\examples{
COHORT <- cohort(data.table(anonpatid = 1:3,
    indexdate = as.IDate(c("2012-1-3", "2012-1-2", "2010-1-9")),
    sex = c("M", "F", "M")))
removeColumns(COHORT, c('indexdate', 'sex'))
print(COHORT)
}
