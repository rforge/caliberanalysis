\name{removeColumn}
\alias{removeColumn}

\title{
Remove columns from a cohort object
}

\description{
Deletes one or more columns and also deletes their description. The cohort
object is updated by reference.
}

\usage{
removeColumn(x, colname)
}

\arguments{
  \item{x}{
a \code{\link{cohort}} object
}
  \item{colname}{
a character vector of the column names to delete
}
}

\value{
Returns the description table, invisibly.

\item{colname}{Column name}
\item{description}{Column description}
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
removeColumn(COHORT, c('indexdate', 'sex'))
print(COHORT)
}
