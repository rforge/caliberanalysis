\name{cohortmethods}
\alias{subset.cohort}
\alias{print.cohort}
\alias{summary.cohort}
\alias{merge.cohort}
\alias{as.cohort}
\alias{is.cohort}

\title{
Functions for handling cohort objects
}

\description{
Functions for printing, subsetting and merging cohort objects.
}

\usage{
as.cohort(x, \dots)
is.cohort(x)

\method{print}{cohort}(x, \dots)
\method{summary}{cohort}(object, \dots)
\method{subset}{cohort}(x, subset, select)
\method{merge}{cohort}(x, y, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a cohort object
}
  \item{object}{
a cohort object
}
\item{y}{
another cohort object
}
  \item{subset}{
expression denoting which rows to keep. It is interpreted within the environment
of the cohort data.table, so columns can be referred to directly by name
}
  \item{select}{
vector of column names for columns to keep. The ID column is always selected.
}
  \item{\dots}{
additional arguments
}
}

\details{

}

\value{
\code{as.cohort} coerces its argument to a cohort if possible. If it is already a data.table, it is updated by reference, and the new name will be an alias to the existing object. Use \code{copy = TRUE} explicitly to create a cohort object without altering the original object.

\code{is.cohort} returns TRUE or FALSE according to whether the class of
the object is \code{\link{cohort}}

The \code{summary} method displays the description table to the console.

The \code{print} method displays the description table and the data itself, in the way that a data.table is displayed.

The \code{subset} method returns a cohort object which contains a subset of the original cohort.

The \code{merge} method returns a merged cohort containing x and y merged on the ID column.
Both cohort objects must have the same ID column, otherwise they cannot be merged.
}

\author{
Anoop Shah
}


\seealso{
\code{\link{cohort}}, \code{\link{removeColumns}}
}

\examples{
# data.table cohort
COHORT <- cohort(data.table(anonpatid = 1:3, indexdate = as.IDate(c("2012-1-3", 
	"2012-1-2", "2010-1-9"))))
COHORT2 <- cohort(data.table(anonpatid = 1:3, disease = c(TRUE, FALSE, TRUE)))
is.cohort(COHORT)
subset(COHORT, anonpatid == 2)
summary(COHORT)
merge(COHORT, COHORT2)

COHORT <- as.ffdf(COHORT)
COHORT2 <- as.ffdf(COHORT2)
is.cohort(COHORT)
subset(COHORT, COHORT$anonpatid == 2)
merge(COHORT, COHORT2)
}
