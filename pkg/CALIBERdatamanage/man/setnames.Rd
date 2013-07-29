\name{setnames}
\alias{setnames.cohort}
\alias{setnames}
\alias{setnames.ffdf}
\alias{setnames.data.table}

\title{
Modify column names for ffdf, cohort or data.table objects
}

\description{
A version of the \code{setnames} data.table function for ffdf data frames or \code{\link{cohort}} objects.
}

\usage{
setnames(x, old, new)
\method{setnames}{data.table}(x, old, new)
\method{setnames}{cohort}(x, old, new)
\method{setnames}{ffdf}(x, old, new)
}

\arguments{
  \item{x}{
A data.table, cohort or ffdf object
}
  \item{old}{
Old column names or positions, or a vector of all new column names if \code{new} is missing.
}
  \item{new}{
New column names
}
}

\details{
Defines 'setnames' as a generic function. For data.table objects, it
dispatches the \code{setnames} method in the data.table package.
For cohort objects, it also updates the description table and idcolname.
For FFDF objects, the object is not modified by reference but the modified
object is returned.
}

\value{
Invisibly returns the modified object. If it is a data.table, it is also updated by reference.
}

\examples{
COHORT <- cohort(data.table(anonpatid = 1:3, indexdate = as.IDate(c("2012-1-3", 
	"2012-1-2", "2010-1-9"))))
FFDFCOHORT <- as.ffdf(COHORT)

# Data table cohort does not need assignment for changing names
print(COHORT)
setnames(COHORT, c('anonpatid', 'indexdate'), c('id', 'startdate'))
print(COHORT)

print(FFDFCOHORT)
FFDFCOHORT <- setnames(FFDFCOHORT, c('anonpatid', 'indexdate'), c('id', 'startdate'))
print(FFDFCOHORT)
}

