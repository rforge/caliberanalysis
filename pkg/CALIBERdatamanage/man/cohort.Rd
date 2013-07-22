\name{cohort}
\alias{cohort}

\title{
S3 class for cohort datasets (one row per patient)
}

\description{
A class for `cohort' datasets for use in epidemiological studies.
}

\usage{
cohort(x, idcolname = c("patid", "anonpatid", "id"), description = NULL)
}

\arguments{
  \item{x}{
a data.frame, ffdf or data.table which contains data on patients, one row per patient
}
  \item{idcolname}{
the column name representing the unique patient identifier
}
  \item{description}{
a data.frame with two columns containing column descriptions. The column names must be 'colname' and 'description'.
}
}

\details{
Cohort objects have a number of associated methods which make it easier to manage data and generate datasets for analysis. Note that data.table objects are not copied by this function; the original object is modified. If you want to leave the original object as it is, use the \code{copy} function, e.g. \code{mycohort <- cohort(copy(mydataset))}.
}

\value{
An object with classes \sQuote{cohort} and either \sQuote{data.table} or \sQuote{ffdf},
and the following attributes:
\describe{
\item{idcolname}{ID column name}
\item{description}{description of the column}
}
If it is a data.table cohort, the ID column is the leftmost column and all the others are arranged
alphabetically.
}

\author{
Anoop Shah
}

\seealso{
\code{\link{print.cohort}}, \code{\link{summary.cohort}}, \code{\link{merge.cohort}}
}

\examples{
DT <- cohort(data.table(anonpatid = 1:3,
    indexdate = as.IDate(c("2012-1-3", "2012-1-2", "2010-1-9"))))
print(DT)
summary(DT)

FFDF <- as.ffdf(data.table(anonpatid = 1:3,
    indexdate = as.IDate(c("2012-1-3", "2012-1-2", "2010-1-9"))))
FFDF <- cohort(FFDF)
print(FFDF)
summary(FFDF)
}
