\name{modifyDescription}
\alias{modifyDescription}
\alias{purgeDescription}

\title{
Modify the description table for a data.frame, ffdf, data.table or matrix object.
}

\description{
Cohort objects (see \code{\link{cohort}}) contain a description table as an attribute, which can be used to store column descriptions. \code{purgeDescription} removes description entries for non-existent columns.
}

\usage{
modifyDescription(x, colname, description)
purgeDescription(x)
}

\arguments{
  \item{x}{
a data.frame, ffdf, data.table or matrix object.
}
  \item{colname}{
vector of column names
}
  \item{description}{
vector of new descriptions
}
}


\value{
Both functions invisibly return the modified object and
update it by reference.
}


\author{
Anoop Shah
}


\seealso{
\code{\link{cohort}}
}

\examples{
COHORT <- cohort(data.table(anonpatid = 1:3, indexdate = as.IDate(c("2012-1-3", 
  "2012-1-2", "2010-1-9"))))

modifyDescription(COHORT, 'indexdate', 'date when the patient presented')
modifyDescription(COHORT, 'another', 'a non-existent column')
summary(COHORT)
# Cohort with 3 patients.
# ID column: anonpatid
# 
# COLUMN DESCRIPTIONS
# another (NULL): a non-existent column
# indexdate (IDate): date when the patient presented
# Warning message:
# In modifyDescription(COHORT, "another", "a non-existent column") :
#   another not in cohort

purgeDescription(COHORT)
summary(COHORT)
# Cohort with 3 patients.
# ID column: anonpatid
# 
# COLUMN DESCRIPTIONS
# indexdate (IDate): date when the patient presented

# Description for a matrix object
a <- matrix(1:4, nrow = 2, ncol = 2)
colnames(a) <- c('This', 'That')
a <- modifyDescription(a, 'This', 'This is the first column')
a <- modifyDescription(a, c('This', 'That'),
	c('This is the first column', 'That is the second column'))
}
