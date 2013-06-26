\name{addToCohort}
\alias{addToCohort}

\title{
Generate a variable from repeat measures
}

\description{
Convert a variable with multiple records per patient to one record per patient
relative to an index date and a defined time window.
}

\usage{
addToCohort(x, varname, data, old_varname = "value",
    value_choice = function(x) max(x, na.rm = TRUE),
    date_priority = c("all", "first", "last"), limit_years = c(-Inf, 0),
    date_varname = NULL, idcolname = "anonpatid", datecolname = "eventdate",
    indexcolname = "indexdate", overwrite = FALSE, description = NULL)
}

\arguments{
  \item{x}{
a \link{cohort} object which must have an index date whose name should be supplied as
the argument \code{indexcolname}.
}
  \item{varname}{
new variable name
}
  \item{data}{
ffdf or data.table containing patient ID, value and event date.
}
  \item{old_varname}{
variable name in data, default='value'
}
  \item{value_choice}{
a vector of values (e.g. categories),
with the highest priority first
(i.e. the element which will be
chosen in preference if there is more than
one on the chosen date)
OR a function which takes a vector of
values and returns a single value, e.g. mean, median,
max, min, any, all. Default is to choose the maximum
and ignore missing values.
}
  \item{date_priority}{
if multiple records for a patient, which
record to use based on date}
  \item{limit_years}{
a vector of length 2 for the time limits
(inclusive) in years before or after index date (lower limit is negative)
}
  \item{date_varname}{
optional name for date variable for the date
of the event from which the category was drawn.
Not valid if date_priority is 'any'
}
  \item{idcolname}{
name of the patient identifier column in \code{data}
}
  \item{datecolname}{
name of the event date column in \code{data}
}
  \item{indexcolname}{
name of the index date column in the cohort dataset \code{x}
}
  \item{overwrite}{
whether to overwrite the variable if it already
exists in cohort, or merely fill in missing values.
}
  \item{description}{
description for the new variable
}
}

\details{
Summarises the events of interest within the date range of interest,
and adds the result (one entry per patient) to the cohort data.table.
The cohort data.table is updated by reference.
}

\value{
Summary statistics of the new variable generated. For categorical
variables, this is a tabulation; for variables summarised using a 
function it is the mean, median, min, max, sd and N missing.
}

\seealso{
\code{\link{addCodelistToCohort}}
}

\examples{
COHORT <- cohort(data.table(anonpatid = 1:3, indexdate = as.IDate(c("2012-1-3", 
    "2012-1-2", "2010-1-9"))))
print(COHORT)

# Data in data.table
newdata <- data.table(anonpatid = c(2, 2, 3, 3, 4, 4, 4), 
    value = c(1, 2, 1, 3, 1, 2, NA), eventdate = as.IDate(c("2000-1-1", 
    "2012-1-3", "2011-1-1", "2011-1-1", "2012-1-5", "2013-1-1", 
    "2011-1-1")))
addToCohort(COHORT, "newvar", newdata, "value", value_choice = c(3, 
    1, 2))
print(COHORT)

# Data in FFDF
removeColumn(COHORT, 'newvar')
newffdf <- as.ffdf(newdata)
addToCohort(COHORT, "newvar", newffdf, "value", value_choice = c(3, 
    1, 2))
print(COHORT)

# Additional data for patient 1, missing data for patient 2
# Not over-writing but adding new values
newdata2 <- data.table(anonpatid = c(1, 1, 1, 1, 2, 2, 2), 
    value = c(1, 2, 1, 3, NA, NA, NA), eventdate = as.IDate(c("2000-1-1", 
    "2012-1-3", "2011-1-1", "2011-1-1", "2012-1-5", "2013-1-1", NA)))
addToCohort(COHORT, "newvar", newdata2, "value", value_choice = c(3, 
    1, 2))
print(COHORT)

# Now over-writing
addToCohort(COHORT, "newvar", newdata2, "value",
    value_choice = c(3, 1, 2), overwrite = TRUE)
print(COHORT)
}
