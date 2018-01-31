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
addToCohort(cohort, varname, data, old_varname = "value",
    value_choice = function(x) max(x, na.rm = TRUE),
    date_priority = c("all", "first", "last"), limit_years = c(-Inf, 0),
    date_varname = NULL,  idcolname = attr(cohort, 'idcolname'),
    datecolname = "eventdate", indexcolname = "indexdate",
    overwrite = TRUE, description = NULL, limit_days = NULL)
}

\arguments{
  \item{cohort}{
a \link{cohort} object which must have an index date whose name should be supplied as the argument \code{indexcolname}.
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
\code{value_choice = function(x) TRUE} can be used
to return TRUE for any rows that match.
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
name of the patient identifier column in \code{data}, default
is the ID column in the cohort.
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
description for the new variable. Defaults to the function call which generated this variable.
}
  \item{limit_days}{
a vector of length 2 for the time limits, which over-rules limit_years if both are supplied.
A year is considered to be 365.25 days.
}
}

\details{
Summarises the events of interest within the date range of interest,
and adds the result (one entry per patient) to the cohort.

Summary statistics of the new variable are displayed. For categorical
variables, this is a tabulation; for variables summarised using a 
function it is the mean, median, min, max, sd and N missing.

The functions \code{\link{addCodelistToCohort}} and
\code{\link{addCategoryToCohort}} are convenience functions for adding
particular types of variable, particularly for generating binary variables
(e.g. presence of a particular code or category within a time period for
each patient). They call \code{\link{addToCohort}} with date_priority = "all"
and take a codelist or a vector of categories as an argument.
}

\value{
Cohort with extra column(s). If \code{cohort} is a data.table,
it is also modified by reference.
}

\seealso{
\code{\link{addCodelistToCohort}}, \code{\link{addCategoryToCohort}}
}

\examples{
COHORT <- cohort(data.table(anonpatid = 1:3, indexdate = as.IDate(c("2012-1-3", 
    "2012-1-2", "2010-1-9"))))
print(COHORT)

# Data in data.table
newdata <- data.table(anonpatid = c(2, 2, 3, 3, 4, 4, 4), 
    value = c(1, 2, 1, 3, 1, 2, NA), eventdate = as.IDate(c("2000-1-1", 
    "2012-1-2", "2011-1-1", "2011-1-1", "2012-1-5", "2013-1-1", 
    "2011-1-1")))
addToCohort(COHORT, "newvar", newdata, "value", value_choice = c(3, 
    1, 2))
print(COHORT)

# Data in FFDF
removeColumns(COHORT, 'newvar')
newffdf <- as.ffdf(newdata)
addToCohort(COHORT, "newvar", newffdf, "value", value_choice = c(3, 
    1, 2))
print(COHORT)

# Exact date matches only
removeColumns(COHORT, 'newvar')
addToCohort(COHORT, "newvar", newffdf, "value", value_choice = c(3, 
    1, 2), limit_days = c(0, 0))
print(COHORT)

# Additional data for patient 1, missing data for patient 2
# Not over-writing but adding new values
newdata2 <- data.table(anonpatid = c(1, 1, 1, 1, 2, 2, 2), 
    value = c(1, 2, 1, 3, NA, NA, NA), eventdate = as.IDate(c("2000-1-1", 
    "2012-1-3", "2011-1-1", "2011-1-1", "2012-1-5", "2013-1-1", NA)))
addToCohort(COHORT, "newvar", newdata2, "value", value_choice = c(3, 
    1, 2))
print(COHORT)

# Now over-writing previous column
addToCohort(COHORT, "newvar", newdata2, "value",
    value_choice = c(3, 1, 2), overwrite = TRUE)
# TRUE if exists
addToCohort(COHORT, "trueifexists", newdata2, "eventdate",
    value_choice = function(x) TRUE)
print(COHORT)


# Trials with FFDF cohort
# Need to assign the modified cohort to a name using <-
# (ffdf is not updated by reference)

COHORT <- cohort(data.table(anonpatid = 1:3, indexdate = as.IDate(c("2012-1-3", 
    "2012-1-2", "2010-1-9"))))
COHORT <- as.ffdf(COHORT)
print(COHORT)

# Data in data.table
COHORT <- addToCohort(COHORT, "newvar", newdata, "value",
    value_choice = c(3, 1, 2))
print(COHORT)

# Data in FFDF
COHORT <- removeColumns(COHORT, 'newvar')
COHORT <- addToCohort(COHORT, "newvar", newffdf, "value",
    value_choice = c(3, 1, 2))
print(COHORT)
}
