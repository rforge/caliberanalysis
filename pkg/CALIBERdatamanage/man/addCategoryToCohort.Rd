\name{addCategoryToCohort}
\alias{addCategoryToCohort}

\title{
Creates one row per patient from repeated categorical data
}

\description{
Adds to the cohort data.table a column labelled varname
containing the value of a category from a list of anonpatid,
category, eventdate. If a patient has
more than one record, the category of choice is chosen according to
an order of priority, or a binary outcome is generated if the patient
has any records fulfilling the criteria.
}

\usage{
addCategoryToCohort(cohort, varname, data, old_varname = 'category',
    categories, binary = FALSE,
    limit_years = c(-Inf, 0),  idcolname = attr(cohort, 'idcolname'),
    datecolname = 'eventdate', indexcolname = 'indexdate',
    overwrite = TRUE, description = NULL)
}

\arguments{
  \item{cohort}{
a cohort object
}
  \item{varname}{
new variable name
}
  \item{data}{
ffdf or data.table containing patient identifier, eventdate and \code{old_varname}. 
}
  \item{old_varname}{
the column name containing the categories of interest
}
  \item{categories}{
vector of categories to use, in priority order
(highest priority first). For each patient,
all records within limit_years of the
index date are searched for the highest priority category first, then
the next highest etc. 

If the result is
binary, the order of categories does not matter, and the function
returns TRUE / FALSE according to whether the patient has any
records of the categories of interest within limit_years of the index date.
}
  \item{binary}{
whether to lump all categories together to make a binary variable (TRUE / FALSE).
If there are no records for a patient the result is FALSE, not missing.
}
  \item{limit_years}{
earliest and latest year relative to index date. Default is c(-Inf, 0),
which searches all events prior to or on the index date.
}
  \item{idcolname}{
name of the patient identifier column in \code{data}
}
  \item{datecolname}{
name of the event date column in \code{data}
}
  \item{indexcolname}{
name of the index date column in the cohort dataset
}
  \item{overwrite}{
whether to overwrite this variable if it exists
}
  \item{description}{
description for the new variable. Defaults to the function call which generated this variable.
}
}

\details{
This is a convenience function which calls \code{\link{addToCohort}} and
then converts the output to a TRUE / FALSE variable (where lack of an entry is 
converted to FALSE instead of a missing value).

The function selects events with the relevant categories and entity types. The subset of 
relevant events is then used in a call to \link{addToCohort}, to select a category
if it occurs within the defined time window relative to each patient's
index date. The final variable is added to the cohort data.table.
}

\value{
Cohort with a extra column(s). If \code{cohort} is a data.table,
it is also modified by reference.
}

\seealso{
\code{\link{addToCohort}}, \code{\link{addCodelistToCohort}}
}

\examples{
COHORT <- cohort(data.table(anonpatid = 1:3,
    indexdate = as.IDate(c("2012-1-3", "2012-1-2", "2010-1-9"))))
print(COHORT)

# New data
newdata <- data.table(anonpatid = c(2, 2, 3, 3, 4, 4, 4), 
    medcode = c(1, 2, 2, 3, 1, 2, NA), eventdate = as.IDate(c("2000-1-1", 
    "2012-1-3", "2011-1-1", "2011-1-1", "2012-1-5", "2013-1-1", 
    "2011-1-1")), category = c(1, 2, 1, 3, 2, 3, 4))

# Using data.table, categories 1 or 2 (1 priority)
addCategoryToCohort(COHORT, varname = "newvar", data = newdata,
    categories = 1:2)
print(COHORT)
removeColumns(COHORT, 'newvar')

# Using ffdf
newffdf <- as.ffdf(newdata)
FFDFCOHORT <- as.ffdf(COHORT)

# Category 1 only
addCategoryToCohort(COHORT, varname = "V_1", data = newffdf,
    categories = 1)

# Category 2 or 1
addCategoryToCohort(COHORT, varname = "V_12", data = newffdf,
    categories = 2:1)

# Binary
addCategoryToCohort(COHORT, varname = "V_binary", data = newffdf,
    categories = 1:2, binary = TRUE)

# Category 2 or 1, no time limits
addCategoryToCohort(COHORT, varname = "V_12anytime", data = newffdf,
    categories = 2:1, limit_years = c(-Inf, Inf))
print(COHORT)

# Using FFDF cohort; need to reassign the result to the 
# cohort object using <-
# Category 1 only
FFDFCOHORT <- addCategoryToCohort(FFDFCOHORT, varname = "V_1",
    data = newffdf, categories = 1)

# Category 2 or 1
FFDFCOHORT <- addCategoryToCohort(FFDFCOHORT, varname = "V_12",
    data = newffdf, categories = 2:1)
print(FFDFCOHORT)
}
