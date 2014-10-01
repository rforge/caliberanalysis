\name{addCodelistToCohort}
\alias{addCodelistToCohort}

\title{
Adds information from categories based on a codelist
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
addCodelistToCohort(cohort, varname, data, codelist,
    categories = NULL, enttypes = NULL,
    codename = switch(attr(codelist, 'Source'),
    GPRD = 'medcode', ONS = 'cod', HES = 'icd', OPCS = 'opcs',
    GPRDPROD = 'prodcode'), binary = FALSE,
    limit_years = c(-Inf, 0),  idcolname = attr(cohort, 'idcolname'),
    datecolname = c('eventdate', 'evdate', 'dod', 'epistart',
    'admidate'), indexcolname = 'indexdate',
    overwrite = TRUE, description = NULL, limit_days = NULL,
    entities = 'all')
}

\arguments{
  \item{cohort}{
a cohort object
}
  \item{varname}{
new variable name
}
  \item{data}{
ffdf or data.table containing anonpatid, medcode and eventdate
and enttype if entity type is specified
}
  \item{codelist}{
a codelist object or the name of a codelist file for selection of events by medcode.
}
  \item{categories}{
vector of categories to use, in priority order
(highest priority first). If the result is
binary, the order of categories does not matter.
Leave as NULL (default) to include all categories.
}
  \item{enttypes}{
numeric vector of entity types to use, default = NULL (use all)
}
  \item{codename}{
name of the column in data containing the relevant medical code
}
  \item{binary}{
whether to lump all categories together to make a binary variable
}
  \item{limit_years}{
earliest and latest year relative to index date. Default is c(-Inf, 0),
which selects all events prior to or on the index date.
}
  \item{idcolname}{
name of the patient identifier column in \code{data}
}
  \item{datecolname}{
name of the event date column in \code{data}. If multiple options are
given, they are tried in turn to find a column that contains dates.
}
  \item{indexcolname}{
name of the index date column in the cohort dataset \code{x}
}
  \item{overwrite}{
whether to overwrite this variable if it exists
}
  \item{description}{
description for the new variable. Defaults to the function call which generated this variable.
}
  \item{limit_days}{
a vector of length 2 for the time limits, which over-rules limit_years if both are supplied.
A year is considered to be 365.25 days.
}
  \item{entities}{
deprecated way to specify which entity types to use, use enttypes instead.
}
}

\details{
The function first merges the codelist onto data, to select events
with medcodes in the relevant categories and entity types. The subset of 
relevant events is then used in a call to \link{addToCohort}, to select a category
if it occurs within the defined time window relative to each patient's
index date. The final variable is added to the cohort data.table.
}

\value{
Cohort with a extra column(s). If \code{cohort} is a data.table,
it is also modified by reference.
}

\seealso{
\code{\link{addToCohort}}
}

\examples{
COHORT <- cohort(data.table(anonpatid = 1:3,
    indexdate = as.IDate(c("2012-1-3", "2012-1-2", "2010-1-9"))))
print(COHORT)

# New data (existing category column should be ignored)
newdata <- data.table(anonpatid = c(2, 2, 3, 3, 4, 4, 4), 
    medcode = c(1, 2, 2, 3, 1, 2, NA), eventdate = as.IDate(c("2000-1-1", 
    "2012-1-3", "2011-1-1", "2011-1-1", "2012-1-5", "2013-1-1", 
    "2011-1-1")), category = rep(2, 7))

# Create a sample codelist
mycodelist <- data.table(medcode = 1:4,
	code = c('246..00', '22A..00', '229..00', '423..00'),
    term = c('O/E - weight', 'O/E - blood pressure reading',
    	'O/E - height', 'Haemoglobin estimation'), category = c(1, 2, 2, 2))
setattr(mycodelist, 'Source', 'GPRD')
setattr(mycodelist, 'Categories', data.table(category = 1:2,
    shortname = c('Weight', 'Other')))
class(mycodelist) <- c('codelist', 'data.table', 'data.frame')

# Using data.table, categories 1 or 2 (1 priority)
addCodelistToCohort(COHORT, varname = "newvar", data = newdata,
    codelist = mycodelist, categories = 1:2)
print(COHORT)
removeColumns(COHORT, 'newvar')

# Using ffdf
newffdf <- as.ffdf(newdata)
FFDFCOHORT <- as.ffdf(COHORT)

# Category 1 only
addCodelistToCohort(COHORT, varname = "V_1", data = newffdf,
    codelist = mycodelist, categories = 1)

# Category 2 or 1
addCodelistToCohort(COHORT, varname = "V_12", data = newffdf,
    codelist = mycodelist, categories = 2:1)

# Binary
addCodelistToCohort(COHORT, varname = "V_binary", data = newffdf,
    codelist = mycodelist, categories = 1:2, binary = TRUE)

# Category 2 or 1, no time limits
addCodelistToCohort(COHORT, varname = "V_12anytime", data = newffdf,
    codelist = mycodelist, categories = 2:1, limit_years = c(-Inf, Inf))
print(COHORT)

# Using FFDF cohort; need to reassign the result to the 
# cohort object using <-
# Category 1 only
FFDFCOHORT <- addCodelistToCohort(FFDFCOHORT, varname = "V_1", data = newffdf,
    codelist = mycodelist, categories = 1)

# Category 2 or 1
FFDFCOHORT <- addCodelistToCohort(FFDFCOHORT, varname = "V_12",
    data = newffdf, codelist = mycodelist, categories = 2:1)
print(FFDFCOHORT)
}
