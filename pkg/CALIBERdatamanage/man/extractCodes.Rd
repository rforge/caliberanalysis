\name{extractCodes}
\alias{extractCodes}

\title{
Extract subset of data according to medcode (Read code), OPCS, ICD-10 or product code
}

\description{
This function uses a codelist to extract a subset of the data, and adds a column with a category for each record, according to the codelist. The input data can be either FFDF or data.table. The code column can be either factor or character, and when extracting by ICD-10 codes any characters after the code (e.g. X, -) are ignored. When extracting OPCS codes, dots in the code are ignored.
}

\usage{
extractCodes(data, codelist, enttypes = NULL,
    codename = switch(attr(codelist, 'Source'),
    GPRD = 'medcode', ONS = 'cod', HES = 'icd', OPCS = 'opcs',
    GPRDPROD = 'prodcode'), 
    varname = attr(codelist, 'Name'))
}

\arguments{
  \item{data}{
data.table or FFDF data frame from which to extract a subset.
}
  \item{codelist}{
codelist containing medcodes, ICD-10 codes, OPCS codes or product codes to extract.
}
  \item{enttypes}{
vector of entity types to extract, NULL to extract all entity types.
Only relevant for GPRD Read codelists.
}
  \item{codename}{
name of the code column in data.
}
  \item{varname}{
new variable name, which must not be already present in data. If not supplied in either the codelist or as an argument, the new category variable is named 'category'.
}
}

\value{
Either a data.table or ffdf, depending on the format of the original data.
The new column named varname is a factor with levels given by the category labels (shortnames) in the codelist.
}

\author{
Anoop Shah
}


\seealso{
\code{\link{addCodelistToCohort}}, \code{\link{addToCohort}}, \code{\link{extractEntity}}
}

\examples{
# Obtain test data
data(test_data)
TESTDT <- data.table(test_data)
convertDates(TESTDT)
TESTFFDF <- as.ffdf(TESTDT)

# Create a fake codelist
mycodelist <- data.table(medcode = c(2, 4, 6, 8), category = c(1, 3, 1, 3))
setattr(mycodelist, 'class', c('codelist', 'data.table', 'data.frame'))
setattr(mycodelist, 'Name', 'test_codelist_gprd')
setattr(mycodelist, 'Source', 'GPRD')
setattr(mycodelist, 'Categories',
    data.table(category = c(1, 3), shortname = c('This', 'That')))

# Extract subset from FFDF
extractCodes(TESTFFDF, mycodelist)
extractCodes(TESTFFDF, mycodelist, enttypes = c(1, 5))
# Extract subset from data.table
extractCodes(TESTDT, mycodelist)
extractCodes(TESTDT, mycodelist, enttypes = c(1, 5))

# View the structure to check that the integers behind the new variable are
# the same as the original categories 
# (i.e. categories 1 and 3, with 2 is missed out)
str(extractCodes(TESTDT, mycodelist, enttypes = c(1, 5)))

# Trial using ICD-10 data
mycodelist <- data.table(code = c('I20', 'I21'), category = c(1, 3))
setattr(mycodelist, 'class', c('codelist', 'data.table', 'data.frame'))
setattr(mycodelist, 'Name', 'test_codelist_hes')
setattr(mycodelist, 'Source', 'HES')
setattr(mycodelist, 'Categories',
    data.table(category = c(1, 3), shortname = c('This', 'That')))

mydata <- data.frame(icd = c('I201', 'I230X', 'I21-'), anonpatid = 1:3)
TESTFFDF <- as.ffdf(mydata)
TESTDT <- as.data.table(mydata)
extractCodes(TESTFFDF, mycodelist)
extractCodes(TESTDT, mycodelist)

# Trial using OPCS data
mycodelist <- data.table(code = c('K.123', 'K.234'), category = c(1, 3))
setattr(mycodelist, 'class', c('codelist', 'data.table', 'data.frame'))
setattr(mycodelist, 'Name', 'test_codelist_opcs')
setattr(mycodelist, 'Source', 'OPCS')
setattr(mycodelist, 'Categories',
    data.table(category = c(1, 3), shortname = c('This', 'That')))

mydata <- data.frame(opcs = c('K.123', 'K234', 'K000'), anonpatid = 1:3)
TESTFFDF <- as.ffdf(mydata)
TESTDT <- as.data.table(mydata)
extractCodes(TESTFFDF, mycodelist)
extractCodes(TESTDT, mycodelist)
}
