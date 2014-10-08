\name{CALIBERdatamanage-package}
\alias{CALIBERdatamanage-package}
\alias{CALIBERdatamanage}
\docType{package}

\title{
CALIBER data management tools
}

\description{
Tools for handling data in the GPRD, HES, ONS and MINAP linked dataset (CALIBER)
}

\details{
\tabular{ll}{
Package: \tab CALIBERdatamanage\cr
Type: \tab Package\cr
Version: \tab 0.1-10\cr
Date: \tab 2014-10-08\cr
License: \tab GPL-3\cr
}

If you are using this package for the first time, you are advised to read the 
\href{../doc/datamanage_example.html}{introduction and example}
and the 
\href{../doc/Rdatatable.html}{introduction to data.table}.

This package contains four sets of tools:

\describe{
\item{1. Importing data}{Functions to import single or multiple files to data.table or ffdf objects in R, with automatic unzipping of compressed files and conversion of dates, and applying lookups. (\code{\link{importDT}}, \code{\link{importFFDF}}, \code{\link{extractEntity}},  \code{\link{convertDates}}) 
}
\item{2. Building cohorts}{A 'cohort' S3 class to store information about a cohort,
  and functions for generating analysis variables from multiple row per patient data
(\code{\link{cohort}}, \code{\link{summary.cohort}}, \code{\link{addToCohort}}, \code{\link{addCodelistToCohort}})
}
\item{3. Presentation tables}{Producing summary tables in LaTeX or plain text, with functions
  to format numbers and percentages. (\code{\link{summaryTable}})
}
\item{4. Forest plot}{Producing forest plots using a spreadsheet template, including the facility to include several plots side by side, and specify the formatting of text. (\code{\link{multiforest}})
}
}

This package uses the \href{../doc/Rdatatable.html}{data.table} package extensively. Data tables can be modified by reference and are fast and efficient at handling large datasets. There are also functions to use ffdf data frames, which allow huge datasets to be stored in a temporary folder on the hard disk but appear as R objects in the workspace.

The package includes tools for date conversion in CALIBER files and tools for selecting values of a repeat measure or a diagnosis for patients within a particular time window.

The CALIBERlookups package, if installed, can provide lookup tables for the function \code{\link{extractEntity}}. The CALIBERcodelists package is useful for creating codelists, but is not required for this package to work.
}

\author{
Anoop Shah
}

\references{
Denaxas et al. Data Resource Profile: Cardiovascular disease research using linked bespoke studies and electronic health records (CALIBER).  Int. J. Epidemiol. (2012) 41 (6): 1625-1638. doi: 10.1093/ije/dys188 
\url{http://ije.oxfordjournals.org/content/41/6/1625}
}

\keyword{ package }

\examples{
# A sample patient cohort file
mycohort <- cohort(data.table(anonpatid = 1:3,
    indexdate = c('2010-01-01', '2009-03-05', '2008-05-06'),
    deathdate = c(NA, '', '2009-09-08'),
    ethnic_hes = c('Black', 'White', 'Indian')))
convertDates(mycohort)
print(mycohort)

# A sample data file with repeat measures for some patients
mydata <- data.table(anonpatid = c(2, 2, 3),
    eventdate = as.IDate(c('2006-01-01', '2008-01-01', '2005-01-01')),
    data1 = c(1, 2, 3))

# Copy the index dates and ethnicity to the repeated measures file.
transferVariables(mycohort, mydata, c('indexdate', 'ethnic_hes'))
print(mydata)

# Now use them to do a calculation on the repeated measures.
mydata[, temp:= ifelse(ethnic_hes == 'White', data1, 2)]

# Select a summary measure using addToCohort
addToCohort(mycohort, 'newvar', data = mydata,
    old_varname = 'temp', value_choice = c(2, 1))
print(mycohort)
}
