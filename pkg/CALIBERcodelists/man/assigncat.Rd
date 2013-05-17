\name{assigncat}
\alias{assigncat}

\title{
Assign selected terms to a category
}

\description{
Assigns a set of terms to a category in the master dictionary (\code{\link{CALIBER_DICT}}), and adds a category description to the category table. 
}

\usage{
assigncat(number, definition = NULL, logic=NULL,
cats_to_convert=NULL, dictionary=NULL, codes=NULL)
}

\arguments{
  \item{number}{
a number for the new category being allocated.
Only terms in one of the dictionaries currently being used (as set by \code{\link{setdictionary}}) can be assigned.
It can be any integer, but the convention is for -1 to be used for terms not selected
but with similar codes to a selected term, 0 for excluded terms,
and positive integers for codes actually included in the codelist.
When exporting codelists from the master dictionary using \code{\link{exportall}},
only categories with positive integers are included.
}
  \item{definition}{
Category description. A short and long form of the description can be given separated by |
(e.g. 'MI|myocardial infarction'), in which case the short form will be listed in the exported CSV
codelist and both forms will be in the HTML documentation.
}
  \item{logic}{
a selection object, as returned by one of the functions \code{\link{termhas}}, \code{\link{codematch}},
or a number of such functions combined using operators such as \code{\link{\%AND\%}}) or \code{\link{\%OR\%}}.
If \code{logic} is NULL or not given, an entry is made in the \code{categories} table but no codes
are allocated to this category.
}
  \item{cats_to_convert}{
An integer vector stating which previously assigned categories are permitted to be changed to the new category.
If this is blank, any terms can be changed to the new category regardless of their existing category.
If it is set to a range of previously assigned categories, only these categories can be changed by the new allocation.
This might be useful to mark previously selected terms for exclusion whilst ignoring terms meeting the exclusion
criteria if they were not previously included. NA can be included in the vector of \code{cats_to_convert}
if terms with no category can also be assigned to the new category.
}
\item{dictionary}{To be used with \code{codes}, for assigning codes in a specified dictionary. It should be either a single string (e.g. 'read') or a character vector as long as \code{codes}.}
\item{codes}{To be used with \code{dictionary}, a character vector of codes to assign to the specified category. These must be the exact codes, wildcards or partial matches are not accepted.}
}

\details{
The function \code{\link{setdictionary}} must be called before \code{assigncat} to specify which terms are eligible for selection and to initialise the categories table.

Note that it is not currently possible to add categories to a codelist using this method.
Codelists are intended to built using the master dictionaries and then exported in their final form.
However, the modifications can be created in a new codelist and the codelists combined using
\code{\link{merge.codelist}}.
}

\value{
Returns the \code{categories} table invisibly. Prints a message stating the number of codes assigned to the category.
}

\note{
A script for generating a codelist with many categories may have multiple \code{assigncat} statements. Subsequent category allocations overwrite previous ones, so it is important to consider the order carefully in case some selected terms fulfil the criteria for more than one category.
}

\seealso{
\code{\link{as.codelist}},  \code{\link{codematch}}, \code{\link{dictis}}, \code{\link{explode}}, \code{\link{getdictionary}}, \code{\link{setdictionary}}, \code{\link{termhas}}

To combine codelists: \code{\link{merge.codelist}}

To compare codelists: \code{\link{compare}}

To convert codelists from one dictionary to another: \code{\link{convert}}
}

\examples{
## Setting up a codelist for cardiomyopathy terms
setdictionary('read')
mycodelist <- as.codelist(termhas('cardiomyopathy'))
mycodelist[, category:=1L]
print(mycodelist)
print(as.codelist('read'))

## Assigning category using a selection
assigncat(2, 'hcm|hypertrophic cardiomyopathy', termhas('hyper*cardiomyopath'))

## Assigning category using Read codes
assigncat(3, 'hcm|hypertrophic cardiomyopathy',
  dictionary='read', codes=c('12CJ.00', '12CR.00'))

## Show the selection
print(as.codelist())
}


