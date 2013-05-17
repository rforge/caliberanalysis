\name{categories}
\alias{retrieveCategories}
\alias{saveCategories}
\alias{addCategory}
\alias{removeUnusedCategories}

\title{
Get and set categories table for the master dictionary
}
\description{
These functions retrieve or save the categories table to/from the META table (which is inside the CALIBERcodelists namespace),
or retrieve the categories table from a codelist.
}
\usage{
retrieveCategories(codelist = NULL)
saveCategories(numbers, shortnames = NULL, descriptions = NULL,
	cattable = NULL, codelist = NULL)
addCategory(number, shortname, description = "",
	codelist = NULL)
removeUnusedCategories(codelist = NULL)
}

\arguments{
  \item{codelist}{codelist to retrieve categories from, or save/retrieve to/from META if NULL.}
  \item{numbers}{integer vector of category numbers for the entire categories table}
  \item{shortnames}{character vector of short category descriptions for the entire categories table}
  \item{descriptions}{character vector of full category descriptions for the entire categories table}
  \item{cattable}{a data.table containing the new categories table with columns:
category, shortname, description (i.e. in the same format as returned by \code{retrieve_categories})}
  \item{number}{a single new category number}
  \item{shortname}{character vector of a new category shortname to add}
  \item{description}{character vector of a new full category description to add}
}
\details{
\code{saveCategories} saves the categories to META, in the form of three strings which are elements in the 'value' column alongside the  'item' entries: category, shortname and description (see \code{\link{META}}).

\code{removeUnusedCategories} removes unused category levels from a codelist, or from META if no codelist is given.
}

\value{
\code{retrieveCategories} returns a data.table containing the category numbers, shortnames and descriptions. \code{saveCategories} invisibly returns the subset of META containing the category table.
\code{removeUnusedCategories} invisibly returns the modified codelist or the subset of META containing the category table.
}

\seealso{
\code{\link{as.codelist}},  \code{\link{assigncat}}
}


\examples{
retrieveCategories()

saveCategories(numbers=c(1, 2),
	shortnames=c('SA', 'UA'),
	descriptions=c('Stable angina', 'Unstable angina'))
}
