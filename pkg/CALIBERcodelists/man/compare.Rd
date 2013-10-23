\name{compare}
\alias{compare}
\alias{print.codelistComparison}

\title{
Compare two codelists
}

\description{
Compares one codelist against another, or compares a codelist against the selection in the master dictionary. Returns a list of differences and a set of instructions for updating the master dictionary from the older codelist to the newer one. 
}
\usage{
compare(oldlist, newlist = NULL, expandICD10 = TRUE, expandICD9 = TRUE)
\method{print}{codelistComparison}(x, ...)
}
\arguments{
\item{oldlist}{The first codelist for the comparison, ideally the older codelist. If the master dictionary is to be considered as \code{oldlist}, it can be supplied as the argument by coercing it to a codelist using e.g. \code{oldlist = as.codelist('read')} (replacing 'read' by 'icd10' or 'opcs' as appropriate).}
\item{newlist}{The second codelist to compare against, or NULL to compare against the master dictionary.}
\item{expandICD10}{TRUE or FALSE, whether to ensure that ICD-10 codelists are in the 'Expanded' form before doing the comparison. The codelists should both be in the same form before comparison, and the expanded form might give a more informative comparison. See \code{\link{expandCodelist}}.}
\item{expandICD9}{TRUE or FALSE, whether to ensure that ICD-9 codelists are in the 'Expanded' form before doing the comparison.}
\item{x}{a \code{codelistComparison} object as returned by the \code{compare} function.}
\item{\dots}{not used}
}

\details{
Missing categories and categories less than one are lumped together as 'Excluded' terms for the purposes of comparison.
}

\value{
A list object of class \code{codelistComparison}, with the following elements:
\item{same_terms}{whether all the terms are the same (considering terms with category 0
 and missing terms as equivalent)}
\item{same_attr}{whether all the attributes are the same}
\item{same_cattable}{whether all the category descriptions are the same}
\item{cattable_comparison}{data.table comparing category tables}
\item{attr_comparison}{data.table comparing attributes}
\item{terms_comparison}{data.table comparing old and new terms}
\item{message}{Comparison of terms, as reported by the \code{print} method}
\item{instructions}{R code for updating the master dictionary from oldlist to newlist}
\item{identical}{TRUE or FALSE according to whether the categories (ignoring 0),
category descriptions and other attributes are identical between the two codelists.}
}

\note{
The instructions can be parsed and evaluated by \code{\link{assigncat}}
to convert the selection of terms in the master dictionary from oldlist to newlist.
}

\seealso{
\code{\link{as.codelist}}, \code{\link{assigncat}}, \code{\link{contractCodelist}}, \code{\link{expandCodelist}}, \code{\link{is.codelist}}, \code{\link{print.codelist}}, \code{\link{subset.codelist}}
}

\examples{
setdictionary('read')
# Generate codelists
c1 <- as.codelist(termhas('angina|myocardial infarct'))
c2 <- as.codelist(termhas('myocard'))

# Generate a codelist with a category
assigncat(1, 'myocard', termhas('myocard'))
c3 <- as.codelist()

# Compare codelists
compare(c1, c3)
compare(c1, c2)
# Codelists c1 and c2 have no category assigned so they cannot be compared

# Use the data.table function to set a category for both codelists
# 1L means the number 1 as an integer rather than the real number 1.0000 ...
# This is to avoid a data.table warning, although c1[, category:=1] would also
# work. You can update a subset of categories like this:
# c1[readcode %like% '^12C', category:=2L]
c1[, category:=1L]
c2[, category:=1L]

# Now compare them again
compare(c1, c2)
compare(c2, c1)
compare(c1, c2)$identical
}
