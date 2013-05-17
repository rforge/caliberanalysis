\name{selection}
\alias{selection}
\alias{is.selection}
\alias{as.selection}

\title{
selection: S3 class for a list of Read, OPCS and/or ICD-10 terms
}

\description{
This class is used in the generation of codelists (see the \code{\link{codelist}} class).
Selections differ from codelists in that they can contain terms from more than one
source dictionary, but they are Boolean (terms are included or not) without categories. Also they do not have any associated metadata, and are not allowed to have missing data.

The special Boolean operators \code{\link{\%AND\%}}, \code{\link{\%OR\%}} and \code{\link{NOT}} can be used to combine selections, automatically coercing their arguments to selections (e.g. using \code{\link{termhas}} if one of the arguments is a character string). They can also be combined using the conventional Boolean operators & (AND), | (OR) and ! (NOT), but the result will be a selection only if all the arguments are already selection objects.
}

\usage{
selection(x = NULL, category = NULL, categories = NULL)
as.selection(x, ...)
is.selection(x)
}

\arguments{
  \item{x}{
a codelist, character string or logical vector as long as the number of
rows in \code{\link{CALIBER_DICT}} to convert to a selection object. If it is
a character string, the terms are searched in a non-case-sensitive
manner using \code{\link{termhas}}.
}
  \item{category}{
if converting a codelist to a selection, a numeric vector of categories to
include in the selection.
}
  \item{categories}{
alternative spelling for the \code{category} argument.
}
  \item{\dots}{
other arguments to pass to \code{selection}.
}
}

\value{
a selection object.
}

\seealso{
\code{\link{print.selection}}.
Functions which return a selection: \code{\link{termhas}}, \code{\link{codematch}}
}

\examples{
setdictionary('opcs')

# generating selections using codes or terms
termhas('bronchoscop')
codematch('E5[01]', 'opcs')

# selection invokes termhas if the argument is a character string
selection('bronchoscop')

# the selection special Boolean operators automatically invoke
# termhas if necessary
termhas('bronchoscop') \%AND\% termhas('laser')
termhas('bronchoscop') & termhas('laser')

# if the argument to termhas is already a selection, 
# it is not modified
termhas('bronchoscop' \%AND\% 'laser')
'bronchoscop' \%AND\% 'laser'

'bronchoscop' \%AND\% NOT('diagnostic')
}
