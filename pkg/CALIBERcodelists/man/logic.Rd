\name{Logic}
\alias{\%OR\%}
\alias{\%AND\%}
\alias{NOT}

\title{Logical operators for selection objects}

\description{\%OR\% is a version of | (the OR binary operator) which automatically invokes \code{\link{as.selection}}
to coerce its argument to a selection (e.g. a character string is used by \code{\link{termhas}} to select terms).

\%AND\% is a version of & (the AND binary operator) which acts similarly, and NOT is a function which converts
its argument to a selection and then negates it.}

\usage{
cond1 \%OR\% cond2
cond1 \%AND\% cond2
NOT(cond1)
}

\arguments{
  \item{cond1}{a selection object, or a character string which will be converted to a selection using \code{\link{termhas}}}
  \item{cond2}{another selection object or character string}
}

\value{
A selection object.
}

\seealso{
S3 Boolean operators for codelist objects: \code{\link{!.selection}}, \code{\link{|.selection}}, \code{\link{&.selection}}
}

\examples{
setdictionary('icd10')
as.selection('angina') \%OR\% as.selection('myocardial infarction')
my1 <- as.selection('angina')  \%AND\% as.selection('unstable')
my2 <- 'angina' \%AND\% 'unstable'
identical(my1, my2)
'angina' \%AND\% NOT('unstable')
}
