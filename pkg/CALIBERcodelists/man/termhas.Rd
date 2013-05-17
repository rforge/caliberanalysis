\name{termhas}
\alias{termhas}

\title{
Select terms which contain a particular string
}
\description{
These functions are used in statements to assign categories. They return a Boolean vector stating whether each term in the dictionary contains the regular expression text of interest or not, AND is in one of the dictionaries in use (icd10, opcs, read).
}
\usage{
termhas(regexpr, exact = FALSE, ignorecase = TRUE)
}

\arguments{
  \item{regexpr}{
a search phrase, which can use POSIX regular expressions.
}
  \item{exact}{
Whether to search for an exact term instead of using regular expression matching.
}
  \item{ignorecase}{
Whether to use case sensitive matching; the default is to ignore case.
}
}

\value{
a selection object.
}

\seealso{
\code{\link{as.codelist}},  \code{\link{assigncat}}, \code{\link{codematch}}, \code{\link{dictis}}, \code{\link{explode}},
\code{\link{getdictionary}}, \code{\link{setdictionary}}
}

\examples{
setdictionary('opcs')
termhas('bronchoscop')
termhas('stab.*angina')
}
