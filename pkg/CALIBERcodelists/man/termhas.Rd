\name{termhas}
\alias{termhas}

\title{
Select terms which contain a particular string
}
\description{
Returns a selection object with terms matching the regular expression pattern.
}
\usage{
termhas(regexpr, exact = FALSE, ignorecase = TRUE)
}

\arguments{
  \item{regexpr}{
a single search pattern or vector of patterns, which can use POSIX regular expressions. 
If it is a vector of search patterns the results are combined using OR.
}
  \item{exact}{
whether to search for an exact term instead of using regular expression matching.
}
  \item{ignorecase}{
whether to use case sensitive matching; the default is to ignore case.
}
}

\value{
a \code{\link{selection}} object.
}

\seealso{
\code{\link{assigncat}}, \code{\link{codematch}}, \code{\link{dictis}}, \code{\link{medcodeis}}, \code{\link{explode}},
\code{\link{setdictionary}}
}

\examples{
setdictionary('opcs')
termhas('bronchoscop')
termhas('stab.*angina')
}
