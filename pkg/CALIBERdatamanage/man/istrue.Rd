\name{istrue}
\alias{istrue}
\alias{isfalse}

\title{
Whether a vector is TRUE or FALSE, ignoring missing values
}

\description{
Use \code{istrue()} as a wrapper for conditional statements which might
be missing if one of the components is missing, but for which you want
a missing value to be interpreted as FALSE.
}

\usage{
istrue(x)
isfalse(x)
}

\arguments{
  \item{x}{
logical expression to evaluate.
}
}

\details{
A convenience function to avoid unwanted NA's.
}

\value{
A logical vector containing TRUE or FALSE, with no missing values.
}

\examples{
istrue(c(TRUE, FALSE, NA))
# TRUE FALSE FALSE
isfalse(c(TRUE, FALSE, NA))
# FALSE TRUE FALSE
}
