\name{test_lookups}
\alias{test_lookups}
\docType{data}

\title{
Sample lookups table
}

\description{
A subset of the CPRD lookup tables, for use with the \code{\link{extractEntity}} function. It is loaded using \code{data()} so the character strings are imported as factors.
}

\usage{data(test_lookups)}
\format{
  A data frame with 26 observations on the following 3 variables.
  \describe{
    \item{\code{lookup}}{3-letter code for the lookup table, a factor with levels \code{CER} \code{OPR} \code{POP} \code{RCT} \code{SEV} \code{SUM} \code{TQU} \code{YND}}
    \item{\code{category}}{category code,  a numeric vector}
    \item{\code{description}}{a factor with levels \code{<} \code{<=} \code{=} \code{\%} \code{Adverse Effect} \code{Age and race based} \code{Age based} \code{Allergy} \code{Data Not Entered} \code{Intolerance} \code{Mild} \code{Minimal} \code{No} \code{No Data Entered} \code{Tentative} \code{Unlikely} \code{Yes}}
  }
}

\details{
This is in a similar format to the CALIBER_LOOKUPS table in the CALIBERlookups package.
}

\source{
Based on the CPRD GOLD lookups (\url{www.cprd.com}).
}

\examples{
data(test_lookups)
}

\keyword{datasets}
