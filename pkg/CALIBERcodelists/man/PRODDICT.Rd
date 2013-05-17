\name{CALIBER_PRODDICT}
\alias{CALIBER_PRODDICT}

\title{
CPRD (GPRD) Product dictionary
}

\description{
CPRD Pegasus Product (drug) dictionary
}

\details{
  A data frame with observations on the following 11 variables.
  \describe{
    \item{\code{prodcode}}{GPRD product code, a numeric vector}
    \item{\code{multilex}}{Multilex product code, a character vector. Mapped to the \code{code} column in a drug codelist.}
    \item{\code{events}}{Number of events in GPRD, a numeric vector}
    \item{\code{prodname}}{Generic or proprietary product name, a character vector. Mapped to the \code{term} column in a drug codelist.}
    \item{\code{drugsubstance}}{Drug substance, a character vector}
    \item{\code{strength}}{Dosage a character vector}
    \item{\code{formulation}}{Drug formulation (tablets, liquid etc.), a character vector}
    \item{\code{route}}{Route of administration, a character vector}
    \item{\code{bnfcode}}{BNF codes that the drug belongs to, separated by /, a character vector}
    \item{\code{bnfheader}}{Text BNF header, a character vector}
    \item{\code{db}}{Database date, a character vector}
  }

For decoding GPRD therapy files.
}

\note{
For CALIBER users, the tables are included in the CALIBERlookups package.
External users need to obtain their own copies of the dictionaries
and generate the data file using the supplied script \code{demo(loadnewdicts)}.
}

\source{
Clinical Practice Research Datalink, \url{http://www.cprd.com}.
}

\examples{
# data(CALIBER_PRODDICT)
# print(attr(CALIBER_PRODDICT, 'VERSION'))
}
