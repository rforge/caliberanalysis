\name{test_entity}
\alias{test_entity}
\docType{data}

\title{
Sample subset of the CPRD entity table
}

\description{
A 5-row subset of the CALIIBER entity table, for testing the \code{\link{extractEntity}} function.
}

\usage{data(test_entity)}

\format{
  A data frame with 5 observations on the following 18 variables.
  \describe{
    \item{\code{enttype}}{entity type, a numeric vector}
    \item{\code{data_fields}}{number of data fields, a numeric vector}
    \item{\code{data1}}{description of entry in data1, a factor with levels \code{Diastolic} \code{Drug Code} \code{Operator} \code{Status}}
    \item{\code{data1_lkup}}{lookup table for data1, a factor with levels  \code{OPR} \code{Product Dictionary} \code{YND}}
    \item{\code{data2}}{description of entry in data2, a factor with levels \code{Cigarettes per day} \code{Reaction Type} \code{Systolic} \code{Units per week} \code{Value}}
    \item{\code{data2_lkup}}{lookup table for data2, a factor with levels  \code{RCT}}
    \item{\code{data3}}{description of entry in data3, a factor with levels \code{Cigars per day} \code{Korotkoff} \code{Severity} \code{Start Date} \code{Unit of measure}}
    \item{\code{data3_lkup}}{lookup table for data3, a factor with levels  \code{SEV} \code{SUM} \code{YYYYMMDD}}
    \item{\code{data4}}{description of entry in data4, a factor with levels  \code{Certainty} \code{Ounces of tobacco} \code{Qualifier} \code{Stop Date}}
    \item{\code{data4_lkup}}{lookup table for data4, a factor with levels  \code{CER} \code{TQU} \code{YYYYMMDD}}
    \item{\code{data5}}{description of entry in data5, a factor with levels  \code{Normal range from} \code{Read Code For Reaction} \code{Start Date}}
    \item{\code{data5_lkup}}{lookup table for data5, a factor with levels  \code{GEN_SDC} \code{Medical Dictionary}}
    \item{\code{data6}}{description of entry in data6, a factor with levels  \code{Normal range to} \code{Stop Date}}
    \item{\code{data6_lkup}}{lookup table for data6, a factor with levels  \code{GEN_SDC}}
    \item{\code{data7}}{description of entry in data7, a factor with levels  \code{Normal range basis}}
    \item{\code{data7_lkup}}{lookup table for data7, a factor with levels  \code{POP}}
  }
}

\source{
Based on the entity table in the CPRD GOLD lookup tables.
}

\examples{
data(test_entity)
}

\keyword{datasets}
