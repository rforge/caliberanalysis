\name{GEN_SDCtoDate}
\alias{GEN_SDCtoDate}
\alias{YYYYMMDDtoDate}

\title{
Convert CPRD special date formats to IDate
}

\description{
Converts GEN_SDC and YYYYMMDD dates in the additional data area of the
CPRD clinical and test tables from the CPRD GOLD format to IDate.
The YYYYMMDD format can be
used for imprecise dates where the day or month are unknown; these are filled in 
using default values.
}

\usage{
GEN_SDCtoDate(GEN_SDC)
YYYYMMDDtoDate(YYYYMMDD, default_day_month = 15, 
    default_month_year = 7, default_day_month_year = 1, 
    valid_lower_year = 1800, valid_upper_year = 2050)
}

\arguments{
  \item{GEN_SDC}{
a numeric or integer vector of dates coded in GEN_SDC format.
}
  \item{YYYYMMDD}{
a numeric or integer vector of dates coded in YYYYMMDD format.
}
  \item{default_day_month}{
default day of the month to use for dates in year-month format.
}
  \item{default_month_year}{
default month of the year to use if only the year is given.
}
  \item{default_day_month_year}{
default day of the month to be used if only the year is given.
}
  \item{valid_lower_year}{
lower year limit for valid dates.
}
  \item{valid_upper_year}{
upper year limit for valid dates.
}
}

\details{
The GEN_SDC date in dd/mm/yyyy format can be obtained as follows:

\tabular{ll}{
0 \tab An invalid / missing date \cr%
2 \tab A date greater than 31/12/2014 \cr%
3 \tab A date earlier than 01/01/1800 \cr%
All other values \tab number of days between the date and the 31/12/2014 offset by 10. \cr
}

Example: A value of 4027 decodes to the date 01/01/2004. 4027-10 = 4017 days prior to the date 31/12/2014 is the date 01/01/2004

The YYYYMMDD format is an integer with 4-8 digits specifying the year with optional month and day, ie. YYYY, YYYYMM or YYYYMMDD.

Example: A value of 2001 is the year 2001, a value of 2000107 is July 2001 and 20010706 is 6 July 2001.
}

\value{
An integer vector of dates in IDate format. Dates after 31/12/2014 or before 01/01/1800 (GEN_SDC) and any 
dates outside the range of valid_lower_year and valid_upper_year are converted to missing (NA).
}


\references{
Puri S. GPRD Gold Data Dictionary, Version 1.1. GPRD, MHRA, UK.
}

\seealso{
\code{\link{textToDate}}, \code{\link{convertDates}}
}

\examples{
##
GEN_SDCtoDate(c(0, 1, 2, 3, 4027))
YYYYMMDDtoDate(c(1987,198701, 19870507))
}
