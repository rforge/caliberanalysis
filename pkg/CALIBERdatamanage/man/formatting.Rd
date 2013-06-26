\name{formatting}
\alias{formatp}
\alias{formatnum}
\alias{formathr}
\alias{formatci}

\title{
Format numerical results for export tables
}

\description{
Functions to format numbers for publication, producing either plain text or LaTeX output. \code{formatnum} is the general function. \code{formatp} formats P values. \code{formathr} exponentiates and prints coefficients such as hazard ratios with confidence intervals, and displays significance levels with stars.
}

\usage{
formatnum(numbers, dp = NA, sigfig = 3, scientific = TRUE,
  latex = (getOption("xtable.type") == "latex"),
  usephantom = getOption("CALIBERdatamanage_phantom"), names = NULL)
formatp(pvalues, minp = 0.0001,
  latex=(getOption('xtable.type')=='latex'), ...)
formathr(coef, se, df = Inf, dp = NA, pstar = TRUE, ...)
formatci(range, scientific = FALSE, intsep = ", ", ...)
}


\arguments{
  \item{numbers}{
numeric vector of numbers to format
}
  \item{dp}{
number of decimal places
}
  \item{sigfig}{
number of significant figures
}
  \item{scientific}{
TRUE for scientific mode, FALSE for standard output
}
  \item{latex}{
TRUE for LaTeX output (math mode), FALSE, NULL or logical(0) for plain text output
}
  \item{usephantom}{
TRUE to pad the LaTeX output with blank (phantom) characters so that the decimal points line up
}
  \item{names}{
names for the output vector
}
  \item{pvalues}{
numeric vector of P value values to be formatted
}
  \item{minp}{
minimum P value to display as an exact value
}

  \item{coef}{
coefficient (log hazard ratio or log odds ratio)
}
  \item{se}{
standard error of coefficient
}
  \item{df}{
degrees of freedom for t-distribution; Inf to use the Normal distribution
}
  \item{pstar}{
TRUE to include stars for significance level: * < 0.05, ** < 0.01, *** < 0.001
}
  \item{range}{
numeric vector containing the upper and lower confidence limits
}
  \item{intsep}{
character string for the confidence interval separator; default is comma
}
  \item{\dots}{
other arguments to be passed to \code{formatnum}
}
}


\value{
a character vector containing formatted numbers
}

\author{
Anoop Shah
}


\seealso{
\code{\link{percent}}, \code{\link{npercent}}, \code{\link{percentConf}}, \code{\link{nmissing}}, \code{\link{meansd}}
}

\examples{
formatnum(1:5, latex = FALSE)
# [1] "1" "2" "3" "4" "5"
formatnum(1:5, latex = TRUE)
# [1] "$1$" "$2$" "$3$" "$4$" "$5$"
formatnum(c(0.00001, 0.02, 5.6, 67, 700), latex = FALSE, dp = 1)
# [1] "0.0"   "0.0"   "5.6"   "67.0"  "700.0"
formatnum(c(0.00001, 0.02, 5.6, 67, 700), latex = TRUE, usephantom = TRUE)
# [1] "$1\\times 10^{-5}$"                       
# [2] "\\phantom{0}\\phantom{0}$0.02$"           
# [3] "\\phantom{0}\\phantom{0}$5.6\\phantom{0}$"
# [4] "\\phantom{0}$67\\phantom{.}\\phantom{0}\\phantom{0}$"
# [5] "$700\\phantom{.}\\phantom{0}\\phantom{0}$"

# Formatting hazard ratios
formathr(0.8, 0.3, latex = FALSE)
# "2.23 (1.24, 4.01) **"
formathr(0.8, 0.3, latex = TRUE)
# "$2.23$ ($1.24$, $4.01$) **"

# Formatting confidence intervals
formatci(c(2.4343, 4.3245), latex = FALSE)
# "(2.43, 4.32)"
formatci(c(2.4343, 4.3245), latex = TRUE)
# "($2.43$, $4.32$)"
}