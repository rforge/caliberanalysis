\name{summaryStats}
\alias{nmissing}
\alias{npercent}
\alias{percent}
\alias{meansd}
\alias{percentConf}

\title{
Calculate and format summary statistics
}

\description{
These functions are designed to be used when creating summary tables. There is an option to produce LaTeX output (math mode).
}

\usage{
nmissing(x, ...)
percent(logicalvector, dp = 1,
    latex = (getOption("xtable.type") == "latex"))
npercent(logicalvector, ...)
percentConf(logicalvector, ...)
meansd(x, ...)
}

\arguments{
  \item{x}{
a numeric vector (missing values are ignored)
}
  \item{logicalvector}{
a logical vector (missing values are ignored)
}
  \item{dp}{
number of decimal places
}
  \item{latex}{
TRUE for LaTeX output (math mode), FALSE, NULL or logical(0) for plain text output
}
  \item{\dots}{
other arguments to pass to \code{\link{formatnum}}
}
}

\value{
a character vector containing the formatted summary statistic. \code{percentConf} prints the percentage and a confidence interval using the binomial distribution, without percentage signs.
}


\author{
Anoop Shah
}

\seealso{
\code{\link{formatnum}}, \code{\link{formatp}}, \code{\link{formathr}}, \code{\link{formatci}}
}

\examples{
# Number and % missing
nmissing(c(1, 2, 3, NA, NA), latex = FALSE)
# "2 (40.0%)"
nmissing(c(1, 2, 3, NA, NA), latex = TRUE)
# "2 (40.0\\%)"

# Percentage
percent(c(TRUE, TRUE, FALSE, NA), latex = FALSE)
# "66.7%"
percent(c(TRUE, TRUE, FALSE, NA), latex = TRUE)
# "66.7\\%"

# Number and percentage
npercent(c(TRUE, TRUE, FALSE, NA), latex = FALSE)
# "2 (66.7%)"
npercent(c(TRUE, TRUE, FALSE, NA), latex = TRUE)
# "2 (66.7\\%)"

# Percentage with 95% confidence interval
percentConf(c(TRUE, TRUE, FALSE, NA), latex = FALSE)
# "66.7 (9.43, 99.2)"
percentConf(c(TRUE, TRUE, FALSE, NA), latex = TRUE)
# "$66.7$ ($9.43$, $99.2$)"

# Mean and standard deviation
meansd(1:100, latex = FALSE)
# "50.5 (29)"
meansd(1:100, latex = TRUE)
# "$50.5$ ($29$)"
meansd(1:100, dp = 5, latex = FALSE)
# "50.50000 (29.01149)"
}