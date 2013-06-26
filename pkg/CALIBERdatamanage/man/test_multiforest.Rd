\name{test_multiforest}
\alias{test_multiforest}
\docType{data}
\title{
Sample output specificaton for forest plot
}

\description{
A spreadsheet with sample data in the format for the
\code{\link{multiforest}} function to produce multiple forest plots.
}

\usage{data(test_multiforest)}
\format{
  A data frame with 9 observations on the following 23 variables.
  \describe{
    \item{\code{s1text}}{text, a factor with levels  \code{Men} \code{Myocardial infarction} \code{OVERALL} \code{Stable angina} \code{Women}}
    \item{\code{s1fontface}}{font style, a factor with levels  \code{bold.italic} \code{italic} \code{plain}}
    \item{\code{s1just}}{justification (left, right centre), a factor with levels  \code{left}}
    \item{\code{s1fontsize}}{a numeric vector}
    \item{\code{a2text}}{a factor with levels  \code{26791} \code{3542} \code{5642} \code{7843} \code{9764} \code{N}}
    \item{\code{x1mean}}{log odds ratio for plotting, a numeric vector}
    \item{\code{x1se}}{standard error of log odds ratio for plotting, a numeric vector}
    \item{\code{x1xlab}}{label on the X axis, a factor with levels  \code{Odds ratio}}
    \item{\code{s2text}}{text odds ratio and confidence interval, a factor with levels  \code{1.11 (0.75, 1.64)} \code{1.35 (0.75, 2.43)} \code{1.49 (1.01, 2.21)} \code{1.65 (0.92, 2.97)} \code{1.82 (1.50, 2.22)} \code{OR (95\% CI)}}
    \item{\code{s2fontface}}{a factor with levels  \code{italic} \code{plain}}
    \item{\code{s2just}}{a factor with levels  \code{right}}
    \item{\code{s2fontsize}}{a numeric vector}
    \item{\code{s3text}}{a factor with levels  \code{*} \code{***}}
    \item{\code{s3just}}{a factor with levels  \code{left}}
    \item{\code{g1boxsize}}{a numeric vector}
    \item{\code{g1mean}}{a numeric vector}
    \item{\code{g1se}}{a numeric vector}
    \item{\code{g1xlab}}{a factor with levels  \code{Hazard ratio}}
    \item{\code{g1linewidth}}{a numeric vector}
    \item{\code{g1boxcolour}}{a factor with levels  \code{black} \code{blue} \code{green} \code{orange} \code{red}}
    \item{\code{g1linecolour}}{a factor with levels  \code{black} \code{blue} \code{green} \code{orange} \code{red}}
    \item{\code{g1graphwidth}}{a numeric vector}
    \item{\code{g1summary}}{whether to show a summary diamond (1) or a box and whisker (0), a numeric vector}
  }
}


\examples{
data(test_multiforest)
multiforest(test_multiforest)
}
\keyword{datasets}
