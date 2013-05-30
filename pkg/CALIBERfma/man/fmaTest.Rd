\name{fmaTest}
\alias{fmaTest}
\alias{print.fmaTest}

\title{
fmaTest: S3 class for storing free text and results of analysis
}

\description{
The S3 class defines an object for storing the raw free text and the coded output from the Freetext Matching Algorithm, so that it can be displayed on screen in a convenient way. The analysis function \code{\link{doAnalysis}} can return a fmaTest object, or it can be created from files or data.frame objects containing the raw free text, associated medcodes and the Freetext Matching Algorithm output.
}

\usage{
fmaTest(infile, outfile, medcodefile = NULL, lookupfolder = NULL)

\method{print}{fmaTest}(x, ...)
}

\arguments{
  \item{infile}{
character string for the path to an input file used for FMA analysis, or a data.frame with the input data, in the format required by \code{\link{doAnalysis}}.
}
  \item{outfile}{
character string to FMA output file or a data.frame with FMA structured output
}
  \item{medcodefile}{
character string for a file containing medcodes, or a data.frame containing the data, in the format required by \code{\link{doAnalysis}}.
}
  \item{lookupfolder}{
folder containing lookup files. If NULL, it is obtained from the option CALIBERfma_lookups_R.
}
\item{x}{a fmaTest object}
\item{\dots}{Other arguments to pass to \code{link{print.fmaTestElement}}}
}


\value{
A fmaTest object. This is a list of fmaTestElement objects, each of which is a list with the following elements:

\item{pracid}{practice identifier}
\item{textid}{text identifier}
\item{medcode}{medcode associated with the text}
\item{readcode}{Read code linked to the medcode associated with the text}
\item{readterm}{Read term linked to the medcode associated with the text}
\item{text}{unstructured free text}
\item{fma}{structured output from the Freetext Matching Algorithm}
}

\author{
Anoop Shah
}


\seealso{
\code{\link{print.fmaTestElement}}
}
