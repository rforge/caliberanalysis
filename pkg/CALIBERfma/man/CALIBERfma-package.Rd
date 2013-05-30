\name{CALIBERfma-package}
\alias{CALIBERfma-package}
\alias{CALIBERfma}
\docType{package}

\title{
R interface to the Freetext Matching Algorithm
}

\description{
The Freetext Matching Algorithm (FMA) is a natural language processing system for clinical text, designed to convert unstructured free text into coded information (Read codes, dates and laboratory values). Each piece of structured information is associated with a virtual entity type and medcode in a similar way the native CPRD GOLD format data, facilitating analysis using the same tools. This R package facilitates the analyses of texts and creation and manipulation of FMA lookup tables.

This package is currently only available for Linux as it uses system commands to download the FMA program
and lookups from the GitHub master repository, and runs it using Wine. FMA is a compiled Visual Basic 6.0 program.

The executable and lookup files must be present in the same folder, which can be set up manually by running \code{demo(manualsetup)}, or automatically by calling \code{\link{downloadFMA}()}, which downloads the latest version of the program and lookups from the GitHub repository.

The function \code{\link{doAnalysis}} analyses a single text in debug mode or a set of free text records in a file or data.frame. If loading from a file, it must be in the format required by FMA, as specified here: 

\url{https://github.com/anoopshah/freetext-matching-algorithm/blob/master/README.md}

The functions \code{\link{importLookup}}, \code{\link{addtoLookup}} and \code{\link{exportLookup}} assist in the manipulation of lookup tables, but it is up to the user to ensure that they are in the correct format (e.g. sorted in the correct way) when exported. The formats are specified here:

\url{https://github.com/anoopshah/freetext-matching-algorithm-lookups/blob/master/README.md}
}

\details{
\tabular{ll}{
Package: \tab CALIBERfma\cr
Type: \tab Package\cr
Version: \tab 0.1-0\cr
Date: \tab 2013-05-08\cr
License: \tab GPL-3\cr
}

}
\author{
Anoop Shah

Maintainer: anoop@doctors.org.uk
}

\references{
Shah AD, Martinez C, Hemingway H. The freetext matching algorithm: a computer program to extract diagnoses and causes of death from unstructured text in electronic health records. BMC Med Inform Decis Mak 2012, 12:88 doi:10.1186/1472-6947-12-88. \url{http://www.biomedcentral.com/1472-6947/12/88/}

\url{https://github.com/anoopshah/freetext-matching-algorithm/}

\url{https://github.com/anoopshah/freetext-matching-algorithm-lookups/}
}

\keyword{ package }

\seealso{
\code{\link{doAnalysis}}, \code{\link{fmaTest}}, \code{\link{importLookup}}, \code{\link{downloadFMA}} 
}

