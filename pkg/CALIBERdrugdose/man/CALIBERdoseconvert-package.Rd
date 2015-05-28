\name{CALIBERdrugdose-package}
\alias{CALIBERdrugdose-package}
\alias{CALIBERdrugdose}
\docType{package}

\title{
CALIBER dose conversion program
}

\description{
A program to convert unstructured dosage text into structured dosage information
for use in pharmacoepidemiological studies
}

\details{
\tabular{ll}{
Package: \tab CALIBERdrugdose\cr
Type: \tab Package\cr
Version: \tab 0.1-0\cr
Date: \tab 2013-01-31\cr
License: \tab GPL-3\cr
}
}

\author{
Anoop Shah
}

\references{
Denaxas et al. Data Resource Profile: Cardiovascular disease research using linked 
bespoke studies and electronic health records (CALIBER). 
Int. J. Epidemiol. (2012) 41 (6): 1625-1638. doi: 10.1093/ije/dys188 
\url{http://ije.oxfordjournals.org/content/41/6/1625}

Shah AD, Martinez C. An algorithm to derive a numerical daily dose from unstructured text dosage instructions.
Pharmacoepidemiol Drug Saf 2006; 15(3): 161–166. doi: 10.1002/pds.1151
\url{http://onlinelibrary.wiley.com/doi/10.1002/pds.1151/}
}

\keyword{ package }

\examples{
doseconvert('take 2 puffs twice daily') 
}
