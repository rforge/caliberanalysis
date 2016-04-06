\name{CALIBERdrugdose-package}
\alias{CALIBERdrugdose-package}
\alias{CALIBERdrugdose}
\docType{package}

\title{
CALIBER dose conversion program
}

\description{
A program to convert unstructured dosage text into structured dosage information.
It was originally designed for general practitioner dosage instructions from the
Vision clinical system. The program uses a number of lookup tables of words and patterns
which are built in to the package, but with the option of using custom lookup tables.

The main user function is \code{\link{doseconvert}}, which converts a vector of one or
more text dosage instructions into a data.table of structured dose information. The 
program is designed for speed; it automatically collates identical dosage instructions
prior to conversion, and can use multiple processors (using the parallel package on Linux).

The program can be run in server mode using RServe. This is an example using Python
on Ubuntu to access an R service (type these commands into the terminal):

# Install required packages (command line): 
\cr
sudo apt-get install r-cran-rserve \cr
sudo apt-get install python-pip \cr
sudo pip install pyRserve

# Start R in daemon mode: 
\cr
R CMD Rserve

# Start python: 
\cr
python

# Now type the following commands in python: 
\cr
import pyRserve \cr
conn = pyRserve.connect() \cr
conn.voidEval('library(CALIBERdrugdose)')

# Analyse a text \cr
conn.r.doseconvert('2 tablets daily')

# Shut down R session \cr
conn.shutdown()

Example \href{../doc/pythonexample.py}{python code} for a faster way to
run a conversion service (by preloading lookup dictionaries).
}

\details{
\tabular{ll}{
Package: \tab CALIBERdrugdose\cr
Type: \tab Package\cr
Version: \tab 0.1-1\cr
Date: \tab 2016-03-31\cr
License: \tab GPL-3\cr
}
}

\author{
Anoop Shah
}

\references{
Shah AD, Martinez C. An algorithm to derive a numerical daily dose
from unstructured text dosage instructions.
Pharmacoepidemiol Drug Saf 2006; 15(3): 161-166. doi: 10.1002/pds.1151
\url{http://onlinelibrary.wiley.com/doi/10.1002/pds.1151/}
}

\keyword{ package }

\seealso{
\code{\link{doseconvert}}
}

\examples{
doseconvert('take 2 puffs twice daily') 
}
