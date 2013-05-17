\name{print.selection}
\alias{print.selection}
\alias{length.selection}

\title{
Prints the codes in a selection
}

\description{
Prints to the screen the codes included in a selection that are from
dictionaries currently in use (according to \code{\link{getdictionary}}).
}

\usage{
\method{print}{selection}(x, \dots)
\method{length}{selection}(x)
}

\arguments{
  \item{x}{
a selection object.
}
  \item{\dots}{
other arguments to print.selection.
}
}

\details{
S3 method for the 'selection' class, a list of (Read, OPCS and/or ICD-10) 
terms selected from the \code{\link{CALIBER_DICT}} dictionary, stored as a logical vector with the
same length as the number of rows in CALIBER_DICT.
}

\value{
\code{print.selection} returns the selection, invisibly.
\code{length.selection} returns the number of terms in the selection, that are in one of the dictionaries in use.
}


\seealso{
\code{\link{selection}}
}

\examples{
setdictionary('read')
myselection <- termhas('angina')
myselection
}
