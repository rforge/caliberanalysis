\name{getCodelist}
\alias{getCodelist}

\title{
Find and import a codelist from a folder
}

\description{
Loads the latest version of a codelist in a folder named according to the CALIBER codelist
naming policy. The function searches through all subfolders of the folder.
}

\usage{
getCodelist(folder = NULL, codelistname = NULL)
}

\arguments{
  \item{folder}{
Folder in which to search for the codelist, or the name of the codelist if
\code{codelistname} is NULL, in which case the function searches in the R working directory.
}
  \item{codelistname}{
The name (not filename) of the codelist
}
}

\value{
Returns the a \code{\link{codelist}} object if the codelist is found, blank otherwise.
}

\note{
This is a convenience function to avoid having to remember the exact location of a codelist file. If one of the arguments is null, the other argument is taken to be the
codelist name and the function searches in the current working directory.
}


\seealso{
\code{\link{codelist}}
}

