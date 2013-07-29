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
getCodelist(folder, codelistname)
}

\arguments{
  \item{folder}{
Folder in which to search for the codelist
}
  \item{codelistname}{
The name (not filename) of the codelist
}
}

\details{

}
\value{
Returns the a \code{\link{codelist}} object if the codelist is found, blank otherwise.
}


\note{
This is a convenience function to avoid having to remember the exact location of a codelist file.
}


\seealso{
\code{\link{codelist}}
}

