\name{exploreCodes}
\alias{exploreCodes}

\title{
Manage codelists and create a new codelist interactively
}

\description{
This function invokes an interactive mode for browsing the dictionary and
creating codelists. The R commands and output are automatically logged in a file,
with the output commented out so that the log file can be re-run as a script.
The log file is automatically opened with a  text editor at the end of the session
so that it can be saved or edited.
}

\usage{
exploreCodes(spreadsheet = 'Default', texteditor = 'Default',
    showLog = TRUE, direct = NULL)
}

\arguments{
  \item{spreadsheet}{
for UNIX systems, the spreadsheet program to use (default is \code{soffice}). Ignored on Windows, instead
the default program for files with the extension .csv is used (usually Microsoft Excel).
}
  \item{texteditor}{
for UNIX systems, the text editor program to use (default is \code{gedit}).
Ignored on Windows, instead the default program for files with the extension .log is used (usually Microsoft Notepad).
}
  \item{showLog}{
whether to open the log of the interactive session. This is set to FALSE for the makecodelist demo,
in which the codes are written to a Rmd document.
}
  \item{direct}{
option to bypass the main menu. \code{direct} = 1 opens the menu for managing codelists.
\code{direct} = 2 opens the menu for creating a new codelist.
}
}

\seealso{
Interactive editing of selections and codelists: \code{\link{browseCodelist}}, \code{\link{browseSelection}}

\code{\link{as.codelist}}, \code{\link{compare}}, \code{\link{CALIBER_DICT}}, \code{\link{export}},
\code{\link{process}}, \code{\link{as.selection}}
}

\examples{
# exploreCodes()
}
