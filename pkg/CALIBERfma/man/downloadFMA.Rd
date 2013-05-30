\name{downloadFMA}
\alias{downloadFMA}

\title{
Download Freetext Matching Algorithm from the master GitHub repository
}

\description{
Downloads the latest version of the Freetext Matching Algorithm to the temporary folder (or other specified folder) so that it can be used to analyse free text by calling \code{\link{doAnalysis}}. Alternatively, to use specific versions of the program or lookups, run \code{demo()}.

This function also sets the options CALIBERfma_dir_R, CALIBERfma_dir_win and CALIBERfma_command. This allows \code{\link{doAnalysis}} to be run immediately afterwards without needing to manually set these options.
}

\usage{
downloadFMA(filename = "fma15command.exe")
}

\arguments{
  \item{filename}{
a character string with the name of the Freetext Matching Algorithm executable
}
}

\value{
None
}

\details{
Git must be installed. This function uses Unix system commands to create a
directory 'fma' in the R temporary directory, and subdirectories 'program'
and 'lookups'. The online Git repositories 'freetext-matching-algorithm'
and 'freetext-matching-algorithm-lookups' are downloaded into these folders;
the relevant binary file and lookup tables are moved out into the 'fma'
directory and the R options are set to this directory.
}

\author{
Anoop Shah
}

\seealso{
\code{\link{doAnalysis}}
}