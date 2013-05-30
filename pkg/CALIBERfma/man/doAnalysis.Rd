\name{doAnalysis}
\alias{doAnalysis}

\title{
Analyse free text using the Freetext Matching Algorithm
}

\description{
Analyses free text using the Freetext Matching Algorithm. The text to analyse can be in a file or an R object; if it is R it is written to a temporary file and analysed using a system call. The Freetext Matching Algorithm itself is a compiled Visual Basic 6.0 program; the system command to run the program must be specified in the option 'CALIBERfma_command'. This can be done using the function \code{\link{downloadFMA}}, which downloads the latest version from GitHub, or the demo script.
}

\usage{
doAnalysis(freetext = NULL, medcode = NULL, infile = NULL,
    outfile = NULL, medcodefile = NULL, logfile = NULL, 
    fmadir_R = getOption('CALIBERfma_dir_R',
        paste(tempdir(), '/fma/', sep='')),
    fmadir_win = getOption('CALIBERfma_dir_win',
        paste(gsub('/', '\\\\\\\\', tempdir()), '\\\\fma\\\\', sep='')))
}

\arguments{
  \item{freetext}{
a string (character vector of length 1) containing a single free text record to analyse in debug mode.
If \code{freetext} is specified, the arguments \code{infile}, \code{outfile} and \code{medcodefile}
are ignored, and the output of analysis is written to the log file or displayed on screen.
}
  \item{medcode}{
a single integer of a medcode associated with \code{freetext}. Ignored when processing multiple text records from files or a data.frame.
}
  \item{infile}{
either a string stating the file path of the text to analyse, or a data.frame containing columns pracid (integer), textid (integer), text (character) in that order. If a text file, it must be tab-delimited with no column headers and columns in the order pracid, textid, text, and Windows-style line endings (carriage return and linefeed).
}
  \item{outfile}{
output file path. If NULL, the output structured data are returned as a data.table.
}
  \item{medcodefile}{
either a string stating the file path of the medcodes associated with infile, or data.frame containing columns pracid (integer), textid (integer), medcode (integer). If a text file, it must be comma separated with column headers pracid, textid, text. It must have Windows-style line endings.
}
  \item{logfile}{
file path for log file. If NULL, the log file is printed to screen.
}
  \item{fmadir_R}{
folder location of FMA program and lookup tables, in Unix / R format
}
  \item{fmadir_win}{
folder location of FMA program and lookup tables, in Wine format for Windows programs to use
}
}


\value{
If outfile is NULL, a \code{\link{fmaTest}} object containing the results of analysis is returned.
}

\author{
Anoop Shah
}


\seealso{
\code{\link{fmaTest}}
}

