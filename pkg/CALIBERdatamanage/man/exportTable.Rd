\name{exportTable}
\alias{exportTable}

\title{
Export a LaTeX table to file
}

\description{
Exports a LaTeX table (as produced by \code{xtable}) to a file,
optionally converting blanks to dashes and sanitizing text.
}

\usage{
exportTable(data, filename = NULL, align = "l",
    include.rownames = FALSE, include.colnames = FALSE,
    sanitize.text.function = "default",
    blank = "$-$", digits = 3, booktabs = FALSE, ...)
}

\arguments{
  \item{data}{
a data.frame or character matrix
}
  \item{filename}{
filename to export to. If NULL, the output is returned visibly by the function.
}
  \item{align}{
character vector indicating column alignment. If include.rownames is TRUE, it should have length equal to the number of columns + 1, otherwise it can have length equal to the number of columns. If it is shorter than this, the last element is repeated to make it the correct length. Unlike with the function \code{xtable}, \code{align} is not permitted to be a single character string.
}
  \item{include.rownames}{
whether row names should be included
}
  \item{include.colnames}{
whether column names should be included
}
  \item{sanitize.text.function}{
either 'default' or a function to convert special characters to LaTeX, for use by \code{xtable} in the output. The default is to handle <, _, >, % and & and leave all other characters as they are. Set \code{sanitize.text.function} to \code{function(x) x} to avoid any conversion.
}
  \item{blank}{
text to enter in blank cells
}
  \item{digits}{
number of significant digits
}
  \item{booktabs}{
whether to use \code{toprule}, \code{midrule} etc. in place of \{\code{hline}. There must be a \code{\\usepackage{booktabs}} command in the preamble of the LaTeX document.
}
  \item{\dots}{
other arguments to pass to \code{xtable}.
}
}


\value{
Returns a character vector with the LaTeX output. This is returned invisibly if 
\code{filename} is not NULL, otherwise it is returned invisibly.
}

\author{
Anoop Shah
}

\seealso{
\code{\link{summaryTable}}
}

