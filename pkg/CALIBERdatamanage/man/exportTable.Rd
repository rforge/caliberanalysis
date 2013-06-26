\name{exportTable}
\alias{exportTable}

\title{
Export a LaTeX table to file
}

\description{
Exports a LaTeX table (as produced by \code{xtable}) to a file,
optionally converting
}

\usage{
exportTable(data, filename = NULL, align = "l",
    include.rownames = FALSE, sanitize.text.function = function(x) x,
    blank = "$-$", digits = 3, booktabs = FALSE, ...)
}

\arguments{
  \item{data}{
a data.frame or character matrix
}
  \item{filename}{
filename to export to
}
  \item{align}{
column alignment
}
  \item{include.rownames}{
whether row names should be included
}
  \item{sanitize.text.function}{
function to convert special characters, if necessary, so that they can be output as
by LaTeX
}
  \item{blank}{
text to enter in blank cells
}
  \item{digits}{
number of significant digits
}
  \item{booktabs}{
whether to use \code{toprule}, \code{midrule} etc. if , \{\code{hline}
}
  \item{\dots}{
other arguments to pass to \code{xtable}
}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}



\author{
Anoop Shah
}

\seealso{
\code{\link{summaryTable}}
}

\examples{

}
