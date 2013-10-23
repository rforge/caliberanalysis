\name{multiforest}
\alias{multiforest}
\alias{multiforestPDF}
\alias{multiforestPNG}

\title{
Produce multiple forest plots side by side
}

\description{
Produces forest plot using data in a standardised format from a spreadsheet. Can produce multiple forest plots in one figure, arranged horizontally. Box sizes, font styles and sizes can be specified in a spreadsheet to make the output easy to configure. 
}

\usage{
multiforest(data = NULL, rowheights = 1.1, colgaps = unit(0.3, "cm"),
    colgaps_cm = NULL, defaultfontface = "plain",
    defaultjust = "left", defaultfontsize = 10,
    defaultgraphwidth = unit(5, "cm"), 
    defaultboxsize = 0.8, defaultxlab = "Hazard ratio",
    xticks = NULL, xtickfontsize = 10, xlabfontsize = 12,
    xlaboffset = -2.5, zerocolour = "gray", zerolinewidth = 1, ...)
multiforestPNG(data, outfile, border = 0.1, ...)
multiforestPDF(data, outfile, border = 0.1, ...)
}

\arguments{
  \item{data}{
either a filename of a tab or comma-delimited text file or Stata file containing the data to plot, or a data.frame containing this information. If NULL, the user is asked to choose the text file using \code{file.choose}.
}
  \item{outfile}{
output filename. If missing and \code{data} is a filename, the time and PNG or PDF file extension are appended to the data file name.
}
  \item{border}{
border width in inches.
}
  \item{rowheights}{
relative row height (default=1), can be a single number or a vector of heights
}
  \item{colgaps}{
gap between columns of the output.
}
  \item{colgaps_cm}{
a convenience wrapper for supplying colgaps as a \code{units} object.
}
  \item{defaultfontface}{
font face as a character string, default = 'plain', possible alternatives are 'bold', 'italic' or 'bold.italic'
}
  \item{defaultjust}{
default justification of text, default = 'left', possible alternatives are 'right' or 'centre'
}
  \item{defaultfontsize}{
a number specifying the default point size
}
  \item{defaultgraphwidth}{
default width of graph as a unit object, if not specified in data
}
  \item{defaultboxsize}{
default box size, between 0 and 1. Set to NULL to set box size to relative inverse variance.
}
  \item{defaultxlab}{
default label for x axis
}
  \item{xticks}{
a vector of positions for tick marks on the x axis, NULL to generate tick positions from the data
}
  \item{xtickfontsize}{
point size of axis numbers
}
  \item{xlabfontsize}{
point size of x axis label
}
  \item{xlaboffset}{
offset of x axis label from axis, measured in text lines
}
  \item{zerocolour}{
colour of zero line (for hazard ratio = 1)
}
  \item{zerolinewidth}{
width of zero line
}
  \item{\dots}{
Other arguments to multiforest, read.delim or read.dta.
}
}

\details{
Uses grid graphics. Based on the forestplot function in the rmeta package, with extensions to allow multiple plots and other options.

The spreadsheet must contain groups of columns specifying the text, numbers to graph and formatting column headings in the first row. The first two characters of each column heading denote the group, and the remainder must be one of the following:

For text columns:
\describe{
\item{text}{text or an R expression. If the text is of the form
\code{expression(<R expression>)}, e.g. \code{expression(paste(theta^2))}
it is interpreted as an R expression, otherwise it is treated as text.}
\item{fontface}{either 'plain', 'italic', 'bold' or 'bold.italic' (must be in lower case)}
\item{fontsize}{size of font in points}
\item{just}{'left', 'right' or 'centre'}
\item{numcols}{number of columns to span (default = 1)}
}

To insert spaces between columns, either specify the \code{colgaps} argument, or insert an intermediate column with the appropriate number of spaces in one of the cells to provide the correct width. (see \code{\link{test_multiforest2}}).

For graph columns, options which apply to every row:
\describe{
\item{mean}{coefficient; required}
\item{upper}{upper 95\% CI; either \code{upper} and \code{lower} or \code{se} must be supplied}
\item{lower}{lower 95\% CI}
\item{se}{standard error, can be used to calculate 95\% CI if upper and lower are not supplied}
\item{boxsize}{relative box sizes}
\item{boxcolour}{colour specified as a string}
\item{linecolour}{line colour specified as a string}
\item{linewidth}{default is 2}
\item{summary}{1 if it is the summary row, 0 if not}
\item{clipupper}{clip if upper CI greater than this}
\item{cliplower}{clip if lower CI less than this}
}

For graph columns, the following options apply to the whole graph, and only the first row below the column heading is considered:
\describe{
\item{xlog}{whether to use a log scale: 1 for log scale (default), 0 for ordinary scale (which will not exponentiate coefficients either)}
\item{xlab}{label for x axis, default is 'Hazard ratio'}
\item{graphwidth}{width of graph in centimetres}
}

For example, the column headings might be a1text, a1fontface, g1mean, g1se, ... If a column is omitted the default settings are used.

See \url{http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf} for a list of colours.
}

\value{
\code{multiforest} returns a list containing the image dimensions, which is used by
\code{multiforestPNG} and \code{multiforestPDF} to output the plot at the appropriate size.

\item{height_in_inches}{height of the plot in inches}
\item{width_in_inches}{width of the plot in inches}
}

\seealso{
\code{\link{test_multiforest}}, \code{\link{test_multiforest2}}
}

\examples{
mydata <- data.frame(a1text = c(
    'expression(paste("Graph of ", theta))', 'Plot1', 'Plot2',
    'expression(theta)'),
    a1fontface = c('plain', 'italic', 'bold', 'bold.italic'),
    a1fontsize = c(13, 9, 10, 11),
    a1just = c('centre', 'left', 'right', 'right'),
    a1numcols = c(2, 1, 1, 1),
    a2mean = c(NA, 0.3, 0.6, 1),
    a2se = c(NA, 0.3, 0.6, 0.1),
    a2boxsize = c(NA, 1, 2, 3),
    a2boxcolour = c(NA, 'red', 'green', 'blue'),
    a2linecolour = c(NA, 'red', 'green', 'black'),
    a2linewidth = c(NA, 1, 2, 3),
    a2summary = c(0, 0, 0, 1))
multiforest(mydata, xticks = c(0.5, 1, 2, 4))

# Load an example from data
data(test_multiforest)
multiforest(test_multiforest)

# Another example
data(test_multiforest2)
# Create spaces in column 2 (these were lost when the text file was
# loaded using 'data', but wil be retained if loaded directly by multiforest)
test_multiforest2$a2text <- "   "
multiforest(test_multiforest2)
}
