\name{as.data.table.ffdf}
\alias{as.data.table.ffdf}
\alias{as.ffdf.data.table}

\title{
S3 methods to convert between FFDF and data.table
}

\description{
Converts a Flat File Data Frame (ffdf) to a data.table object, or the other way around.
FFDF is a convenient form for storing very large datasets without
using RAM, but data.table is faster and convenient for data processing.
}

\usage{
\method{as.data.table}{ffdf}(x, keep.rownames = FALSE)
\method{as.ffdf}{data.table}(x, vmode = NULL,
    col_args = list(), ...)
}

\arguments{
  \item{x}{
object to convert (FFDF data frame or data.table). FFDF objects
must have more than one row in order to be converted to data.table.
}
  \item{keep.rownames}{
TRUE will retain the rownames of x in a column named rn.
}
  \item{vmode}{
optional specification of the vmodes of columns of the data.frame. Either a character vector of vmodes (named with column names of the data.frame or recycled if not named) or a list named with vmodes where each element identifies those columns of the data.frame that should get the vmode encoded in the name of the element
}
  \item{col_args}{
further arguments; passed to ff
}
  \item{\dots}{
further arguments; passed to ffdf for .ff_vector, .ff_matrix and .data.frame methods, ignored for .ffdf identity method
}
}

\details{
If these functions are provided in the ffdf or data.table packages,
they will be removed from this package. \code{as.ffdf.data.table}
is a convenience function which automatically converts characters to
factors in the original data.table then calls \code{as.ffdf.data.frame}.
}

\value{
\code{as.ffdf} returns a FFDF object; \code{as.data.table} returns a data.table object.
}

\author{
Anoop Shah
}

\seealso{
\code{\link{importDT}}, \code{\link{importFFDF}}
}

\examples{
mydatatable <- data.table(a = 1:6, b = 'this')
mydatatable

# Convert to ffdf (column b will be converted to a factor)
myffdf <- as.ffdf(mydatatable)
myffdf

# Convert back to data.table (column b remains as a factor)
as.data.table(myffdf)
str(mydatatable)
}
