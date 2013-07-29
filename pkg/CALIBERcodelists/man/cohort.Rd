\name{cohort}
\alias{cohort}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cohort(x, idcolname = c("patid", "anonpatid", "id"), description = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{idcolname}{
%%     ~~Describe \code{idcolname} here~~
}
  \item{description}{
%%     ~~Describe \code{description} here~~
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
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, idcolname = c("patid", "anonpatid", "id"), description = NULL) 
{
    out <- as.data.table(x)
    idcolname <- idcolname[which(idcolname \%in\% names(x))]
    if (length(idcolname) == 1) {
        setattr(x, "idcolname", idcolname)
    }
    else {
        stop("Invalid ID column  name")
    }
    if (any(is.na(out[[idcolname]]))) {
        stop("ID may not be missing")
    }
    if (length(unique(out[[idcolname]])) < nrow(out)) {
        stop("ID must be unique")
    }
    if (is.null(description)) {
        description <- data.table(colname = setdiff(names(x), 
            idcolname))
    }
    else {
        description <- data.table(description)
        if (names(description) != c("colname", "description")) {
            stop("Description table must have two columns: colname and description")
        }
    }
    setkey(description, colname)
    setcolorder(out, c(idcolname, sort(setdiff(names(out), idcolname))))
    setkeyv(out, idcolname)
    setattr(out, "description", description)
    class(out) <- "cohort"
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
