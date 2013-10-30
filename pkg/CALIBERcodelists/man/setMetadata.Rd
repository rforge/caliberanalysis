\name{setMetadata}
\alias{setMetadata}

\title{
Set metadata for a codelist or the master dictionary
}

\description{
Sets the metadata attributes for a codelist or the master dictionary (the \code{\link{META}} table).
}

\usage{
setMetadata(codelist=NULL, Name=NULL, Version=NULL, Author=NULL,
    Date=NULL, Categories=NULL, Source=NULL)
}

\arguments{
  \item{codelist}{a codelist object. NULL to update the \code{\link{META}} table for the codelist under construction.}
  \item{Name}{new name for the codelist. The naming convention is for GPRD (Read) codelists to end in _gprd, ICD-10 codelists to end in _hes, OPCS (procedure) codelists to end in _opcs and GPRD drug codelists to end in _gprddrug.}
  \item{Version}{Version number, to be supplied as a character string or a number. Versions 0, 0.1, 0.2 \dots should be used for unapproved (development) versions, and versions 1, 2 etc. for approved codelists. The version number is incremented if any terms are added to the codelist or the categories are changed.}
  \item{Author}{a string of author names, or a character vector of names, or a vector of \code{person} objects.}
  \item{Date}{a Date object or character string representing the date on which the codelist was created.}
  \item{Categories}{the categories table, in the form of a data.table with columns \code{category} (number), \code{shortname} (brief definition) and \code{description} (full definition)}
  \item{Source}{a string representing the data source. For the master dictionary, it can be a space or comma separated list or a vector of strings if multiple dictionaries are in use (but only one distinct source per dictionary). Options are 'GPRD' for Read codelists, 'HES' or 'ONS' for ICD-10 codelists, 'OPCS' for OPCS codelists, and 'ONSICD9' for ICD-9 codelists. A particular Source can only be associated with a single dictionary at a time (e.g. it is not possible to export a codelist to HES and ONS simultaneously; instead one has to export one version, use SetMetadata to update the source and then export the other). The allowable Source / dictionary combinations are hard-coded in the \code{SOURCEDICTS} data.table (in the \code{sysdata.R} source file of the package).}
}

\details{
If using R Markdown, the metadata can be drawn from a table outside the R code itself, meaning that you only have to write this information in one place for it to be in the HTML document and the codelists. The fields "Author", "Date", "Name", "Source" and "Version" can be supplied in the Rmd file in the format \code{item | value} on a single line (e.g. \code{Version | 1}). 
}

\value{
When used with a codelist, \code{setMetadata} updates the metadata of the codelist by reference and returns the modified codelist.
}

\author{
Anoop Shah, Joshua Wallace
}

\seealso{
\code{\link{assigncat}}, \code{\link{as.codelist}}, \code{\link{setdictionary}}, \code{\link{process}}
}

\examples{
setMetadata(Version=1)

h <- as.codelist(termhas('angina'), dictionary='read')
setMetadata(h, Name='angina_gprd', Date='2003-02-04',
	Author=c('Me', 'You'), Version='2.1')
stopifnot(identical(attr(h, 'Name'), 'angina_gprd'))
stopifnot(identical(attr(h, 'Date'), '04 Feb 2003'))
stopifnot(identical(attr(h, 'Author'), 'Me, You'))
stopifnot(identical(attr(h, 'Version'), '2.1'))
}

