\name{browsedict}
\alias{browseCodelist}
\alias{browseSelection}

\title{
Browse a selection, codelist or the master dictionary in a spreadsheet
}

\description{
The browse functions allow an existing codelist, a selection or 
a codelist under construction to be viewed in a spreadsheet and the
categories edited manually. This is done by writing the selection
of terms to a spreadsheet in the temporary directory.
If the edited spreadsheet is saved (keeping it as a
.csv file with the same name), the modified data are returned to R.

Note that Read codes may be formatted wrongly when opened in Excel;
however the medcode is used as the definitive identifier
for the Read term so this is not a problem.
}

\usage{
browseCodelist(x, spreadsheet = NULL)
browseSelection(x = NULL, spreadsheet = NULL, update_cats = FALSE)
}

\arguments{
  \item{x}{
for browseCodelist: a codelist; for browseSelection: a selection object,
or an object which can be coerced to selection (e.g. a character vector,
in which case it will automatically be used to select terms using
\code{\link{termhas}}). If NULL, the currently selected
terms in CALIBER_DICT with a positive category are used.
}
  \item{spreadsheet}{
for UNIX systems, the spreadsheet program to use (default is \code{soffice}). Ignored on Windows, instead
the default program for files with the extension .csv is used (usually Microsoft Excel).
}
  \item{update_cats}{
TRUE if the categories in CALIBER_DICT are to be used (i.e. when generating a
new codelist). FALSE if a modified selection is to be returned without
affecting the categories in CALIBER_DICT.
  }
}


\value{
browseCodelist returns the modified codelist (which is also updated by reference).

browseSelection returns the modified selection if \code{update_cats} = FALSE.
If \code{update_cats} = TRUE, browseSelection returns a character string of 
instructions for updating the categories of CALIBER_DICT using \code{\link{assigncat}}
statements. This is used in interactive mode to enable a user to edit the categories
directly in the spreadsheet while recording the instructions for the
modifications in a file.
}

\seealso{
Interactive mode: \code{\link{exploreCodes}}

\code{\link{as.codelist}}, \code{\link{compare}}, \code{\link{CALIBER_DICT}}, \code{\link{export}},
\code{\link{process}}, \code{\link{as.selection}}
}

\examples{
# setdictionary('read')
# browseSelection('meningitis')
# mi <- as.codelist(termhas('myocardial infarction'))
# browseCodelist(mi)
}
