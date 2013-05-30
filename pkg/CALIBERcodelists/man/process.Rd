\name{process}
\alias{process}

\title{
Process an R Markdown file
}

\description{
Process an R Markdown file to generate a CALIBER codelist together with documentation.
The text can have extension .Rmd or .txt (which may be more convenient on Windows).
If it is a R file (extension .R) or a CALIBERcodelists log file (.log),
it is processed as a R script instead. The categories and metadata
are always cleared at the start of this function.

The working directory is assumed to be the directory
containing the Markdown file; the new HTML file and codelists
are created in this directory (although codelists can be exported
elsewhere if the directory is specified in the \code{\link{export}} or
\code{\link{exportall}} function.
}

\usage{
process(filename, varname = NULL, showR = FALSE, 
    showhtml = TRUE, show_not_selected = TRUE, css = customCSS())
}

\arguments{
  \item{filename}{
the name of the R markdown file or R script to process (if it is in the current working directory), or the full filepath if it is elsewhere. If the file extension is .R or .log, it is assumed to be an R script, otherwise it is assumed to be a R Markdown document.
}
  \item{varname}{
the variable name. This can be left blank if it is supplied in the Rmd document in a line containing text such as "Name|angina" to set varname to be "angina".
}
  \item{showR}{
whether to show the R output by default, if no output options are specified for a Markdown chunk.
}
  \item{showhtml}{
whether to automatically call \code{\link{showhtml}} at the end of document to show the final codelist.
}
  \item{show_not_selected}{
whether to show terms with the same three-character code (and more than 100 events for GPRD terms) but not included in the exported codelist. Ignored if \code{showhtml} = FALSE.
}
  \item{css}{custom stylesheet for HTML documentation. For convenience the default option is hard-coded into the internal customCSS function, which can be viewed using \code{CALIBERcodelists:::customCSS}}
}

\details{
The search strategy and documentation for a set of codelists for a CALIBER variable can be documented in R Markdown. Markdown is a plain text format with simple ways of denoting headings, bullet points and tables, which enables R code to be embedded directly within the document. \code{process} uses the \link{knitr} package to run the R code and embed the results in the HTML document.

The table of codelist metadata should be entered like this (it doesn't matter if the | do not line up):

\preformatted{
Name    | mycodelist 
--------|--- 
Author  | Anoop Shah 
Version | 2 
Date    | 1 Jan 2013 
Changes | Corrected term XXX to category 3.
}

In the HTML document, this is automatically formatted like this:
\tabular{ll}{
\bold{Name} \tab \bold{mycodelist} \cr
Author \tab Anoop Shah \cr
Version \tab 2 \cr
Date \tab 1 Jan 2013 \cr
Changes \tab Corrected term XXX to category 3. \cr
}

The `Changes' field is not enforced by the program, and is not necessary for the first version of a codelist.

The codelist name will be taken from the metadata table, and used as the stem for files exported using \code{\link{exportall}} (i.e. the GPRD codelist file will be named mycodelist_gprd.codelist.2.csv).

R code is entered like this:

\preformatted{
```{r}
# Enter R code here, e.g.
setdictionary('read', 'icd10')
assigncat(1, 'hcm', termhas('hypertroph*cardiomyopath'))
exportall()
```
}

R code with HTML output can be entered like this (to produce a table of the current codelists selected in the master dictionary):

\preformatted{
```{r results='asis'}
showhtml()
```
}

This is automatically included at the end of the document (if \code{showhtml} = TRUE), but
to put the table of terms at a different point in the document call \code{process} with 
\code{showhtml} = FALSE and explicitly enter a \code{\link{showhtml}} command.
}

\value{
A string describing the comparison between the current codelists and the previously saved version of the codelists (if available), also stored in META['message', value]
}


\seealso{
\code{\link{as.codelist}},  \code{\link{assigncat}},   \code{\link{export}}, \code{\link{exportall}}, \code{\link{getdictionary}}, \code{\link{setdictionary}}, \code{\link{showhtml}}
}


