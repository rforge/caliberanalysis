\name{showhtml}
\alias{showhtml}

\title{
Show the selected and excluded terms in HTML
}

\description{
This is designed to be used within a R 'as is' section in a R Markdown (Rmd) document. It displays all the selected codes in the master dictionary by dictionary, as well as excluded terms (those with category 0) and terms with the same 3-character code which were not included (if \code{show_not_selected} is TRUE).
}

\usage{
showhtml(show_not_selected = TRUE)
}

\arguments{
\item{show_not_selected}{TRUE or FALSE, whether to show terms with the same
three-character code but not included in the selected or excluded terms.}
}

\details{
Within a R Markdown document, the function should be called as follows:

```{r results='asis'}
showhtml()
```

This instructs knitr to enter the output of showhtml directly as HTML.
}

\value{
A character vector of length 1, containing the HTML code fragment.
}

\note{
In the table of categories with number of terms, ICD-10 headers are not
included in the number of ICD-10 terms but the individual 4 and 5 character
terms are included. This means that the number of terms is different
from those displayed in the document (where headers and individual codes
are shown separately) and in the final codelist (where only the 3-character
header is included if all terms under that header are included in the same
category).
}

\seealso{
\code{\link{as.codelist}}, \code{\link{export}}, \code{\link{exportall}}, \code{\link{process}}
}

\examples{
setdictionary('read', 'icd10')
assigncat(1, 'rhabdomyo', termhas('rhabdomyolysis'))
assigncat(0, 'Excluded', termhas('renal'))
showhtml()
}
