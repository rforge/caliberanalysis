\name{setdictionary}
\alias{setdictionary}
\alias{getdictionary}

\title{
Get and set which dictionary or dictionaries to use
}

\description{
The first step in the generation of any codelist is to call the \code{setdictionary} function in order to specify which dictionaries are to be used for the codelist, and to blank the categories. The dictionary names can be supplied in a vector or as three separate arguments.
The function \code{getdictionary} returns the dictionary or dictionaries in use. This function is not used
to specify the dictionaries to use when creating a selection; use \code{\link{dictis}} instead.

To start off with an existing codelist and modify it using the master dictionary (e.g. to add
new terms). This updates blank entries in the master category table and other metadata
in META but does not over-write any information that is there already.
}

\usage{
setdictionary(dictName1 = NULL, dictName2 = NULL, dictName3 = NULL,
    dictName4 = NULL)
getdictionary()
}

\arguments{
  \item{dictName1}{
Either a codelist or a dictionary name, one of "icd9", "icd10", "opcs" or "read", or a vector of two or more of these dictionary names, or leave empty to clear the categories but not alter which dictionaries are  in use.
}
  \item{dictName2}{
Either a codelist or a dictionary name, one of "icd9", "icd10", "opcs" or "read".
}
  \item{dictName3}{
Either a codelist or a dictionary name, one of "icd9", "icd10", "opcs" or "read".
}
  \item{dictName4}{
Either a codelist or a dictionary name, one of "icd9", "icd10", "opcs" or "read".
}
}


\value{
\code{setdictionary} returns the selected dictionaries, invisibly. It also displays a message stating which dictionaries are in use and that the categories have been cleared, and whether 
the terms in the dictionary have been initialised using another codelist.

\code{getdictionary} returns a character vector for the selected dictionaries.
}

\note{
\code{setdictionary} should only be used to set which dictionaries are in use for an entire work session.
Selection objects can contain terms in all dictionaries but only the terms in 
dictionaries in use (according to \code{\link{getdictionary}}) are visible.
To limit the dictionary for a particular selection, use \code{\link{dictis}} with a Boolean operator
e.g. \verb{termhas('angina') \%AND\% dictis('read', 'icd10')}
}


\seealso{
Master dictionary: \code{\link{CALIBER_DICT}}

Metadata for master dictionary: \code{\link{META}}

Assigning categories in master dictionary: \code{\link{assigncat}}

Selecting terms to assign to a category: \code{\link{codematch}}, \code{\link{dictis}}, \code{\link{explode}}, \code{\link{termhas}}
}

\examples{
## To clear categories but not alter which dictionaries are selected
setdictionary()

## To select one, two or three dictionaries to use for codelist generation
setdictionary('icd10')
setdictionary('read', 'opcs')
setdictionary('icd10', 'read', 'opcs')

## To find out which dictionaries are selected
getdictionary()
}

