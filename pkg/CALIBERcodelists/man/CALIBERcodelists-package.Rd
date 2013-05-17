\name{CALIBERcodelists-package}
\alias{CALIBERcodelists-package}
\alias{CALIBERcodelists}

\docType{package}
\title{
Generate ICD10, Read and OPCS codelists
}

\description{
Facilitate the generation, documentation and comparision of codelists for
identifying diagnoses, procedures, investigations and other events recorded
for patients in CALIBER electronic health record data. The CALIBER programme
is funded by the Wellcome Trust [086091/Z/08/Z] and the National Institute
for Health Research (NIHR) under its Programme Grants for Applied Research
programme (RP-PG-0407-10314). The author is supported by a Wellcome Trust
Clinical Research Training Fellowship [0938/30/Z/10/Z].

If using this package for the first time, please read the
\href{../doc/userguide.pdf}{user guide}.

To explore the dictionaries in an interactive way, or convert codelists in another format
to the standard CALIBER format, type \code{demo(explore)} and press ENTER.

To create a Markdown document for defining a new codelist interactively,
type \code{demo(makecodelist)} and press ENTER.

If you are not a CALIBER user, you will need to create your own lookup tables
using \code{demo(loadnewdicts)}.

The CALIBERlookups package contains the International Classification of Diseases
sub-licensed to the Department of Health from the World Health Organization,
and is valid only for NHS and academic use in the United Kingdom, hence it
cannot not be distributed outside the UK academic community.
The Read Terms are licensed under the UK Open
Government Licence. The program code is licensed under GPL-3.
}

\details{

\tabular{ll}{
Package: \tab CALIBERcodelists\cr
Type: \tab Package\cr
Version: \tab 0.2-0\cr
Date: \tab 2013-05-5\cr
License: \tab GPL-3\cr
}

This code list generation package aims to simplify the process of
creating codelists and enable existing codelists to be converted to
a standard format with metadata included.

The file format of the definitive codelist is a comma-separated values file with the following columns:
\itemize{
	\item metadata - quoted text, containing Name: value pairs and the category
	table. The text is padded with spaces to ensure that the categories
	line up. The width of the column is determined by the length of the
	variable names and category levels.
	\item category - the category, right justified by adjusting the amount of
	right padding of the metadata column
	\item icd_code, opcs_code or readcode - the relevant code, quoted text
	\item medcode - only for Read code lists, integer
	\item readterm, icd_term or opcs_term - the relevant term, quoted text
}
These codelists are created by the \code{\link{export}} function.

The rationale for choosing comma as the delimiter is that .csv files
are opened in a spreadsheet by default, making them easy to view and
to add extra columns. They can also be loaded directly into a statistical
package or database, the metadata column can be discarded.

The codelist generation process can be documented in R Markdown, which can be processed by
\code{\link{process}} to produce a HTML document. The function \code{\link{exportall}}
in the Markdown document will cause the new codelists to be exported to the working
directory and produce HTML documentation.

Code lists can be generated via a number of sources:
\itemize{
	\item Using the interactive mode (\code{\link{exploreCodes}})
	\item Existing code lists (\code{\link{as.codelist}})
	\item Search for specific Read, ICD10 or OPCS4 codes (\code{\link{codematch}})
	\item Text search of terms (\code{\link{termhas}})
}

}

\author{
Anoop Shah

Maintainer: Anoop Shah <anoop@doctors.org.uk>
}

\seealso{
Interactive mode: \code{\link{exploreCodes}}

Browsing a selection or codelist as a spreadsheet: \code{\link{browseSelection}}, \code{\link{browseCodelist}}

Creating a selection of terms: \code{\link{termhas}}, \code{\link{codematch}}

Assigning categories to a codelist under development: \code{\link{assigncat}}

Assigning metadata: \code{\link{setMetadata}}

Creating a codelist object:  \code{\link{as.codelist}}

Manipulating codelists:  \code{\link{compare}}, \code{\link{merge.codelist}}, \code{\link{convert}}

Exporting a codelist:  \code{\link{export}}

Processing an R Markdown file containing a codelist definition:  \code{\link{process}}

Master dictionary: \code{\link{CALIBER_DICT}}, \code{\link{CALIBER_DICTMAPS}}

Storage of metadata for codelist under development: \code{\link{META}}
}

\keyword{ package }

\examples{
library(CALIBERcodelists)
setdictionary('read','icd10')
assigncat(2, 'SA|Stable angina', termhas('angina') \%AND\% 
    NOT(termhas('vincent|herpangina|abdominal|ludwig')))
assigncat(3, 'USA|Unstable angina', termhas('unstable|crescendo'), cats_to_convert=2)
assigncat(1, 'Hx|History of angina', termhas('history|h/o'), cats_to_convert=2:3)
assigncat(0, 'Excluded', termhas('^fh|no fh'), cats_to_convert=1:3)
print(as.codelist('read'))
print(as.codelist('icd10'))
}
