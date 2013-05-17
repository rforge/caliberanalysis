\name{export}
\alias{export}
\alias{exportall}

\title{
Export the codelists to the definitive .csv format or .dta (Stata) format
}

\description{
The function \code{export} exports a single codelist, and \code{exportall} exports codelists
from each of the master dictionaries (Read, OPCD, ICD-10) that are in use.
\code{export} exports to a specific file; \code{exportall} exports to a given directory and automatically names the files \code{varname_gprd.codelist.1.csv} (where \code{1} is the version number and \code{gprd} can be replaced by \code{hes} or \code{opcs}) etc.

Terms with category 0 or negative categories are not exported.
}

\usage{
export(codelist, filename, categories = 'all', contractIfICD10 = TRUE)
exportall(directory = getwd(), varname = NULL, saveMessage = TRUE,
	icd10_source = 'hes')
}

\arguments{
  \item{codelist}{
A codelist object created by \code{\link{as.codelist}}.
}
  \item{filename}{
Filename to export to. The file type (comma separated or Stata file) is chosen by the file extension. With an ICD-10 codelist, if the filename contains _ons or _hes, the Source attribute of the codelist is automatically updated.
}
  \item{categories}{
Which categories to export, either the character string 'all' or a numeric vector
}
  \item{contractIfICD10}{
Whether to represent 4-character ICD-10 codes which have the same 3-character prefix using the prefix only. This is recommended because it means that codelists are shorter and easier to understand.
}
  \item{icd10_source}{
Suffix for ICD-10 source name: hes or ons. Either a single string or the vector \code{c('hes', 'ons')} if both are to be exported.
}
  \item{directory}{
Directory to export to. Uses the current working directory if no directory stated.
}
  \item{varname}{
Filename and codename (CALIBER variable name) for the exported codelist. If it is not supplied as an argument, it is obtained from the 'Name' field of the META table. If the codelist selection process is done by calling \code{\link{process}} on a R Markdown file, the variable name is drawn from a line such as "Name|angina" for the angina codelist.
}
  \item{saveMessage}{
If there already exists a file of the same name as the codelist being exported, \code{exportall} compares the code selection in this previous file with the new version being exported. If \code{saveMessage} is TRUE, this comparison is saved in the 'message' field of the META table rather than being printed on screen immediately. When processing a R Markdown file using \code{\link{process}}, this feature is used to prevent the comparison message from being printed in the HTML document, and it is shown on the screen instead after the file has been processed.
}

}
\details{
The function \code{exportall} extracts the selected terms from the master dictionary as up to three codelists (GPRD, HES and OPCS), and calls \code{export} to export each of them as a CSV file.

}
\value{
\code{export} invisibly returns the codelist as a data.table in the format it was exported to .CSV (i.e. no longer a \code{codelist} object). \code{exportall} does not return anything.
}

\seealso{
\code{\link{as.codelist}},  \code{\link{assigncat}},  \code{\link{codematch}},  \code{\link{CALIBER_DICT}}, \code{\link{dictis}}, \code{\link{explode}}, \code{\link{getdictionary}}, \code{\link{setdictionary}}, \code{\link{termhas}}
}

\examples{
# Generate and export a codelist
setdictionary('read', 'icd10', 'opcs')
# Ensure that category labels containing . are exported and
# re-imported correctly
assigncat(3, 'P.C.O.S.', 'polycystic ov|drill.*ovar')
assigncat(2, 'FH: PCOS', 'fh|family', cats_to_convert=3)

setMetadata(Date = '13 Feb 2013')
setMetadata(Author = 'An epidemiologist')
setMetadata(Name = 'pcos')
setMetadata(Version = 0)

pcos_gprd <- as.codelist('read')
pcos_opcs <- as.codelist('opcs')
pcos_hes <- as.codelist('icd10')

# Export codelists in Stata and CSV format
export(pcos_gprd, paste(tempdir(), '/pcos_gprd.codelist.0.csv', sep=''))
export(pcos_opcs, paste(tempdir(), '/pcos_opcs.codelist.0.csv', sep=''))
export(pcos_hes, paste(tempdir(), '/pcos_ons.codelist.0.csv', sep=''))
export(pcos_hes, paste(tempdir(), '/pcos_hes.codelist.0.csv', sep=''))
export(pcos_gprd, paste(tempdir(), '/pcos_gprd.codelist.0.dta', sep=''))
export(pcos_opcs, paste(tempdir(), '/pcos_opcs.codelist.0.dta', sep=''))
export(pcos_hes, paste(tempdir(), '/pcos_ons.codelist.0.dta', sep=''))
export(pcos_hes, paste(tempdir(), '/pcos_hes.codelist.0.dta', sep=''))

# Re-import codelists and check against the originals
csv_gprd <- as.codelist(paste(tempdir(), '/pcos_gprd.codelist.0.csv', sep=''))
csv_opcs <- as.codelist(paste(tempdir(), '/pcos_opcs.codelist.0.csv', sep=''))
csv_hes <- as.codelist(paste(tempdir(), '/pcos_hes.codelist.0.csv', sep=''))
csv_ons <- as.codelist(paste(tempdir(), '/pcos_ons.codelist.0.csv', sep=''))
dta_gprd <- as.codelist(paste(tempdir(), '/pcos_gprd.codelist.0.dta', sep=''))
dta_opcs <- as.codelist(paste(tempdir(), '/pcos_opcs.codelist.0.dta', sep=''))
dta_hes <- as.codelist(paste(tempdir(), '/pcos_hes.codelist.0.dta', sep=''))
dta_ons <- as.codelist(paste(tempdir(), '/pcos_ons.codelist.0.dta', sep=''))

# Check that ONS and HES codelists are identical apart from Source
# attribute
setMetadata(dta_ons, Source='HES')
setMetadata(csv_ons, Source='HES')
setMetadata(dta_ons, Name='pcos_hes')
setMetadata(csv_ons, Name='pcos_hes')
stopifnot(compare(dta_ons, dta_hes)$identical)
stopifnot(compare(csv_ons, csv_hes)$identical)

# Export and re-import
export(dta_gprd, paste(tempdir(), '/dta_gprd.codelist.0.csv', sep=''))
export(dta_opcs, paste(tempdir(), '/dta_opcs.codelist.0.csv', sep=''))
export(dta_hes, paste(tempdir(), '/dta_hes.codelist.0.csv', sep=''))
dta_csv_gprd <- as.codelist(paste(tempdir(), '/dta_gprd.codelist.0.csv', sep=''))
dta_csv_opcs <- as.codelist(paste(tempdir(), '/dta_opcs.codelist.0.csv', sep=''))
dta_csv_hes <- as.codelist(paste(tempdir(), '/dta_hes.codelist.0.csv', sep=''))

# Check if roundtrip export / import produces an identical codelist
stopifnot(compare(csv_gprd, pcos_gprd)$identical)
stopifnot(compare(csv_opcs, pcos_opcs)$identical)
stopifnot(compare(csv_hes, pcos_hes)$identical)
stopifnot(compare(dta_csv_gprd, pcos_gprd)$identical)
stopifnot(compare(dta_csv_opcs,
	removeUnusedCategories(pcos_opcs))$identical)
stopifnot(compare(dta_csv_hes,
	removeUnusedCategories(pcos_hes))$identical)

# Check if codelist can be exported with NULL name
setattr(pcos_gprd, 'Name', NULL)
export(pcos_gprd, paste(tempdir(), '/pcos_gprd.codelist.0.csv', sep=''))

# Clean up temporary directory
unlink(paste(tempdir(), '/pcos_gprd.codelist.0.csv', sep=''))
unlink(paste(tempdir(), '/pcos_opcs.codelist.0.csv', sep=''))
unlink(paste(tempdir(), '/pcos_hes.codelist.0.csv', sep=''))
unlink(paste(tempdir(), '/pcos_gprd.codelist.0.dta', sep=''))
unlink(paste(tempdir(), '/pcos_opcs.codelist.0.dta', sep=''))
unlink(paste(tempdir(), '/pcos_hes.codelist.0.dta', sep=''))
unlink(paste(tempdir(), '/dta_gprd.codelist.0.dta', sep=''))
unlink(paste(tempdir(), '/dta_opcs.codelist.0.dta', sep=''))
unlink(paste(tempdir(), '/dta_hes.codelist.0.dta', sep=''))
}
