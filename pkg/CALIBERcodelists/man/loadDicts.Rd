\name{loadDicts}
\alias{loadDicts}

\title{
Script to replace the source dictionaries used by CALIBERcodelists
}

\description{
This script is called using the command \code{demo(loadnewdicts)}.

The scripts replaces the existing dictionaries in your installed
version of the CALIBERcodelists package.
It loads terminology dictionaries from text files, converts them
into a standardised format and saves them in the data folder of the
CALIBERcodelists package (or elsewhere if specified).

It is easiest to place all the relevant source dictionary files in one
directory and make it the working directory (using \code{setwd}) before
running this script.
}


\details{
\itemize{
  \item{pegasusMedicalFilename}{
filename of CPRD Pegasus Medical Dictionary, default "pegasus_medical.txt".
This is a tab-separated file with metadata in the first two rows.
}
  \item{pegasusProductFilename}{
filename of CPRD Pegasus Product (drug) Dictionary, default "pegasus_product.txt".
This is a tab-separated file with metadata in the first two rows.
}
  \item{icdFilename}{
filename of ICD-10 file, default "ICD10_Edition4_CodesAndTitlesAndMetadata_GB_20120401.txt"
(located within the Content folder of the NHS ICD-10 zip file nhs_icd10_1.0.0_20120401000001.zip)
}
  \item{icdVersion}{
version of the ICD-10 dictionary used, to add as an attribute. Default "ICD-10 codes, terms and text used by permission of WHO, from: International Statistical Classification of Diseases and Related Health Problems, Tenth Revision (ICD-10). Vols 1-3. Geneva, World Health Organization, 1992-2000. 4th Edition."
}
  \item{opcsFilename}{
filename of OPCS file, default "OPCS46 CodesAndTitles Jan 2011 V1.0.txt", supplied together with
documentation in a zip file (nhs_opcs4_6.0.0_20110110000001.zip)
}
  \item{opcsVersion}{
version of the OPCS file used, default "Office of Population Censuses and Surveys Codes 4.6, Jan 2011 V1.0, NHS Classifications Service".
}
  \item{icdMapFilename}{
filename of the ICD-10 map file, default "Icd10.v3", in the V3 folder of the
Read release (supplied as a zip file e.g. nhs_readctv3_14.0.0_20121001000001.zip).
This is a pipe-separated text file with no header row.
}
  \item{opcsMapFilename}{
filename of the OPCS map file, default "Opcs.v3", in the V3 folder of the
Read release (supplied as a zip file e.g. nhs_readctv3_14.0.0_20121001000001.zip).
This is a pipe-separated text file with no header row.
}
  \item{mapVersion}{
version of the Read Clinical Terms from which the OPCS and ICD-10 maps
were obtained, default "NHS UK Read Codes Clinical Terms Version 3, Release October 2012, UK Terminology Centre".
}
  \item{libraryDataPath}{
path to the 'data' folder within the installed CALIBERcodelists package, 
which must end in CALIBERcodelists/data/. The package is supplied with
dummy dictionaries (CALIBER_DICT, PRODCALIBER_DICT, CALIBER_DICTMAPS) which enable the tests to work, but
should be replaced by the actual dictionaries before using the package.
}
}

The ICD-10, OPCS and GPRD Read dictionaries are combined into a single data.table \code{\link{CALIBER_DICT}},
and the mappings between Read and OPCS / ICD-10 are combined into the data.table \code{\link{CALIBER_DICTMAPS}}.
The link between these two data.tables is by medcode or ICD-10 / OPCS code.

Full filepaths must be supplied for each of the arguments.

Version statements are added to the dictionaries as attributes VERSION if the table is derived
from only one source, or VERSION_READ, VERSION_ICD10, VERSION_OPCS if it combines more than one
source. For the Pegasus dictionaries, the version date is the most recent date in the 
database date column.
}

\references{
Clinical Practice Research Datalink, \url{cprd.com}.

International Statistical Classification of Diseases and Related
Health Problems, Tenth Revision (ICD-10).  Vols 1-3. Geneva,
World Health Organization, 1992-2000. 4th Edition,
\url{http://apps.who.int/classifications/icd10/browse/2010/en}
}

\author{
Anoop Shah
}

\note{
The CPRD Pegasus dictionaries are supplied with datasets from the General Practice
Research Database (GPRD). The Pegasus Medical dictionary contains the subset of Read codes
that are actually used by general practitioners contributing to GPRD.

Other dictionaries are obtained from the UK Terminology Centre
(\url{https://www.uktcregistration.nss.cfh.nhs.uk/trud3/}).

These dictionaries are not supplied as part of the package because of licensing
issues. For UK NHS CALIBER researchers, we can provide
precompiled dictionaries from the CALIBER co-ordinating centre if desired.

This function may need to be modified in the future if the formats of the
source dictionaries change.
}


\seealso{
\code{\link{setdictionary}}
}

