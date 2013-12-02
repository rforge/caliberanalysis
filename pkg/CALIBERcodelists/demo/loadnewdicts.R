# Script for replacing source dictionaries used by CALIBERcodelists
# Note that this does not add ICD-9 terms to CALIBER_DICT because there
# is no official source.
library(data.table)

OKtoStart <- menu(c('Start script to create dictionaries', 'Exit'),
	graphics=TRUE,
	title='Creating CALIBER dictionaries.\nWarning: this script may over-write objects in your workspace. Please choose whether to proceed.')

if (OKtoStart==2){
	stop('Exiting script.')
}

message('This script processes source dictionaries supplied by CPRD or the
NHS Terminology Centre, and creates the files CALIBER_DICT.rda, 
CALIBER_PRODDICT.rda and CALIBER_DICTMAPS.rda which are used with the
CALIBERcodelists package.')

message('\nThe new dictionaries will be placed in the data subdirectory of
the current working directory.')

message(paste('\nThe current working directory is:\n', getwd()))

message('\nType exit at any time to stop this script.')

needToMakeDataFolder <- !(any(grepl('[\\\\/]data$', 
	list.dirs(getwd(), recursive=FALSE))))

if (needToMakeDataFolder){
	message('Creating a new directory "data" in the current working directory')
	dir.create('data')
}

#### OBTAIN FILE LOCATIONS ####

askMe <- function(prompt, default, isFile = TRUE){
	found <- FALSE
	while (found == FALSE){
		if (isFile == FALSE){
			found <- TRUE
		}
		message(paste('\nPlease enter', prompt, '\n(default = ', default, ')\n'))
		answer <- readline()
		if (tolower(answer)=='exit'){
			stop("Exiting script.", call.=FALSE)
		} else if (answer==''){
			answer <- default	
		}
		if (isFile == TRUE){
			if (file.exists(answer)){
				message(paste('Loading', answer))
				found <- TRUE
			} else {
				message(paste(answer,
					'not found. Please try again, or type EXIT to abort.'))
			}
		}
	}
	answer
}

pegasusMedicalFilename <- askMe(
	"filename of CPRD Pegasus Medical Dictionary",
	"pegasus_medical.txt")
pegasusProductFilename <- askMe(
	"filename of CPRD Pegasus Product Dictionary",
	"pegasus_product.txt")
icdFilename <- askMe(
	"filename of tab-separated file with ICD-10 codes and terms",
	"ICD10_Edition4_CodesAndTitlesAndMetadata_GB_20120401.txt")
icdVersion <- askMe(
	"version name for ICD-10",
	"ICD-10 codes, terms and text used by permission of WHO, from: International Statistical Classification of Diseases and Related Health Problems, Tenth Revision (ICD-10). Vols 1-3. Geneva, World Health Organization, 1992-2000. 4th Edition.",
	isFile = FALSE)
opcsFilename <- askMe(
	"filename of tab-separated file with OPCS codes and terms",
	"OPCS46 CodesAndTitles Jan 2011 V1.0.txt")
opcsVersion <- askMe(
	"version name for OPCS",
	"Office of Population Censuses and Surveys Codes 4.6, Jan 2011 V1.0, NHS Classifications Service", isFile = FALSE)
icdMapFilename <- askMe(
	"filename of mapping of ICD-10 codes to Read codes, pipe delimited text file",
	"Icd10.v3")
icd9MapFilename <- askMe(
	"filename of mapping of ICD-9 codes to Read codes, pipe delimited text file",
	"Icd9.v3")
opcsMapFilename <- askMe(
	"filename of mapping of OPCS codes to Read codes, pipe delimited text file",
	"Opcs4.v3")
mapVersion <- askMe(
	"version name for mappings",
	"NHS UK Read Codes Clinical Terms Version 3, Release March 2013, UK Terminology Centre", isFile = FALSE)

#### PEGASUS PRODUCT DICTIONARY (GPRD PROD) ####
# There are two extra lines at the beginning of the Pegasus
# Product dictionary
prod <- read.delim(pegasusProductFilename,
	skip=2, sep='\t', header=FALSE, as.is=TRUE, quote=NULL)
names(prod) <- c('prodcode', 'multilex', 'events', 'prodname',
	'drugsubstance', 'strength', 'formulation', 'route', 
	'bnfcode', 'bnfheader', 'db')	
CALIBER_PRODDICT <- data.table(prod, key='prodcode')
prodDate <- CALIBER_PRODDICT[which.max(as.Date(
	paste('1', db), format='%d %B %Y')), db]
setattr(CALIBER_PRODDICT, 'VERSION', paste('CPRD Pegasus Product Dictionary,', prodDate))

#### PEGASUS MEDICAL DICTIONARY (GPRD READ) ####
# There are two extra lines at the beginning of the Pegasus
# Medical dictionary
pegasus <- read.delim(pegasusMedicalFilename,
	skip=2, sep='\t', header=FALSE, as.is=TRUE, quote=NULL)
names(pegasus) <- c('medcode', 'code', 'ev1', 'ev2', 'ev3', 'ev4', 'term', 'db')	
pegasusDate <- pegasus[which.max(as.Date(
	paste('1', pegasus$db), format='%d %B %Y')), 'db']
pegasus <- data.table(pegasus)
# Remove blank terms with zero events
pegasus <- pegasus[events > 0 | term != '']
pegasus[, events:=ev1 + ev2 + ev3 + ev4]
# Note that quotes are doubled because it is
# a Microsoft VB format file
pegasus[, term:=gsub('""', '"', term)]
dict.read <- pegasus[, list(code, term, dict='read', events, medcode)]
# Preparing for crossmaps (note that maps are made by Read code and then
# mapped by term to medcode). Ignore Read codes without a term:
read5toTerm <- dict.read[term!='', list(read5=substr(code, 1, 5),
	termlc=sub('^\\[[sodqvx]+\\]', '', tolower(term)))]
setkey(read5toTerm, read5)
setnames(read5toTerm, 'termlc', 'term')

#### ICD-10 DICTIONARY ####
icd10 <- data.table(read.delim(icdFilename, as.is=TRUE, sep='\t'))
icd10[, code:=ALT_CODE]
icd10[, term:=DESCRIPTION]
icd10[MODIFIER_4!='', term:=paste(term, '; ', MODIFIER_4)]
icd10[MODIFIER_5!='', term:=paste(term, '; ', MODIFIER_5)]

correct <- function(newcode, newterm){
	message(paste('Converting', icd10[code==newcode, term], 'to', newterm))
	invisible(icd10[code==newcode, term:=newterm])
}

# Non-ASCII character codes to be converted to ASCII
correct('C841', "Sezary disease")
correct('C880', "Waldenstrom macroglobulinaemia")
correct('D51', 'Vitamin B12 deficiency anaemia')
correct('D510', 'Vitamin B12 deficiency anaemia due to intrinsic factor deficiency')
correct('D511', 'Vitamin B12 deficiency anaemia due to selective vitamin B12 malabsorption with proteinuria')
correct('D513', 'Other dietary vitamin B12 deficiency anaemia')
correct('D518', 'Other vitamin B12 deficiency anaemias')
correct('D519', 'Vitamin B12 deficiency anaemia, unspecified')
correct('E672', "Megavitamin-B6 syndrome")
correct('E750', "GM2 gangliosidosis")
correct('G375', "Concentric sclerosis [Balo)")
correct('G610', "Guillain-Barre syndrome")
correct('H810', "Meniere disease")
correct('L705', "Acne excoriee des jeunes filles")
correct('L813', "Cafe au lait spots")
correct('M911', "Juvenile osteochondrosis of head of femur [Legg-Calve-Perthes]")
correct('M931', "Kienbock's disease of adults")
correct('M350', 'Sicca syndrome [Sjogren]')
correct('M352', "Behcet's disease")
correct('T470', "Poisoning: Histamine H2-receptor antagonists")
correct('Y441', 'Vitamin B12, folic acid and other anti-megaloblastic-anaemia preparations')
correct('Y530', "Histamine H2-receptor antagonists")

# Category icdhead is for ICD 3 character codes
# i.e. these are not used in HES or ONS coding
icd10[, dict:=ifelse(ALT_CODE==CODE, 'icdhead', 'icd10')]
dict.icd10 <- icd10[, list(code, term, dict)]
dict.icd10[, events:=NA_integer_]
dict.icd10[, medcode:=NA_integer_]

# Add extra header terms for ONS ICD-10 codes for those ending in X
# (e.g. stroke NOS I64X)
dict.icdhead <- dict.icd10[grepl('X$', code), ]
dict.icdhead[, code:=sub('X$', '', code)]
dict.icdhead[, dict:='icdhead']
dict.icdhead[, events:=NA_integer_]
dict.icdhead[, medcode:=NA_integer_]

#### OPCS ####
opcs <- read.delim(opcsFilename, header=FALSE, sep='\t', as.is=TRUE)
names(opcs) <- c('code', 'term')
dict.opcs <- data.table(opcs)
dict.opcs[, code:=gsub('\\.', '', code)]
dict.opcs[, dict:='opcs']
dict.opcs[, events:=NA_integer_]
dict.opcs[, medcode:=NA_integer_]

#### COMBINED DICTIONARY ####
CALIBER_DICT <- rbind(dict.read, dict.opcs, dict.icd10,
	dict.icdhead, use.names=TRUE)
CALIBER_DICT[, termlc:=sub('^\\[[sodqvx]+\\]', '', tolower(term))]
# Convert quotation marks to single quotes (for simplicity)
CALIBER_DICT[, term:=gsub('"', "'", term)]
CALIBER_DICT[, category:=NA_integer_]
setkey(CALIBER_DICT, dict, code)
setattr(CALIBER_DICT, 'VERSION_READ',
	paste('CPRD Pegasus Medical Dictionary,', pegasusDate))
setattr(CALIBER_DICT, 'VERSION_ICD10', icdVersion)
setattr(CALIBER_DICT, 'VERSION_OPCS', opcsVersion)

#### MAPPINGS READ TO OPCS, ICD9 AND ICD10 ####
map.read.icd10 <- read.delim(icdMapFilename,
	sep='|', as.is=TRUE, header=FALSE)
map.read.icd9 <- read.delim(icd9MapFilename,
	sep='|', as.is=TRUE, header=FALSE)
map.read.opcs <- read.delim(opcsMapFilename,
	sep='|', as.is=TRUE, header=FALSE)
names(map.read.opcs) <- names(map.read.icd10) <- names(map.read.icd9) <-
	c('read5', 'code', 'map_stat', 'ref_flag',
	'add_flag', 'elem_num', 'block_num')

# Merge with GPRD Read dictionary
map.read.icd10 <- data.table(map.read.icd10, key='read5')
map.read.icd9 <- data.table(map.read.icd9, key='read5')
map.read.opcs <- data.table(map.read.opcs, key='read5')
map.icd10 <- merge(map.read.icd10, read5toTerm, by='read5')
map.icd9 <- merge(map.read.icd9, read5toTerm, by='read5')
map.opcs <- merge(map.read.opcs, read5toTerm, by='read5')

# Dictionary names
map.icd10[nchar(code)==3, dict:='icdhead']
map.icd10[nchar(code)>3, dict:='icd10']
map.icd9[, dict:='icd9']
map.opcs[, dict:='opcs']

# Map from Read code to medcode via Read term (so that
# identical Read terms with different codes receive the same ICD-10
# or OPCS mapping).
READ <- CALIBER_DICT[dict=='read']
READ[, term:=sub('^\\[[sodqvx]+\\]', '', tolower(term))]
MAP_ICD10 <- merge(map.icd10, READ[, list(medcode, term)], by='term')
MAP_ICD10[, term:=NULL]
MAP_ICD9 <- merge(map.icd9, READ[, list(medcode, term)], by='term')
MAP_ICD9[, term:=NULL]
MAP_OPCS <- merge(map.opcs, READ[, list(medcode, term)], by='term')
MAP_OPCS[, term:=NULL]

# Create additional exact term maps with category 'T'
# (as these do not currently exist for some terms)
ICD <- CALIBER_DICT[dict %in% c('icd10', 'icdhead')]
ICD[, term:=sub('^\\[[sodqvx]+\\]', '', tolower(term))]
OPCS <- CALIBER_DICT[dict=='opcs']
OPCS[, term:=sub('^\\[[sodqvx]+\\]', '', tolower(term))]
READ_OPCS <- merge(READ, OPCS, by='term')[, list(read5='', code=code.y,
	map_stat='T', ref_flag='', add_flag='', elem_num=0,
	block_num=0, medcode=medcode.x, dict=dict.y)]
cat('\nExact term maps (non case sensitive) between Read and OPCS:\n')
print(READ_OPCS)
READ_ICD <- merge(READ, ICD, by='term')[, list(read5='', code=code.y,
	map_stat='T', ref_flag='', add_flag='', elem_num=0,
	block_num=0, medcode=medcode.x, dict=dict.y)]
cat('\nExact term maps (non case sensitive) between Read and ICD-10:\n')
print(READ_ICD)

CALIBER_DICTMAPS <- rbind(MAP_ICD10, MAP_ICD9, MAP_OPCS,
	READ_OPCS, READ_ICD, use.names=TRUE)
CALIBER_DICTMAPS[, read5:=NULL]
# Remove duplicates
CALIBER_DICTMAPS <- copy(CALIBER_DICTMAPS[!duplicated(CALIBER_DICTMAPS)])
# Final version
setkey(CALIBER_DICTMAPS, dict, code, medcode)
setattr(CALIBER_DICTMAPS, 'VERSION', mapVersion)

#### SAVING DICTIONARIES IN DATA FOLDER ####

message('Loaded dictionaries, now saving to file.')

saveIfOK <- function(objectname, filepath){
	# Check if file already exists
	if (file.exists(filepath)){
		message(paste('There already exists a file', filepath))
		OKtoSave <- NA
		while (is.na(OKtoSave)){
			responseText <- readline('Overwrite? Please answer Yes or No.')
			if (responseText %in% c('Y', 'Yes', 'YES', 'y', 'yes')){
				OKtoSave <- TRUE
			}
			if (responseText %in% c('N', 'No', 'NO', 'n', 'no')){
				OKtoSave <- FALSE
			}
			if (tolower(responseText)=='exit'){
				stop("Exiting script.", call.=FALSE)
			}
		}
	} else {
		OKtoSave <- TRUE
	}
	if (OKtoSave){
		message(paste('Saving to', filepath))
		assign(objectname, get(objectname, envir=.GlobalEnv))
		save(list=objectname, file=filepath)
	}
}

saveIfOK('CALIBER_DICT', file='data/CALIBER_DICT.rda')
saveIfOK('CALIBER_PRODDICT', file='data/CALIBER_PRODDICT.rda')
saveIfOK('CALIBER_DICTMAPS', file='data/CALIBER_DICTMAPS.rda')

# Check loading of CALIBER_DICT
rm(CALIBER_DICT)
load('data/CALIBER_DICT.rda')
setkey(CALIBER_DICT, dict, code)

# Check loading of CALIBER_DICTMAPS
rm(CALIBER_DICTMAPS)
load('data/CALIBER_DICTMAPS.rda')

message('Dictionaries can now be loaded using data(CALIBER_DICT),
data(CALIBER_PRODDICT) or data(CALIBER_DICTMAPS) as long as the
dictionary files are in the data subdirectory of your current
working directory.')

# Remove functions. Keep tables in case the user wants to investigate them.
rm(correct)
rm(saveIfOK)
rm(askMe)
# End of script
