# Script for replacing source dictionaries used by CALIBERcodelists

OKtoStart <- menu(c('Start script to create lookup tables', 'Exit'),
	graphics=TRUE,
	title='Creating CALIBER lookups.\nWarning: this script may over-write objects in your workspace. Please choose whether to proceed.')

if (OKtoStart==2){
	stop('Exiting script.')
}

message('This script processes lookup tables supplied by the
Clinical Practice Research Datalink, and creates the files:
CALIBER_LOOKUPS.rda
CALIBER_ENTITY.rda
CALIBER_BNFCODES.rda
CALIBER_PACKTYPE.rda
CALIBER_SCOREMETHOD.rda

The lookups and entity files are used by the CALIBERdatamanage package.')

message('\nThe new Rdata files will be placed in the data subdirectory of
the current working directory.')

message(paste('\nThe current working directory is:\n', getwd()))

message('\nType exit at any time to stop this script.')

needToMakeDataFolder <- !(grepl('[\\\\/]data$', 
	list.dirs(getwd(), recursive=FALSE)))

if (needToMakeDataFolder){
	message('Creating a new directory "data" in the current working directory')
	dir.create('data')
}

#### OBTAIN FILE LOCATIONS ####

askMe <- function(prompt, default){
	found <- FALSE
	while (found==FALSE){
		message(paste('\nPlease enter', prompt, '\n(default = ', default, ')\n'))
		answer <- readline()
		if (tolower(answer)=='exit'){
			stop("Exiting script.", call.=FALSE)
		} else if (answer==''){
			answer <- default	
		} 
		if (file.exists(answer)){
			message(paste('Loading', answer))
			found <- TRUE
		} else {
			message(paste(answer,
				'not found. Please try again, or type EXIT to abort.'))
		}
	}
	answer
}


TXTFILES <- askMe(
	"name of directory containing text files with data lookups",
	"TXTFILES")
ENTITY <- askMe(
	"filename of entity type table (tab separated text file)",
	"Entity.txt")
PACKTYPE <- askMe(
	"filename of tab-separated file with pack types",
	"packtype.txt")
BNFCODES <- askMe(
	"filename of tab-separated file with BNF codes",
	"bnfcodes.txt")
SCOREMETHOD <- askMe(
	"filename of tab-separated file with clinical score method",
	"scoremethod.txt")

#### TEXT FILE LOOKUPS ####

lookup_names <- dir(TXTFILES)
lookups <- list()
for (file in lookup_names){
	temp <- read.delim(paste(TXTFILES, file, sep='/'),
		sep='\t', as.is=TRUE)
	names(temp) <- c('category', 'description')
	lookupname <- sub('\\.TXT|\\.txt', '', file)
	temp$lookup <- lookupname
	lookups[[lookupname]] <- temp
}
CALIBER_LOOKUPS <- data.table(do.call('rbind', lookups))
setcolorder(CALIBER_LOOKUPS, c('lookup', 'category', 'description'))
setkey(CALIBER_LOOKUPS, lookup, category)

#### OTHER LOOKUPS ####
CALIBER_ENTITY <- data.table(read.delim(ENTITY, as.is=TRUE),
	key='enttype')

CALIBER_PACKTYPE <- data.table(read.delim(PACKTYPE, as.is=TRUE),
	key='packtype')
# Remove non-ASCII codes
require(tools)
cat('\nNon-ASCII descriptions:\n')
capture.output(isNonASCII <- CALIBER_PACKTYPE[, packtype_desc %in%
	showNonASCII(CALIBER_PACKTYPE$packtype_desc)]) -> GARBAGE
rm(GARBAGE)
print(CALIBER_PACKTYPE[isNonASCII])
CALIBER_PACKTYPE[is.na(packtype_desc), packtype_desc:='']
tabletText <- sapply(CALIBER_PACKTYPE$packtype_desc, function(x){
	as.character(try(substr(x, 1, 9)=='TABLET(S)'))
})
# Remove all packtype descriptions with non-ASCII text except if it has
# 'TABLET(S)'
CALIBER_PACKTYPE[isNonASCII, packtype_desc:='']
CALIBER_PACKTYPE[isNonASCII & (tabletText=='TRUE'),
	packtype_desc:='TABLET(S)']
cat('\nAfter removal of non-ASCII characters:\n')
print(CALIBER_PACKTYPE[isNonASCII])

CALIBER_BNFCODES <- data.table(read.delim(BNFCODES,
	colClasses=c('integer', 'character'), as.is=TRUE),
	key='bnf')

CALIBER_SCOREMETHOD <- data.table(read.delim(SCOREMETHOD, as.is=TRUE),
	key='code')

#### SAVE TO FILE ####

message('Loaded lookup tables, now saving to file.')

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

saveIfOK('CALIBER_LOOKUPS', file='data/CALIBER_LOOKUPS.rda')
saveIfOK('CALIBER_ENTITY', file='data/CALIBER_ENTITY.rda')
saveIfOK('CALIBER_PACKTYPE', file='data/CALIBER_PACKTYPE.rda')
saveIfOK('CALIBER_BNFCODES', file='data/CALIBER_BNFCODES.rda')
saveIfOK('CALIBER_SCOREMETHOD', file='data/CALIBER_SCOREMETHOD.rda')

# End of script
