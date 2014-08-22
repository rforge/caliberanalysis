# Standard column order for codelist:
# CODELIST_COLORDER <- c('category', 'code', 'term')
# medcode last for Read terms, any other columns after

codelist <- function(x=NULL, ...){
	# Creates an object of class 'codelist' with metadata
	# and checks that all terms included in the codelist are in CALIBER_DICT
	# Arguments: x - a dictionary name (read, opcs, icd10), a
	#                filename of a CSV, Stata or tab delimited text file,
	#                a selection object, or a data.frame of terms.

	# Check that the master dictionary is loaded
	loadDICT()
	
	if (is.null(x)){
		if (length(getdictionary()) == 1){
			x <- getdictionary()
		} else {
			x <- select.list(ALLDICTS, multiple=FALSE,
				title = 'Which dictionary to extract codes from?')
		}
		cat('\nExtracting codelist from ' %&% x %&%
			' with non-missing categories.\n')
		return(dictToCodelist(x))
	} else if (is.selection(x)){
		return(selectionToCodelist(x, ...))
	} else if (is.codelist(x)){
		return(x)	
	} else if (is.data.frame(x)){
		return(tableToCodelist(x, ...))
	} else if (is.character(x) & length(x)==1){
		if (x %in% ALLDICTS){
			cat('\nExtracting codelist from terms in ' %&% x %&%
				' with non-missing categories.\n')
			return(dictToCodelist(x))
		} else {
			temp <- NULL
			if (grepl('.dta$', x)){
				temp <- importDTA(x)
			} else if (grepl('.csv$', x)){
				temp <- importCSVcodelist(x, ...)
			} else if (grepl('.txt$', x)){
				temp <- read.delim(x, as.is=TRUE, sep='\t', ...)
			}
			if (is.null(temp)){
				stop('Unable to load codelist from file')
			} else {
				return(tableToCodelist(temp))
			}
		}
	}
}

importDTA <- function(x){
	# Imports Stata DTA file, automatically setting
	# factor levels 0 and negative to NA
	# and restoring the correct labels using the
	# label.table attribute
	x <- read.dta(x, convert.factors = FALSE)
	
	# Now loop through all variables, converting to factors as
	# appropriate
	if (!is.null(attr(x, 'label.table'))){
		for (i in names(attr(x, 'label.table'))){
			newlevels <- attr(x, 'label.table')[[i]]
			newlabels <- names(attr(x, 'label.table')[[i]])
			if (max(newlevels) > 0){
				keeplevels <- 1:max(newlevels)
				# Arrange the labels in ascending numerical order
				keeplabels <- sapply(keeplevels, function(z){
					if (any(newlevels==z)){
						newlabels[which(newlevels==z)]
					} else {
						z
					}
				})
				# Find out which columns this applies to
				colnums <- which(attr(x, 'val.labels')==i)
				if (length(colnums) > 0){
					for (col in colnums){
						x[, col] <- factor(x[, col], levels=keeplevels,
							labels=keeplabels)
					}
				}
			}
		}
	}
	x
}

dictToCodelist <- function(dictionary){
	# Extracts all categories with positive numbers into a codelist.
	if (length(dictionary)!=1){
		stop('A codelist can contain terms from only one dictionary')
	}
	if (!(dictionary %in% ALLDICTS)){
		stop('dictionary should be in ' %&% paste(ALLDICTS, collapse='; '))
	}
	metadata <- extractMetadataFromMETA(dictionary)
	if (dictionary=='read'){
		temp <- copy(CALIBER_DICT[dict==dictionary & category>=0,
			list(category, code, term, medcode, events)])
	} else {
		temp <- copy(CALIBER_DICT[dict==dictionary & category>=0,
			list(category, code, term)])
	}

	addAttributesToCodelist(temp, metadata)
	# Set the first 3 columns to be category, code, term
	setcolorder(temp, c(CODELIST_COLORDER,
		setdiff(names(temp), CODELIST_COLORDER)))
	class(temp) <- c('codelist', 'data.table', 'data.frame')
	# All exported codelists are sorted by category, then code
	setkey(temp, category, code)
	return(temp)
}

selectionToCodelist <- function(x, category=1,
	dictionary=NULL){
	# Converts a selection to a codelist object
	# Uses metadata from META
	# Uses the current dictionary unless more than one is in use
	# and the terms come from more than one dictionary, in which
	# case the user is asked which dictionary to use.
	x <- as.selection(x)
	
	# Swap names
	newcategory <- category
	rm(category)
	
	if (is.null(dictionary)){
		dictionary <- getdictionary()
	}

	getDict <- unique(sub('icdhead', 'icd10', unique(CALIBER_DICT[x, dict])))
	if (!is.null(dictionary)){
		getDict <- intersect(dictionary, getDict)
	}
	if (length(getDict)>1){
		getDict <- intersect(getDict, getdictionary())
	}
	if (length(getDict)>1){
		stop('Terms come from more than one dictionary')	
	} else if (length(getDict)==0) {
		warning('Empty codelist')
		x <- copy(CALIBER_DICT[FALSE,
			list(category=integer(0), code, term, medcode, events)])
		getDict <- dictionary[1]
	} else {
		cat('\nUsing terms from ' %&% getDict)
		x <- copy(CALIBER_DICT[x==TRUE & dict==getDict,
			list(category=newcategory, code, term, medcode, events)])
	} 
	
	if (getDict!='read'){
		# medcode is irrelevant
		x[, medcode:=NULL]
		# number of events not available for ICD-10 or OPCS
		x[, events:=NULL]
	}

	# Ensure that category is registered in META
	temp <- retrieveCategoriesFromMETA()
	if (nrow(temp)==0){
		addCategory(newcategory)
	} else if (!(newcategory %in% temp$category)){
		addCategory(newcategory)
	}
	
	metadata <- extractMetadataFromMETA(getDict)
	
	x <- addAttributesToCodelist(x, metadata)
	class(x) <- c('codelist', 'data.table', 'data.frame')
	
	# Expand if ICD-9 or ICD-10 codelist
	if (getDict %in% c('icd9', 'icd10')){
		if (!(isExpandedCodelist(x))){
			x <- copy(expandCodelist(x))
		}
	}
	# All exported codelists are sorted by category, then code
	setkey(x, category, code)
	setcolorder(x, c(CODELIST_COLORDER,
		setdiff(names(x), CODELIST_COLORDER)))
	return(x)	
}

tableToCodelist <- function(x, noisy = FALSE){
	# Converts a table (data.frame, data.table or list)
	# to a codelist. Maps on medcode for Read codelists, 
	# otherwise on ICD-10 or OPCS code.
	# Arguments: x - a codelist in the standard format, or a 
	#                data.frame or data.table
	if (ncol(x)==0){
		stop('No columns in table')
	}
	
	# Before converting to a data.table, extract useful 
	# metadata
	if (!is.null(attr(x, 'datalabel'))){
		datalabel <- attr(x, 'datalabel')
	} else {
		datalabel <- ''
	}
	if (!is.null(attr(x, 'time.stamp'))){
		time.stamp <- attr(x, 'time.stamp')
	} else {
		time.stamp <- ''
	}
	x <- copy(data.table(x))
	
	# Remove duplicates
	x <- copy(x[!duplicated(x)])
	
	#### STANDARDISE COLUMN NAMES ####
	stdNames <- function(ideal, search1, search2, names){
		# Arguments: ideal = name to convert to
		#            search1 = what to search for in name (e.g. gprd)
		#            search2 = second thing to search for (e.g. code)
		#            names = vector of all column  names
		if (ideal %in% names){
			names
		} else {
			chosen <- grepl(search1, names) & grepl(search2, names)
			# Only convert if exactly one column name matches
			if (sum(chosen) == 1) {
				names[chosen] <- ideal
			}
			names
		}
	}
	newnames <- names(x)
	newnames <- stdNames('readcode',
		'^Read|^READ|^read', 'code|Code|CODE', newnames)
	newnames <- stdNames('icd9_code',
		'^Icd9|^ICD9|^icd9', 'code|Code|CODE', newnames)
	newnames <- stdNames('icd_code',
		'^Icd[^9]|^ICD[^9]|^icd[^9]', 'code|Code|CODE', newnames)
	newnames <- stdNames('opcs_code',
		'^Opcs|^OPCS|^opcs', 'code|Code|CODE', newnames)
	newnames <- stdNames('prodcode',
		'^Prod|^PROD|^prod', 'code|Code|CODE', newnames)

	newnames <- stdNames('readterm',
		'^Read|^READ|^read', 'term|Term|TERM', newnames)
	newnames <- stdNames('icd9_term',
		'^Icd9|^ICD9|^icd9', 'term|Term|TERM', newnames)
	newnames <- stdNames('icd_term',
		'^Icd[^9]|^ICD[^9]|^icd[^9]', 'term|Term|TERM', newnames)
	newnames <- stdNames('opcs_term',
		'^Opcs|^OPCS|^opcs', 'term|Term|TERM', newnames)
	newnames <- stdNames('prodname',
		'^Prod|^PROD|^prod', 'name|Name|NAME', newnames)

	newnames <- stdNames('multilex',
		'^Multi[Ll]ex|^MULTILEX', '', newnames)	
	newnames <- stdNames('medcode',
		'^Med[Cc]ode|^MEDCODE', '', newnames)
	newnames <- stdNames('readcode',
		'^READ$|^[Rr]ead$', '', newnames)
	newnames <- stdNames('icd9_code',
		'^ICD9$|^[Ii]cd9$', '', newnames)
	newnames <- stdNames('icd_code',
		'^ICD$|^[Ii]cd$', '', newnames)
	newnames <- stdNames('opcs_code',
		'^OPCS$|^[Oo]pcs$', '', newnames)
	setnames(x, names(x), newnames)

	#### MEDCODE COLUMN ####
	if ('medcode' %in% names(x)){
		# Is it a genuine medcode column?
		if (all(is.na(x$medcode))){
			x[, medcode:=NULL]
		} else if (is.character(x$medcode)){
			x[, medcode:=NULL]
		}
	}
	
	#### LOWER CASE TERM (DELETE COLUMN) ####
	if ('termlc' %in% names(x)){
		x[, termlc:=NULL]
	}
	
	#### CATEGORIES ####
 	if (!('category' %in% names(x))){
		# try to guess the category name. Exclude column names which
		# are commonly found in codelists.
		catname <- setdiff(names(x), c('medcode', 'readcode', 'events',
			'readterm', 'description', 'shortname', 'clin_events',
			'termlc', 'ref_events', 'test_events', 'immun_events',
			'metadata', 'icd', 'icd_code', 'icd_term', 'icd9_code',
			'icd9_term', 'opcs_code', 'opcs_term',
			'code', 'term', 'db_date', 'lc_readterm', "prodcode",
			"multilex", "events", "prodname", "drugsubstance",
			"strength", "formulation", "route", "bnfcode", "bnf",
			"bnfheader", "db", "therapyevents", "drugsubstancename",
			"substancestrength", "formulation", "routeofadministration",
			"databasebuild", "clinicalevents", "referralevents",
			"testevents", "immunisationevents"))[1]
		if (is.na(catname)){
			# create a category with 1
			if (nrow(x) > 0){
				x[, category:=1L]
			}
			catname <- 'category'
		}
		if (nrow(x) > 0){
 			setnames(x, catname, 'category')
		}
 	}

	# Convert the category column to numbers if not already done so
	if (is.character(x$category) | is.factor(x$category)){
		categories <- splitCategory(x$category)
		x[, category:=categories$category]
		categories <- categories[!duplicated(categories)][order(category)]
	} else if ('shortname' %in% names(x) & !('description' %in% names(x))){
		temp <- x[, list(category, shortname, description=shortname)][
			order(category)]
		categories <- temp[!duplicated(temp)]
	} else if ('description' %in% names(x) & !('shortname' %in% names(x))){
		temp <- x[, list(category, shortname=description, description)][
			order(category)]
		categories <- temp[!duplicated(temp)]
	} else if ('description' %in% names(x) & 'shortname' %in% names(x)){
		temp <- x[, list(category, shortname, description)][order(category)]
		categories <- temp[!duplicated(temp)]
	} else {
		# all category labels are blank
		if (nrow(x)==0){
			categories <- data.table(category=integer(0),
				shortname=character(0), description=character(0))
		} else if (all(is.na(x$category))) {
			categories <- data.table(category=integer(0),
				shortname=character(0), description=character(0))	
		} else {
			categories <- data.table(category=sort(unique(x$category)),
				shortname='', description='')			
		}
	}

	# Remove duplicate categories
	allcats <- unique(categories$category)
	setkey(categories, category)
	categories <- copy(categories[J(allcats), , mult='first'])

	#### METADATA ####
	if ('metadata' %in% names(x)){
		metadata <- extractMetadataFromColumn(x$metadata)
		x[, metadata:=NULL]
	} else if (datalabel != '') {
		# Stata metadata in datalabel
		metadata <- extractMetadataFromDatalabel(datalabel, time.stamp)
	} else {
		# no metadata
		metadata <- list()
	}

	# Add category table
	if (is.null(metadata$Categories)){
		metadata$Categories <- categories
	}
	
	# metadata$Source cannot be NULL
	if (is.null(metadata$Source)){
		metadata$Source <- ''
	}

	# Standardise column names
	if ('prodcode' %in% colnames(x)){
		# This is a product codelist
		# multilex == code (column renamed 'multilex' on export)
		# prodname == term (column renamed on export)
		# (prodcode is prodcode)
		if (!(metadata$Source %in% SOURCEDICTS[dict == 'product', Source])){
			metadata$Source <- 'GPRDPROD'
		}
		if ('multilex' %in% colnames(x)) setnames(x, 'multilex', 'code')
		if ('prodname' %in% colnames(x)) setnames(x, 'prodname', 'term')
	} else if ('readcode' %in% names(x)){
		if (!(metadata$Source %in% SOURCEDICTS[dict == 'read', Source])){
			metadata$Source <- 'GPRD'
		}
		setnames(x, 'readcode', 'code')
		# Obtain medcodes if there are none
		if (!('medcode' %in% names(x))){
			x[, dict:='read']
			setkey(x, dict, code)
			setDictKey()
			x[, medcode:=CALIBER_DICT[x][, medcode]]
			x[, dict:=NULL]
		}
		if ('readterm' %in% names(x)){
			setnames(x, 'readterm', 'term')
		}
	} else if ('icd9_code' %in% names(x)){
		if (!(metadata$Source %in% SOURCEDICTS[dict == 'icd9', Source])){
			metadata$Source <- 'ONSICD9'
		}
		if ('icd9_term' %in% names(x)){
			setnames(x, 'icd9_term', 'term')
		}
		setnames(x, 'icd9_code', 'code')
	} else if ('icd_code' %in% names(x)){
		if (!(metadata$Source %in% SOURCEDICTS[dict == 'icd10', Source])){
			metadata$Source <- 'ONS'
		}
		if ('icd_term' %in% names(x)){
			setnames(x, 'icd_term', 'term')
		}
		setnames(x, 'icd_code', 'code')
	} else if ('opcs_code' %in% names(x)){
		if (!(metadata$Source %in% SOURCEDICTS[dict == 'opcs', Source])){
			metadata$Source <- 'OPCS'
		}
		if ('opcs_term' %in% names(x)){
			setnames(x, 'opcs_term', 'term')
		}
		setnames(x, 'opcs_code', 'code')
	} else if ('medcode' %in% names(x)){
		if (!(metadata$Source %in% SOURCEDICTS[dict == 'read', Source])){
			metadata$Source <- 'GPRD'
		}
		if ('readterm' %in% names(x)){
			setnames(x, 'readterm', 'term')
		}
	} 
	
	# metadata$Source cannot be NULL
	if (is.null(metadata$Source)){
		metadata$Source <- ''
	}
	
	if (metadata$Source==''){	
		if ('dict' %in% names(x)){
			# If there is a 'dict' column and all terms come from the same dict:
			getDict <- unique(sub('icdhead', 'icd10',
				sub('icd9head', 'icd9', unique(x$dict))))
			if (length(getDict)==1){
				# Default source
				metadata$Source <- switch(getDict,
					read='GPRD', opcs='OPCS', icd10='HES', icd9='ONSICD9')
			} else {
				stop('Terms come from more than one dictionary')
			}
		}	else if (length(getdictionary())==1){
			# If only one dictionary is in use:
			metadata$Source <- switch(getdictionary(),
				read='GPRD', opcs='OPCS', icd10='HES', icd9='ONSICD9')
		} else {
			stop('Unable to determine source dictionary')
		}
	}

	#### CHECK CODES AGAINST MASTER CALIBER_DICTONARY ####
	# (except for product codelists)
	if (metadata$Source %in%
		SOURCEDICTS[dict == 'read', Source]){
		TEMP <- CALIBER_DICT[dict=='read',
			list(medcode, .corrCode=code, .corrTerm=term)]
		setkey(TEMP, medcode)
		if (any(is.na(x$medcode))){
			x <- copy(x[!is.na(medcode)])
		}
		setkey(x, medcode)
		x[, .corrCode:=TEMP[x][, .corrCode]]
		# If x has Read codes. check for discrepancies
		if ('code' %in% names(x)){
			for (i in 1:nrow(x)){
				if (x[i, code] != x[i, .corrCode] & !is.na(x[i, code])){
					if (noisy){
						message('Read code for medcode ' %&%
							x[i, medcode] %&% ' corrected from ' %&%
							x[i, code] %&% ' to ' %&% x[i, .corrCode])
					}
					x[i, code:=.corrCode]
				}
			}
			x[, .corrCode:=NULL]
		} else {
			# No Read terms originally supplied, so use the new ones
			setnames(x, '.corrCode', 'code')
		}
		# Add Read terms if absent
		if (!('term' %in% names(x))){
			x[, term:=TEMP[x][, .corrTerm]]
		}
	} else if (metadata$Source %in%
		SOURCEDICTS[dict %in% c('icd9', 'icd10'), Source]){
		if (!(isExpandedCodelist(x))){
			setattr(x, 'Source', metadata$Source)
			class(x) <- c('codelist', 'data.table', 'data.frame')
			x <- copy(expandCodelist(x))
		}
	} else if (metadata$Source %in%
			SOURCEDICTS[dict == 'opcs', Source]){
		TEMP <- CALIBER_DICT[dict=='opcs', list(code,
			.corrCode=code, .corrTerm=term)]
		setkey(TEMP, code)
		setkey(x, code)
		x[, .corrCode:=TEMP[x][, .corrCode]]
		for (i in 1:nrow(x)){
			if (is.na(x[i, .corrCode])){
				if (noisy & !is.na(x[i, code])){
					if (x[i, code] != ''){
						message('OPCS code ' %&%
							x[i, code] %&%
							' is not in the master dictionary.')
					}
				}
			}
		}
		x[, .corrCode:=NULL]
		# Add OPCS terms if absent
		if (!('term' %in% names(x))){
			x[, term:=TEMP[x][, .corrTerm]]
		}
	}

	# Remove any missing codes, and check for duplicate categories
	if (metadata$Source %in%
		SOURCEDICTS[dict == 'read', Source]){
		x <- copy(x[!is.na(medcode) & 
			!duplicated(x[, list(medcode, category)])])
		# Check for any remaining duplicate codes (which must be in
		# more than one category)
		if (any(duplicated(x$medcode))){
			# Find one example
			exampleMedcode <- x[min(which(duplicated(x$medcode))), medcode]
			exampleCategories <- paste(x[medcode==exampleMedcode, category],
				 collapse=', ')
			stop('Medcode ' %&% exampleMedcode %&% ' is in categories ' %&%
				exampleCategories %&%
				'.\nIt is not permitted for a term to be in more than one category.\n' %&% 
				'Consider combining some categories and editing afterwards.')
		}	
	} else {
		x <- copy(x[!is.na(code) & !(code=='') & 
			!duplicated(x[, list(code, category)])])
		# Check for any remaining duplicate codes (which must be in
		# more than one category)
		if (any(duplicated(x$code))){
			# Find one example
			exampleCode <- x[min(which(duplicated(x$code))), code]
			exampleCategories <- paste(x[code==exampleCode, category], collapse=', ')
			stop('Code ' %&% exampleCode %&% ' is in categories ' %&%
				exampleCategories %&%
				'.\nIt is not permitted for a term to be in more than one category.\n' %&% 
				'Consider combining some categories and editing afterwards.')
		}
	}

	#### ADD ATTRIBUTES ####
	x <- addAttributesToCodelist(x, metadata)
	class(x) <- c('codelist', 'data.table', 'data.frame')

	# All exported codelists are sorted by category, then code
	setkey(x, category, code)
	# Reorder columns (except for therapy codelists)
	if (metadata$Source != 'GPRDPROD'){
		setcolorder(x, c(CODELIST_COLORDER,
			setdiff(names(x), CODELIST_COLORDER)))
	}
	return(x)
}

as.codelist <- function(x=NULL, ...){
	# Checks if an object is of class 'codelist'
	# Returns an object of class 'codelist' with metadata
	# Arguments: x - a codelist or anything which can be coerced to
	#                a codelist
	if (is.codelist(x)){
		return(x)
	} else {
		return(codelist(x, ...))
	}
}


addAttributesToCodelist <- function(x, metadata){
	# Returns the codelist with attributes added from a list
	# Arguments: x - codelist for which attributes are to be added
	#            metadata - list of metadata
	for (theAttribute in c('Name', 'Version', 'Source', 'Author',
		'Date', 'Timestamp', 'Categories', 'Expanded')){
		if (theAttribute %in% names(metadata)){
			setattr(x, theAttribute, metadata[[theAttribute]])
		} else {
			setattr(x, theAttribute, NULL)
		}
	}
	return(x)
}

print.codelist <- function(x, ...){
	printCodelist(x, ...)
}

printCodelist <- function(codelist, ...){
	# S3 method for printing codelist objects
	sourceDict <- getSourceDict(codelist)
	if (is.null(sourceDict)){
		stop('Source attribute is NULL')
	}
	cat('Codelist based on', sourceDict, 'dictionary with',
		tolower(textTotalTerms(nrow(codelist))) %&%	'.\n\n')
	cat(paste(encodeMetadata(codelist, includeDescription=TRUE),
		collapse='\n'))
	cat('\n\nTERMS (sorted by category and code):\n')
	printTerms(codelist[order(category, code)])
	invisible(codelist)
}

is.codelist <- function(x){
	# Checks the class attribute to decide whether x is a codelist
	if ('codelist' %in% class(x)){
		return(TRUE)
	} else{
		return(FALSE)
	}
}

truncateChar <- function(x, maxchar){
	# Truncates a character vector so that each element does not have more
	# than a specified number of characters, adding ... to the end of 
	# truncated terms
	# Arguments: x - character string to truncate
	#            maxchar - length to truncate to
	convert <- nchar(x) > maxchar
	x[convert] <- substr(x[convert], 1, maxchar-3) %&% '...'
	x
}

extractMetadataFromCodelist <- function(codelist){
	# Extract metadata from attributes of a codelist, returning it in a
	# list. The Expanded attribute is also retrieved even though it is
	# not printed or exported; this is for internal use and to ensure
	# that it is carried through.
	# Argument: codelist with attributes
	metadata <- list()
	for (theAttribute in c('Name', 'Version', 'Source', 'Author',
		'Date', 'Timestamp', 'Categories', 'Expanded')){
		metadata[[theAttribute]] <- attr(codelist, theAttribute)
	}
	return(metadata)
}


extractMetadataFromDatalabel <- function(datalabel, time.stamp){
	# Extracts metadata (except category table) from a Stata datalabel
	# Data elements are expected to be in the order:
	# Name, Version, Date, Author
	metadata <- list()
	splitLabel <- sub('\\|', '', strsplit(datalabel, '\\|')[[1]])
	# Trim spaces
	splitLabel <- sub('^[ ]*', '', splitLabel)
	splitLabel <- sub('[ ]*$', '', splitLabel)
	# Version number is appended to the end of the variable name
	metadata$Name <- splitLabel[1]
	metadata$Version <- splitLabel[2]
	metadata$Date <- splitLabel[3]
	metadata$Author <- splitLabel[4]
	# Source is the last part of the name (e.g. angina_gprd)
	tmp <- strsplit(metadata$Name, '_')[[1]]
	# use the last element of tmp (i.e. after the last _)
	metadata$Source <- toupper(tmp[length(tmp)])
	return(metadata)
}


extractMetadataFromColumn <- function(metadata){
	# Extracts metadata from a character vector (e.g. a metadata column
	# in a codelist CSV file), returning it in a list
	# Argument: character vector containing metadata
	metadata <- c(as.character(metadata), '')

	searchfor <- function(thing){
		use <- grep('^' %&% thing %&% ': ', metadata)
		if (length(use)>0){
			out <- sub('^' %&% thing %&% ': ', '',	
				metadata[use[1]])
			# Remove trailing spaces
			out <- sub('[ ]+$', '', out)
			return(out)
		} else {
			return('')
		}
	}

	out <- lapply(list('Name', 'Version', 'Source', 'Author',	
		'Date', 'Timestamp'), searchfor)
	names(out) <- c('Name', 'Version', 'Source', 'Author',
		'Date', 'Timestamp')

	startfrom <- grep('Categories:', metadata)[1] + 1
	if (length(startfrom) > 0){
		temp <- splitCategory(metadata[
			intersect(startfrom:length(metadata),
			which(gsub("[' ]", '', metadata) != ''))])
		# description is not provided in the 'column' metadata format
		# so set it to be the same as shortname
		temp$description <- temp$shortname
	}
	# Removing missing categories
	temp <- temp[!is.na(temp$category),]
	# Sort by category
	temp2 <- data.table(temp)
	setkey(temp2, category)
	out$Categories <- temp2
	return(out)
}

extractMetadataFromMETA <- function(dictName){
	# Returns a list of codelist metadata from the META data.table
	# Argument: dictionary name ('read', 'icd10' or 'opcs')
	out <- list()
	out$Name <- META[item == 'Name'][, value]
	out$Version <- META[item == 'Version'][, value]
	out$Source <- META[item == dictName][, value]
	# If no source specified, use default
	if (out$Source == 'TRUE'){
		out$Source <- SOURCEDICTS[dict == dictName, Source][1]
	}
	out$Author <- META[item == 'Author'][,value]
	out$Date <- META[item == 'Date'][,value]
	out$Timestamp <- format(Sys.time(), '%H.%m %d-%b-%y')
	out$Categories <- retrieveCategories()
	
	# Name must have the source at the end e.g. _gprd
	if (!grepl(tolower(out$Source) %&% '$', out$Name)){
		out$Name <- out$Name %&% '_' %&% tolower(out$Source)
	}
	return(out)
}

encodeMetadata <- function(x, includeDescription=FALSE){
	# Returns a character string with formatted metadata
	# Creates a timestamp if there is none, and updates the codelist
	# Argument: x - a codelist with attributes
	#           includeDescription - whether to include the long
	#                   form of the description (no for export,
	#                   yes when displaying a codelist on screen)
	if (is.null(attr(x, 'Categories'))){
		catTable <- NULL
	} else if (!is.data.frame(attr(x, 'Categories'))) {
		catTable <- NULL
	} else {
		categories <- attr(x, 'Categories')
		if ('category' %in% names(categories)){
			categories <- categories[!is.na(categories$category),]
			categories <- categories[categories$category>0,]
			usefulDescription <- ifelse((categories$description != '') &
				(categories$description != categories$shortname), 
				categories$description, '')
			if (includeDescription==FALSE){
				usefulDescription <- ''
			}
			catTable <- format(categories$category[
				categories$category>0]) %&% '. ' %&% 
				ifelse(is.na(categories$shortname), '', categories$shortname) %&%
				ifelse(usefulDescription != '',
					' (' %&% usefulDescription %&% ')', '')
		} else {
			catTable <- NULL
		}
	}

	# Set a timestamp if there is none
	if (is.null(attr(x, 'Timestamp'))){
		setattr(x, 'Timestamp', format(Sys.time(), '%H.%m %d-%b-%y'))
	} else if (attr(x, 'Timestamp')=='') {
		setattr(x, 'Timestamp', format(Sys.time(), '%H.%m %d-%b-%y'))
	}

	return(c('Name: ' %&% attr(x, 'Name'),
		'Version: ' %&% as.character(as.numeric(attr(x, 'Version'))),
		'Source: ' %&% attr(x, 'Source'),
		'Author: ' %&% attr(x, 'Author'),
		'Date: ' %&% sanitizeDate(attr(x, 'Date')),
		'Timestamp: ' %&% attr(x, 'Timestamp'),
		'Categories:', catTable))
}

splitCategory <- function(categorystrings){
	# Generates a data.frame of category number and shortname (brief
	# description) from a character vector of numbered categories
	# Argument: a character vector of strings in the format 1. XXX
	
	# Convert any non-ASCII characters when converting to 
	# character (use iconv, sub=byte converts classes to byte.)
	
	if (all(grepl('^[ ]*[[:digit:]]+\\.',
		iconv(as.character(categorystrings), sub='byte')))){
		# Use the numbers in the factor labels
		categorystrings <- iconv(as.character(categorystrings), sub='byte')
		# Match pattern:
		# Optional space, digit, ., optional space, non-space, optional space
		category <- as.integer(sub(
			'^([ [:digit:]]+)\\.([[:print:]]*)$', '\\1',
			categorystrings))
		shortname <- sub('[ ]*$', '', sub(
			'^([ [:digit:]]+)\\.[ ]*([^ ][[:print:]]*)$', '\\2',
			categorystrings))
		return(as.data.table(list(
			category=category,
			shortname=shortname,
			description=shortname)))
	} else {
		if (is.factor(categorystrings)){
			# Use the underlying numbers
			category <- unclass(categorystrings)
			return(as.data.table(list(category=category,
				shortname=iconv(as.character(categorystrings), sub='byte'),
				description=iconv(as.character(categorystrings), sub='byte'))))
		} else {
			# Make up category numbers if none available
			return(as.data.table(list(
				category=as.integer(as.factor(categorystrings)),
				shortname=categorystrings,
				description=categorystrings)))
		}
	}
}

subset.codelist <- function(x, subset, select, ...){
	# S3 method for subsetting a codelist
	# Arguments: x - codelist
	#            subset - a logical expression for rows to keep,
	#                   default is to keep all rows
	#            select - a character vector of columns to keep,
	#                   default is to keep all columns	
	
	metadata <- extractMetadataFromCodelist(x)
	
	# Ensure that as a minimum, code, medcode if GPRD,
	# term and category are kept. It is only valid as a codelist
	# if these columns are kept
	asCodelist <- FALSE
	
	if (missing(select)){
		# keep all columns
		select <- colnames(x)
		asCodelist <- TRUE
	} else {
		if (all(c('code', 'term', 'category') %in% select)){
			asCodelist <- TRUE
		}
		if (SOURCEDICTS$dict[metadata$Source ==
			SOURCEDICTS$Source] == 'read'){
			if (!('medcode' %in% select)){
				asCodelist <- FALSE
			}			
		} else if (SOURCEDICTS$dict[metadata$Source ==
			SOURCEDICTS$Source] == 'product'){
			if (!('prodcode' %in% select)){
				asCodelist <- FALSE
			}
		}
	}

	if (missing(subset)) {
		includeRow <- TRUE
	}	else {
		expr <- substitute(subset)
		includeRow <- eval(expr, x, parent.frame())
		if (!is.logical(includeRow)) 
			stop("'subset' must evaluate to a Boolean vector")
		includeRow[is.na(includeRow)] <- FALSE
	}
	
	out <- x[includeRow, select, with = FALSE]
	
	if (is.data.table(out)){
		out <- addAttributesToCodelist(out, metadata)
		if (asCodelist == TRUE){
			# Restore codelist class
			setattr(out, 'class', c('codelist', 'data.table',
				'data.frame'))
		}
	}
	return(copy(out))
}

getSourceDict <- function(codelist){
	# Returns the source dictionary of a codelist
	# Uses the SOURCEDICTS data.table
	SOURCEDICTS[Source == attr(codelist, 'Source'), dict]
}
