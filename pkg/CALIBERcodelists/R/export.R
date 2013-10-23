exportall <- function(directory = getwd(), varname = NULL,
	saveMessage = TRUE, icd10_source = 'hes'){
	# Exports all codelists for dictionaries in use, and checks them
	# against existing codelists. Saves any messages and the result of
	# checking in META['message']. Exports to filenames name_gprd.csv,
	# name_hes.csv etc.
	# Does not export therapy codelists.
	# Arguments: directory - directory to export to, with or without
	#                    final /, or the current working directory if not
	#                    supplied.
	#            varname - variable name (string), obtained from META['Name']
	#                    if not supplied
	#            saveMessage - whether to save the comparison of current versus
	#                    previous version of codelist in META. The default is
	#                    TRUE so that when called by process() on a Rmd file
	#                    the message can be printed on screen but not
	#                    in the definitive HTML document.
	#            icd10_source - source name for HES codelist
	#                    (options are hes or ons). 
	
	sourcenames <- setdiff(META[item %in% ALLDICTS, value], 'FALSE')
	
	if ('icd10' %in% getdictionary()){
		sourcenames <- unique(c(sourcenames, icd10_source))
		if (!all(icd10_source %in% SOURCEDICTS[dict == 'icd10', Source])){
			stop('ICD-10 source name not valid')
		}
	}
	
	for (i in 1:length(sourcenames)){
		# Generate a codelist -- but only if any terms selected
		sourcename <- sourcenames[i]
		# Select the dictionary
		thisdict <- SOURCEDICTS[Source == sourcenames[i], dict]
		# Use standard naming convention: 
		# Name (from META) _gprd, _hes, _opcs
		my <- as.codelist(thisdict)
		
		if (nrow(my) > 0){
			if (is.null(varname)){
				# Use varname from META
				varname <- META[item=='Name'][, value]
			}
			
			# varname should overwrite default name from META
			if (grepl(sourcename %&% '$', varname)){
				# varname already has a suffix e.g. _gprd
				setMetadata(my, Name=varname)
			} else {
				# add a _gprd, _hes or _opcs suffix
				setMetadata(my, Name=varname %&% '_' %&% sourcename)
			}
			
			# Add an extra '/' or '\\' if needed at the end of directory
			# Add the codelist version (integer part only)
			filepath <- sub('[\\\\/]$', '', directory) %&%
				'/' %&% makeCodelistFilename(my)
			# Compare with existing codelist on this filepath
			old <- NULL
			
			# Look for existing version of codelist
			if (file.exists(filepath)){
				try(old <- as.codelist(filepath))
			}
			
			if (!is.null(old)){
				# Compare codelists
				message <- compare(old, my)$message
				oldmessage <- META[item=='message'][, value]
				if (saveMessage){
					META['message', value:=paste(c(oldmessage, message),
						collapse='\n')]
				}
			}
			export(my, filepath)
		}
	}
}

makeCodelistFilename <- function(codelist){
	if (nameHasSource(codelist)){
		theName <- attr(codelist, 'Name')
	} else {
		theName <- attr(codelist, 'Name') %&% '_' %&%
			tolower(attr(codelist, 'Source'))
	}
	# Use integer part of version number
	theName %&% '.codelist.' %&%
		as.character(floor(as.numeric(attr(codelist, 'Version')))) %&%
		'.csv'
}

filenameAccordingToProtocol <- function(filename, codelist){
	# Checking that the last part of the filename is the idealName
	# (ignore the extension)
	idealName <- makeCodelistFilename(codelist)
	substr(filename, nchar(filename) - nchar(idealName) + 1,
		nchar(filename) - 3) == 
	substr(idealName, 1, nchar(idealName) - 3)
}

nameHasSource <- function(codelist){
	# Whether the codelist name has the appropriate suffix 
	# e.g. angina_gprd
	grepl(tolower(attr(codelist, 'Source')) %&% '$',
		ifelse(is.null(attr(codelist, 'Name')), '', attr(codelist, 'Name')))
}

export <- function(codelist, filename = NULL,
	categories = 'all', contractIfICD10 = TRUE, contractIfICD9 = TRUE){
	# Exports a codelist to file. All metadata must be stored in the codelist.
	# Arguments: codelist - coerced to codelist if is.codelist(codelist) is FALSE
	#            filename - exact filename of file to export to
	#            categories - which categories to export; default=all, otherwise
	#                    supply a numeric vector of categories (note that this
	#                    does not affect the category table)
	#            contractIfICD10 - whether to contract ICD10 codelist

	# If filename is null, create a filename from codelist
	if (is.null(filename)){
		filename <- makeCodelistFilename(codelist)
	} else if (grepl('[\\\\/]$', filename)){
		# filename is a directory; so create a filename
		filename <- filename %&% makeCodelistFilename(codelist)
	} else {
		# Check that it conforms to policy
		if (!filenameAccordingToProtocol(filename, codelist)){
			cat('\nNote: CALIBER codelist naming policy would recommend\n' %&%
				makeCodelistFilename(codelist))
		}
	}
	
	cat('\nExporting ' %&% attr(codelist, 'Name') %&%
		' codelist to ' %&% filename %&% '\n')
	what <- subset(codelist, category > 0)

	# Check that the codelist name contains the source
	if (!nameHasSource(what)){
		setattr(what, 'Name', attr(what, 'Name') %&% '_' %&%
			tolower(attr(what, 'Source')))
		cat('\nChanging name to ' %&% attr(what, 'Name') %&%
			' to comply with naming policy.\n')
	}

	# Create the metadata character vector
	metadata <- encodeMetadata(what)
	metadataWidth <- max(nchar(metadata)) + 1
	metadata <- padTo(metadata, metadataWidth)

	# Which categories to include in the exported codelist
	if (is.numeric(categories)){
		what <- copy(what[category %in% categories])
	} else if (all(is.na(categories))) {
		what <- copy(what[is.na(category)])
	} else if (identical(categories, 'all')){
		# include everything
		what <- copy(what)
	} else {
		stop('categories argument is invalid;
			it should be a numeric vector or "all"')
	}

	# If it is an ICD10 or ICD9 codelist, it should be compressed
	if (attr(what, 'Source') %in% SOURCEDICTS[dict == 'icd10', Source] &
		contractIfICD10 == TRUE){
		if (!(isContractedCodelist(what))){
			what <- copy(contractCodelist(what))
		}
		setnames(what, 'code', 'icd_code')
		setnames(what, 'term', 'icd_term')
		setkey(what, category, icd_code)
	} else if (attr(what, 'Source') %in%
		SOURCEDICTS[dict == 'icd9', Source] &
		contractIfICD9 == TRUE){
		if (!(isContractedCodelist(what))){
			what <- copy(contractCodelist(what))
		}
		setnames(what, 'code', 'icd9_code')
		setnames(what, 'term', 'icd9_term')
		setkey(what, category, icd9_code)
	} else if (attr(what, 'Source') %in%
		SOURCEDICTS[dict == 'read', Source]){
		setnames(what, 'code', 'readcode')
		setnames(what, 'term', 'readterm')
		setkey(what, category, readcode)
	} else if (attr(what, 'Source') %in%
		SOURCEDICTS[dict == 'opcs', Source]){
		setnames(what, 'code', 'opcs_code')
		setnames(what, 'term', 'opcs_term')
		setkey(what, category, opcs_code)
	} else if (attr(what, 'Source') %in%
		SOURCEDICTS[dict == 'product', Source]){
		setnames(what, 'code', 'multilex')
		setnames(what, 'term', 'prodname')
		setkey(what, category, prodcode)
	}
	
	# Output to file
	if (grepl('.dta$', tolower(filename))){
		# export to Stata file
		# Need to get category labels
		catlabels <- getCatLabels(what)
		what$category <- factor(what$category,
			levels=catlabels$category,
			labels=catlabels$shortname)
		# Encode metadata into header row
		myheader <- attr(what, 'Name') %&% ' | ' %&%
			attr(what, 'Version') %&% ' | ' %&%
			attr(what, 'Date') %&% ' | ' %&%
			attr(what, 'Author')
		# maximum header length is 80 characters
		writeToStata(what, filename, datalabel=myheader)
		return(invisible(what))		
	} else {
		# Export to text file (CSV or tab separated)
		# Bind metadata column onto codelist
		if (length(metadata) < nrow(what)){
			metadata <- c(metadata, rep(padTo('', metadataWidth),
				nrow(what) - length(metadata)))
		} else if (length(metadata) > nrow(what)) {
			what <- copy(what[1:length(metadata)])
		}
	
		# Bind them all together in a custom CSV
		what[, metadata:=metadata]
	
		# For 2-digit categories, remove the extra space in the metadata column
		# so they line up in the CSV file
		what[category>9, metadata:=substr(metadata, 1, metadataWidth-1)]
	
		# Put the metadata column first
		setcolorder(what, c('metadata', setdiff(colnames(what), c('metadata'))))

	 if (grepl('.csv$', tolower(filename))){
			write.csv(what, file=filename, row.names=FALSE, na='')
		} else {
			# tab delimited 
			write.table(what, file=filename, row.names=FALSE, sep='\t', na='')
		}
		return(invisible(what))
	}
}

padTo <- function(string, length){
	# Returns a character vector with strings padded to a particular length
	# Arguments: string - string to pad out with additional spaces
	#            length - final length of string
	spaces <- paste(rep(' ', length), collapse='')
	substr(string %&% spaces, 1, length)
}


getCatLabels <- function(codelist){
	# Retrieve category table
  cattable <- data.table(attr(codelist, 'Categories'), key='category')
	# Use all positive categories
	allcatnum <- unique(union(codelist$category, cattable$category))
	allcatnum <- allcatnum[allcatnum > 0] 
	# Complete category table containing all labelled categories and all
	# categories up to the maximum.
	allcats <- cattable[J(1:max(allcatnum, na.rm=TRUE))]
	# Use the category number if there is no shortname
	allcats[is.na(shortname), shortname:=as.character(category)]
  allcats[shortname=='', shortname:=as.character(category)]
	allcats
}


writeDatalabelToStataFile <- function(datalabel,
	stata_filename, add_timestamp = TRUE){
	# Find out file size, and required size of RAW vector
	fileSize <- file.info(stata_filename)$size
	toread = file(stata_filename, "rb")
	# Read Stata file into RAW vector
	myraw <- readBin(toread, 'raw', n=fileSize)
	close(toread)
	
	# Metadata (use 79 characters to over-write
	# previous datalabel if present)
	myraw[11:89] <- charToRaw(substr(
		paste(c(datalabel, rep(' ', 79)), collapse=''),
		1, 79))
	
	# Then add a timestamp
	if (add_timestamp){
		myraw[92:108] <- charToRaw(substr(
			format(Sys.time(), '%d %b %Y %H:%M'), 1, 17))
	}
	
	towrite = file(stata_filename, "wb")
	writeBin(myraw, towrite)
	close(towrite)
}

writeToStata <- function(data, filename, datalabel=NULL, ...){
	# Write to stata file, including timestamp and datalabel
	if (is.null(datalabel)){
		datalabel <- attr(data, 'datalabel')
	}
	# Write to Stata file
	write.dta(data, file=filename)
	# Add datalabel
	if (!is.null(datalabel)){
		writeDatalabelToStataFile(datalabel,
			stata_filename=filename, ...)
	}
}

