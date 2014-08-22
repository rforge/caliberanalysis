setdictionary <- function(dictName1 = NULL, dictName2 = NULL,
	dictName3 = NULL, dictName4 = NULL, reset = TRUE){
	# Clear the categories in CALIBER_DICT and specify the subset of
	# dictionaries to use for code selection. If the character vectors
	# VERSION_READ, VERSION_ICD10, VERSION_ICD9, VERSION_OPCS are
	# available, they are used to produce the text on screen. The
	# default is that all dictionaries are selected.
	# Invisibly returns a character vector of the dictionaries selected.
	# Arguments: dictionary names, either as a vector or as separate
	#            arguments for convenience.
	#            alternatively, dictName1, dictName2 ... can be codelists
	#            (as long as they are from different source dictionaries)
	#            in which case the master dictionary is updated to equal
	#            the codelist. The categories table and metadata are updated.
	#            Only blank entries in metadata are updated.
	
	loadDICT()
	
	if (reset == TRUE){
		META[item == 'category', value := '']
		META[item == 'shortname', value := '']
		META[item == 'description', value := '']
		cat('\nCALIBERcodelists package, version ' %&%
			packageVersion('CALIBERcodelists'))
		cat('\nClearing categories in master dictionary.')
		CALIBER_DICT[, category:=NA_integer_]
	}
	# Reset the key if necessary
	setDictKey()

	codelist1 <- NULL
	codelist2 <- NULL
	codelist3 <- NULL
	codelist4 <- NULL
	getsource <- function(mycodelist){
		getSourceDict(mycodelist)
	}
	
	if (is.codelist(dictName1)) {
		codelist1 <- copy(dictName1)
		tmp <- getsource(dictName1)
		rm(dictName1)
		dictName1 <- tmp
	}
	if (is.codelist(dictName2)) {
		codelist2 <- copy(dictName2)
		tmp <- getsource(dictName2)
		rm(dictName2)
		dictName2 <- tmp	
	}
	if (is.codelist(dictName3)) {
		codelist3 <- copy(dictName3)
		tmp <- getsource(dictName3)
		rm(dictName3)
		dictName3 <- tmp
	}
	if (is.codelist(dictName4)) {
		codelist4 <- copy(dictName4)
		tmp <- getsource(dictName4)
		rm(dictName4)
		dictName4 <- tmp
	}
			
	if (!is.null(dictName1)){
		# Specify which dictionary to use for code selection
		whichdictionary <- unique(c(dictName1, dictName2, dictName3,
			dictName4))
		# Record which dictionaries are selected in META
		# Dictionaries not currently required switched to FALSE.
		META[item %in% ALLDICTS & !(item %in% whichdictionary),
			value := 'FALSE']
		# Change value to TRUE if it is FALSE or blank
		# but leave it unchanged if it specifies a data source
		META[item %in% whichdictionary & value == 'FALSE', value := 'TRUE']
		META[item %in% whichdictionary & value == '', value := 'TRUE']
		if (!all(whichdictionary %in% ALLDICTS)){
			stop('Dictionary name(s) not valid; they should be in (' %&%
				paste(ALLDICTS, collapse=', ') %&% ')')
		}
		if (length(whichdictionary)==1){
			cat('\nUsing ' %&% whichdictionary %&% ' dictionary.\n')	
		} else {
			cat('\nUsing ' %&%
				paste(whichdictionary, collapse=', ') %&%
				' dictionaries.\n')
		}
		
		if ('read' %in% whichdictionary){
			cat(attr(CALIBER_DICT, 'VERSION_READ'), '\n')
		}
		if ('opcs' %in% whichdictionary){
			cat(attr(CALIBER_DICT, 'VERSION_OPCS'), '\n')
		}
		if ('icd10' %in% whichdictionary){
			cat(attr(CALIBER_DICT, 'VERSION_ICD10'), '\n')
		}
		if ('icd9' %in% whichdictionary){
			cat(attr(CALIBER_DICT, 'VERSION_ICD9'), '\n')
		}
	}
	
	# Now add codelists (if any)
	if (!is.null(codelist1)){
		codelistToDict(codelist1)
	}
	if (!is.null(codelist2)){
		codelistToDict(codelist2)
	}
	if (!is.null(codelist3)){
		codelistToDict(codelist3)
	}
	if (!is.null(codelist4)){
		codelistToDict(codelist4)
	}

	invisible(getdictionary())
}

getdictionary <- function(){
	# Returns a character vector of the dictionaries in use
	tmp <- META[item %in% ALLDICTS][value != 'FALSE', item]
	tmp
}

codelistToDict <- function(codelist){
	# Updates CALIBER_DICT with the terms in a codelist
	# Also updates metadata if it is blank (but does not over-write)
	# Not an exported function; called by setdictionary()
	# Argument: codelist
	cat('\nAdding terms from codelist with', nrow(codelist), 'terms.\n')
	
	sourceDict <- getSourceDict(codelist)
	if (sourceDict=='product'){
		cat('\nThere is currently no codelist generation process for products.\n')
	} else {
		mylist <- copy(codelist)
		mylist[, dict:=sourceDict]
		mylist[, category:=as.integer(category)]
		setDictKey()
		setkey(mylist, dict, code)
		# Use a temporary variable to allow terms to be added from multiple codelists
		CALIBER_DICT[, .temp := mylist[CALIBER_DICT]$category]
		CALIBER_DICT[is.na(category), category := .temp]
		CALIBER_DICT[, .temp := NULL]

		# Now update category definitions
		newCategories <- attr(codelist, 'Categories')
		oldCategories <- retrieveCategoriesFromMETA()
		if (is.null(newCategories)){
			# don't bother
		} else if (nrow(newCategories) == 0){
			# don't bother
		} else {
			for (i in 1:nrow(newCategories)){
				if (!(newCategories[i, category] %in% oldCategories$category)){
					needToAdd <- TRUE
				} else if (is.na(oldCategories$shortname[
					oldCategories$category == newCategories[i, category]])){
					# if it has no shortname
					needToAdd <- TRUE
				} else if (oldCategories$shortname[
					oldCategories$category == newCategories[i, category]] == ''){
					# if the shortname is blank
					needToAdd <- TRUE
				} else {
					needToAdd <- FALSE
				}
				if (needToAdd){
					addCategory(number=newCategories[i, category],
						shortname=newCategories[i, shortname],
						description=newCategories[i, description])
				}
			}
		}
		
		# Update other metadata
		for (field in c('Name', 'Author', 'Date', 'Version')){
			temp <- as.character(attr(codelist, field))
			if (length(temp) == 0){
				temp <- ''
			}
			if (is.na(META[item==field, value])){
				META[item==field, value:=temp]
			} else if (META[item==field, value=='']){
				META[item==field, value:=temp]
			}
		}
	}
}

