assigncat <- function(number, definition=NULL, logic=NULL,
	cats_to_convert=NULL, dictionary=NULL, codes=NULL){
	# cats_to_convert can limit the categories which are
	# allowed to be converted. If null, anything can be converted.
	# cats_to_convert should be a vector of eligible categories,
	# e.g. NA if only missing categories can be converted	
	# if logic is null, just add / modify the category table
	# Arguments: number - category number
	#            definition - new shortname and description, separated by |
	#            logic - Boolean vector for codes which are selected
	#            cats_to_convert - a character vector. Default is to
	#                       convert anything for codes being included
	#                       but only convert codes already selected (category > 0)
	#                       if x is 0 (for excluded terms)
	#            dictionary - to be used with codes, for assigning codes in
	#                       a specified dictionary
	#            codes - to be used with dictionary, for assigning codes in
	#                       a specified dictionary

	# Ensure that dictionaries are loaded
	loadDICT()

	if (!is.numeric(number) | length(number)!=1){
		stop('The first argument must be a single integer')
	}

	if (is.null(cats_to_convert)){
		if (number==0){
			# Only exclude terms already included
			# include 1 to ensure that the result
			# is a vector of length at least 1
			condition <- CALIBER_DICT[, !is.na(category) & dict %in% getdictionary()]
		} else {
			condition <- CALIBER_DICT[, dict %in% getdictionary()]
		}
	} else {
		condition <- CALIBER_DICT[, dict %in% getdictionary() &
                        category %in% cats_to_convert]
	}

	# Extract definition
	if (is.null(definition)){
		definition <- ''
	}
	if (grepl('\\|', definition)){
		shortnames <- sub('^([[:print:]]*)\\|([[:print:]]*)$',
			'\\1', definition)
		descriptions  <- sub('^([[:print:]]*)\\|([[:print:]]*)$',
			'\\2', definition)
	} else {
		shortnames <- definition
		descriptions  <- definition
	}

	addCategory(number, shortnames, descriptions)

	if (!is.null(logic)){
		# ensure that logic is a selection, coerce if necessary
		logic <- as.selection(logic)
	}

	if (!is.null(dictionary) & !is.null(codes)){
		if (!all(dictionary %in% ALLDICTS)){
			stop('Dictionary must be in ' %&% 
				paste(ALLDICTS, collapse=', '))
		}
		# Select by Read, OPCS or ICD-10 codes
		if (dictionary=='icd10'){
			# Need to do term matching because of headers etc.
			allcodes <- paste('^' %&% codes, collapse='|')
			codeselect <- CALIBER_DICT[, dict=='icd10' &
				grepl(allcodes, code)]
		} else {
			setDictKey()
			temp <- data.table(dict=dictionary, code=codes, change=TRUE)
			setkey(temp, dict, code)
			codeselect <- temp[CALIBER_DICT][, change]
			codeselect[is.na(codeselect)] <- FALSE
		}
		if (is.null(logic)){
			logic <- codeselect
		} else {
			logic <- logic | codeselect
		}
	}
	if (!is.null(logic)){
		oldnum <- sum(CALIBER_DICT$category==number, na.rm=TRUE)
		CALIBER_DICT[as.logical(condition & logic),
			category := as.integer(number)]
		newnum <- sum(CALIBER_DICT$category==number, na.rm=TRUE)
		if (oldnum==0){
			cat('\n' %&% newnum %&% ' terms assigned to category ' %&%
				number %&% ' (' %&% shortnames %&% ')\n')
		} else {
			cat('\n' %&% format(newnum - oldnum) %&%
				' terms added to category ' %&% number %&% 
				'(' %&% shortnames %&%
				'), bringing the total to ' %&% newnum %&% '\n')
		}		
	}
}

showAssigncat <- function(category, dictionary, codes, showRprompt=FALSE){
	# Display a set of codes for assigning a category
	# Show them in groups of 5 for clarity
	# showRprompt is used in interactive mode
	
	combinecodes <- function(x){
		# Convert codes into a vector list of up to 5
		if (length(x)==1){
			return('"' %&% x %&% '"')
		} else {
			return('c(' %&% paste('"' %&% x %&% '"',
				collapse=', ') %&% ')')
		}
	}
	
	listofcodes <- list()

	for (i in 1:length(codes)){
		if (i %% 5 == 1){
			listofcodes <- c(listofcodes, 
				combinecodes(codes[i:(min(length(codes), i+4))]))
		}
	}

	if (showRprompt){
		prompt <- '> '
	} else {
		prompt <- ''
	}
	
	# Use icd10 for ICD10 and ICD headers; use icd9 for ICD9
	paste(sapply(listofcodes, function(z){
		prompt %&% 'assigncat(' %&% category %&% ', dictionary="' %&%
		ifelse(dictionary == 'icdhead', 'icd10',
		ifelse(dictionary == 'icd9head', 'icd9', dictionary)) %&%
		'", codes=' %&% z %&% ')'
	}), collapse = '\n')
}

