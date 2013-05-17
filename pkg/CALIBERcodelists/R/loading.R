loadDICT <- function(){
	# Loads the master dictionary into the Global Environment
	# Does not re-load if already exists
	# Raises an error if an object exists which is not the dictionary
	# If no dictionary available in data, it uses the 
	# sample version in the namespace
	
	if (!exists('CALIBER_DICT')){
		# Try to load CALIBERlookups package if installed
		if (identical(find.package('CALIBERlookups', quiet=TRUE),
			character(0))){
			message('Searching for dictionaries in ' %&% getwd() %&% '/data')
		} else {
			# Load the CALIBERlookups package, but do not 'require' it
			# explicity so that R CMD check does not report an error -
			# the CALIBERlookups package will not be available
			# to external users.
			eval(parse(text="require('CALIBERlookups', quietly=TRUE)"))
		}
		data('CALIBER_DICT', envir = .GlobalEnv)
		if (!exists('CALIBER_DICT')){
			warning('No CALIBER_DICT available. Using sample dictionary.')
			assign('CALIBER_DICT', SAMPLE_DICT, envir=.GlobalEnv)
		}
		# Set the dictionary key
		setkey(CALIBER_DICT, dict, code)
	} else {
		# Check is a data.table, and check column names
		if (!identical(sort(names(CALIBER_DICT)),
			c("category", "code", "dict", "events",
			"medcode", "term", "termlc"))){
			stop('There is a CALIBER_DICT object in the global environment
but it does not seem to be the master dictionary. Please remove
this object and try again.')
		}
	}
	
	# Check that the dictionary key is correct
	setDictKey()
}


setDictKey <- function(){
	# Sets the default key for CALIBER_DICT: dict, code
	# Returns the current key (in case it is required to reset the key)
	if (identical(key(CALIBER_DICT), c('dict', 'code'))){
		return(c('dict', 'code'))
	} else {
		temp <- key(CALIBER_DICT)
		setkey(CALIBER_DICT, dict, code)
		return(temp)
	}
}


loadDICTMAPS <- function(){
	# Load the dictionary mappings
	if (!exists('CALIBER_DICTMAPS')){
		# Try to load the CALIBERlookups package if installed
		if (identical(find.package('CALIBERlookups', quiet=TRUE),
			character(0))){
			message('Searching for mappings in ' %&% getwd() %&% '/data')
		} else {
			# Load the CALIBERlookups package, but do not 'require' it
			# explicity so that R CMD check does not report an error -
			# the CALIBERlookups package will not be available
			# to external users.
			eval(parse(text="require('CALIBERlookups', quietly=TRUE)"))
		}
		data('CALIBER_DICTMAPS', envir = .GlobalEnv)
		if (!exists('CALIBER_DICTMAPS')){
			warning('No CALIBER_DICTMAPS available. Using sample mappings.')
			assign('CALIBER_DICTMAPS', SAMPLE_DICTMAPS, envir=.GlobalEnv)
		}
		data('CALIBER_DICTMAPS', envir = .GlobalEnv)
		if (!exists('CALIBER_DICTMAPS')){
			warning('No CALIBER_DICTMAPS available. Using sample mapping.')
			assign('CALIBER_DICTMAPS', SAMPLE_DICTMAPS, envir=.GlobalEnv)
		}	
	} else {
		# Check is a data.table, and check column names
		if (!identical(sort(names(CALIBER_DICTMAPS)),
			c("add_flag", "block_num", "code", "dict", "elem_num",
			"map_stat", "medcode", "ref_flag"))){
			stop('There is a CALIBER_DICTMAPS object in the global environment
but it does not seem to be the ICD-10/Read/OPCS mapping. Please remove
this object and try again.')
		}		
	}
}
