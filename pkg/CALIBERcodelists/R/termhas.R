termhas <- function(regexpr, exact=FALSE, ignorecase=TRUE){
	# Returns (invisibly) a selection object with as many elements as 
	# rows of CALIBER_DICT, according to whether the term contains regexpr
	# and is in one of the selected dictionaries. For ICD10, only
	# valid ICD10 4 character entries are searched, not the headers.
	# Arguments: regexpr - search term or POSIX regular expression
	#            exact - whether to use an exact match instead of regex
	#            ignorecase - whether the function is case-sensitive
	#                  (note: ignorecase will also ignore [X] etc. at the
	#                  beginning of terms)
	
	# Ensure that dictionaries are loaded  
	loadDICT()
	if (is.selection(regexpr)){
		# may be a mistake, leave it as it is
		# e.g. termhas('angina' %AND% termhas('mi'))
		return(regexpr)
	} else if (is.character(regexpr)){
		if (exact){
			if (ignorecase==TRUE){
				out <- (CALIBER_DICT$termlc %in% tolower(regexpr))
			} else {
				out <- (CALIBER_DICT$term %in% regexpr)
			}
		} else {
			if (ignorecase==FALSE){
				out <- multigrep(regexpr, CALIBER_DICT$term)
			} else {
				out <- multigrep(tolower(regexpr), CALIBER_DICT$termlc)
			}
		}
		class(out) <- 'selection'
		return(out)
	}
}

multigrep <- function(regexpr, searchterms){
	if (length(regexpr)==1){
		return(grepl(regexpr, searchterms))
	} else {
		temp <- sapply(regexpr, function(x){
			grepl(x, searchterms)
		})
		return(apply(temp, 1, all))
	}
}
