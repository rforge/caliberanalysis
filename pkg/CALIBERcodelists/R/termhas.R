termhas <- function(regexpr, exact=FALSE, ignorecase=TRUE){
	# Returns (invisibly) a selection object with as many elements as 
	# rows of CALIBER_DICT, according to whether the term contains regexpr
	# and is in one of the selected dictionaries. For ICD10, only
	# valid ICD10 4 character entries are searched, not the headers.
	# Arguments: regexpr - search term or POSIX regular expression.
	#                  If a vector, the results are combined using OR.
	#                  Note that this is a change from the previous
	#                  behaviour (CALIBERcodelists v0.2-1), in which
	#                  they were combined using AND. This has been changed
	#                  to make it consistent with the codematch, 
	#                  medcodeis and dictis functions.
	#            exact - whether to use an exact match instead of regex
	#            ignorecase - whether the function is case-sensitive
	#                  (note: ignorecase will also ignore [X] etc. at the
	#                  beginning of terms)
	
	# Ensure that dictionaries are loaded  
	loadDICT()
	if (is.selection(regexpr)){
		# pass through, return the original selection
		# e.g. termhas('angina' %AND% termhas('mi'))
		return(regexpr)
	} else if (is.character(regexpr)){
		
		if (length(regexpr) > 1){
			# concatenate regular expressions
			regexpr <- paste('(' %&% regexpr %&% ')', collapse = '|')
		}

		if (exact){
			if (ignorecase==TRUE){
				out <- (CALIBER_DICT$termlc %in% tolower(regexpr))
			} else {
				out <- (CALIBER_DICT$term %in% regexpr)
			}
		} else {
			if (ignorecase==FALSE){
				out <- grepl(regexpr, CALIBER_DICT$term)
			} else {
				out <- grepl(tolower(regexpr), CALIBER_DICT$termlc)
			}
		}
		class(out) <- 'selection'
		return(out)
	}
}

