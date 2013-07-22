dictis <- function(dictName1, dictName2=NULL, dictName3=NULL){
	# Returns a selection object stating whether a term is in a
  # specified dictionary. This is intended for use in the
  # code selection process together with termhas, codematch etc.
	# Arguments: dictionary names, either as a vector or as separate
	#            arguments for convenience.

	# Ensure that dictionaries are loaded
	loadDICT()
	
	dicts <- c(dictName1, dictName2, dictName3)
	if (!(all(dicts %in% ALLDICTS))){
		stop(dicts[!(dicts %in% ALLDICTS)][1] %&% 
		' is not a valid dictionary.\nIt should be in ' %&%
		paste(ALLDICTS, collapse=', '))
	} else {
		as.selection(CALIBER_DICT$dict %in% META[dicts][value=='TRUE'][, item])
	}
}


