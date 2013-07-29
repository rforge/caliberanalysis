medcodeis <- function(medcodes = integer(0)){
	# Returns a selection object with the given medcodes selected.
	# Arguments: vector of medcodes

	# Ensure that dictionaries are loaded
	loadDICT()
	
	medcodes <- as.integer(medcodes)
	as.selection(CALIBER_DICT$medcode %in% medcodes)
}

