decode <- function(codes, dictionary){
	# returns the term matching a combination of codes and dictionary
	# (dictionary can be a vector)

	if (dictionary[1] == 'product'){
		stop('This function cannot currently decode products')
	}

	# Ensure that dictionary is loaded
	loadDICT()
	if (is.numeric(codes) & all(dictionary == 'read')){
		# Codes must be medcodes
		temp <- data.table(dict = 'read', medcode = codes)
		setkey(temp, dict, medcode)
		setkey(CALIBER_DICT, dict, medcode)
		out <- CALIBER_DICT[temp][, term]
		# Reset dictionary key
		setDictKey()
	} else {
		temp <- data.table(dict = dictionary, code = codes)
		setkey(temp, dict, code)
		# Ensure that dictionary key is dict, code
		setDictKey()
		out <- CALIBER_DICT[temp][, term]		
	}
	out
}

