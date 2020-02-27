codematch <- function(regexpr,
	dictionary=ifelse(length(getdictionary())==1, getdictionary(), ''), 
	mapStatus=NULL, exact=FALSE){
	# Identifies terms by their ICD-10, ICD-9, OPCS or Read code, and also
	# selects mapped terms (there are no ICD-9 terms so these are only
	# used for mapping to Read)
	# Arguments: regexpr - pattern to match ICD-10 codes of interest
	#                    or a vector of patterns; results are combined using OR 
	#            dictionary - a single dictionary in which to look for codes
	#            mapStatus - D=default, A=alternative, G=ICD-10 or OPCS
	#                    term is more general than Read, E=exact,
	#                    R=terms to exclude in order to code to this term
	#            exact - whether regexpr is to be interpreted as exact
	#                    instead of a regular expresion

	# Ensure that dictionary and mappings are available
	loadDICT()
	loadDICTMAPS()

	if (exact == FALSE & length(regexpr) > 1){
		# concatenate regular expressions
		regexpr <- paste('(' %&% regexpr %&% ')', collapse = '|')
	}

	if (identical(dictionary, '')){
		dictionary <- select.list(ALLDICTS,
			title='Which type of code to search for ' %&% regexpr %&% '?',
			multiple=FALSE, graphics=FALSE)
	}
	
	if (length(dictionary) != 1){
		stop('The dictionary argument should contain exactly one dictionary, not ' %&%
			length(dictionary))
	}
	
	if (!(dictionary %in% ALLDICTS)){
		stop('Dictionary is ' %&% dictionary %&% ' but should be one of ' %&%
			paste(ALLDICTS, collapse=', '))
	}
	
	if (is.null(mapStatus)){
		mapStatus <- switch(dictionary, read=c('A', 'D', 'E', 'T'), 
			icd10=c('A', 'D', 'G', 'E', 'T'), icd9=c('A', 'D', 'G', 'E', 'T'), 
			opcs=c('A', 'D', 'G', 'E', 'T'))
	}
	
	if (dictionary %in% c('icd9', 'icd10')){
		dicthead <- ifelse(dictionary == 'icd10', 'icdhead', 'icd9head')
		if (exact){
			out <- CALIBER_DICT[, dict %in% c(dictionary, dicthead) &
				code %in% regexpr]
		} else {
			out <- CALIBER_DICT[, dict %in% c(dictionary, dicthead) &
				grepl(regexpr, code)]
		}
		icdcodes <- unique(CALIBER_DICT[out, code])
		# Link to Read terms
		if (META['read'][, value] != 'FALSE'){
			tempMedcodes <- unique(
				CALIBER_DICTMAPS[(dict == dictionary) &
					(code %in% icdcodes) &
					(map_stat %in% mapStatus), medcode])
			out[CALIBER_DICT[, dict == 'read' &
				medcode %in% tempMedcodes]] <- TRUE
		}
		# Link to other ICD dictionary
		if (dictionary == 'icd9' & META['icd10'][, value] != 'FALSE'){
			loadICDMAPS()
			# Include ICD-10 terms linked to the current ICD-9 term
			setkey(CALIBER_GEM, icd9)
			icd10codes <- CALIBER_GEM[J(icdcodes), allow.cartesian = TRUE][
				use == TRUE & from9to10 == FALSE, icd10]
			out[CALIBER_DICT[, dict %in% c('icd10', 'icdhead') & code %in%
				icd10codes]] <- TRUE
		}
		if (dictionary == 'icd10' & META['icd9'][, value] != 'FALSE'){
			loadICDMAPS()
			# Include ICD-9 terms linked to the current ICD-10 term
			setkey(CALIBER_GEM, icd10)
			icd9codes <- CALIBER_GEM[J(icdcodes), allow.cartesian = TRUE][
				use == TRUE & from9to10 == FALSE, icd9]
			out[CALIBER_DICT[, dict %in% c('icd9', 'icd9head') & code %in%
				icd9codes]] <- TRUE
		}
	} else if (dictionary == 'opcs'){
		if (exact){
			out <- CALIBER_DICT[, dict=='opcs' & code %in% regexpr]
		} else {
			out <- CALIBER_DICT[, dict=='opcs' & grepl(regexpr, code)]
		}
		if (META['read'][, value] != 'FALSE'){
			opcscodes <- CALIBER_DICT[out, code]
			tempMedcodes <- unique(
				CALIBER_DICTMAPS[(dict == 'opcs') &
					(code %in% opcscodes) &
					(map_stat %in% mapStatus), medcode])
			out[CALIBER_DICT[, dict=='read' &
				medcode %in% tempMedcodes]] <- TRUE
		}	
	} else if (dictionary == 'read'){
		if (exact){
			out <- CALIBER_DICT[, dict=='read' & code %in% regexpr]
		} else {
			out <- CALIBER_DICT[, dict=='read' & grepl(regexpr, code)]
		}
		# out should be a vector
		medcodes <- CALIBER_DICT[out, medcode]
		for (mapto in c('icd9', 'icd10', 'opcs')){
			if (META[item == mapto][, value] != 'FALSE'){
				tempcodes <- unique(
					CALIBER_DICTMAPS[(dict == mapto) &
						(medcode %in% medcodes) &
						(map_stat %in% mapStatus), code])
				out[CALIBER_DICT$dict == mapto &
					CALIBER_DICT$code %in% tempcodes] <- TRUE
			}
		}
	}
	class(out) <- 'selection'
	out
}



