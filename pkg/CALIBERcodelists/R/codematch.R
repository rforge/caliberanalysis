codematch <- function(regexpr,
	dictionary=ifelse(length(getdictionary())==1, getdictionary(), ''), 
	mapStatus=NULL, exact=FALSE){
	# Identifies terms by their ICD-10, ICD-9, OPCS or Read code, and also
	# selects mapped terms (there are no ICD-9 terms so these are only
	# used for mapping to Read)
	# Arguments: regexpr - pattern to match ICD-10 codes of interest
	#            dictionary - a single dictionary in which to look for codes
	#            mapStatus - D=default, A=alternative, G=ICD-10 or OPCS
	#                    term is more general than Read, E=exact,
	#                    R=terms to exclude in order to code to this term
	#            exact - whether regexpr is to be interpreted as exact
	#                    instead of a regular expresion

	# Ensure that dictionary and mappings are available
	loadDICT()
	loadDICTMAPS()

	if (dictionary == ''){
		dictionary <- select.list(ALLDICTS,
			title='Which type of code to search for ' %&% regexpr %&% '?',
			multiple=FALSE, graphics=FALSE)
	}
	
	if (length(dictionary) != 1){
		stop('The dictionary argument should contain exactly one dictionary, not ' %&%
			length(dictionary))
	}
	
	if (!(dictionary %in% c(ALLDICTS, 'icd9'))){
		stop('Dictionary is ' %&% dictionary %&% ' but should be one of ' %&%
			paste(ALLDICTS, collapse=', '))
	}
	
	if (is.null(mapStatus)){
		mapStatus <- switch(dictionary, read=c('A', 'D', 'E', 'T'), 
			icd10=c('A', 'D', 'G', 'E', 'T'), icd9=c('A', 'D', 'G', 'E', 'T'), 
			opcs=c('A', 'D', 'G', 'E', 'T'))
	}
	
	if (dictionary=='icd10'){
		if (exact){
			out <- CALIBER_DICT[, dict %in% c('icd10', 'icdhead') & code==regexpr]
		} else {
			out <- CALIBER_DICT[, dict %in% c('icd10', 'icdhead') & grepl(regexpr, code)]
		}
		if (META['read'][, value]=='TRUE'){
			icdcodes <- CALIBER_DICT[out, code]
			tempMedcodes <- unique(
				CALIBER_DICTMAPS[(dict =='icd10') &
					(code %in% icdcodes) &
					(map_stat %in% mapStatus), medcode])
			out[CALIBER_DICT[, dict=='read' & medcode %in% tempMedcodes]] <- TRUE
		}
	} else if (dictionary=='opcs'){
		if (exact){
			out <- CALIBER_DICT[, dict=='opcs' & code==regexpr]
		} else {
			out <- CALIBER_DICT[, dict=='opcs' & grepl(regexpr, code)]
		}
		if (META['read'][, value]=='TRUE'){
			opcscodes <- CALIBER_DICT[out, code]
			tempMedcodes <- unique(
				CALIBER_DICTMAPS[(dict == 'opcs') &
					(code %in% opcscodes) &
					(map_stat %in% mapStatus), medcode])
			out[CALIBER_DICT[, dict=='read' & medcode %in% tempMedcodes]] <- TRUE
		}	
	} else if (dictionary=='read'){
		if (exact){
			out <- CALIBER_DICT[, dict=='read' & regexpr==code]
		} else {
			out <- CALIBER_DICT[, dict=='read' & grepl(regexpr, code)]
		}
		# out should be a vector
		medcodes <- CALIBER_DICT[out, medcode]
		if (META['icd10'][, value]=='TRUE'){
			tempICDcodes <- unique(
				CALIBER_DICTMAPS[(dict == 'icd10') & (medcode %in% medcodes) &
					(map_stat %in% mapStatus), code])
			out[CALIBER_DICT$dict=='icd10' & CALIBER_DICT$code %in% tempICDcodes] <- TRUE
		}
		if (META['opcs'][, value]=='TRUE'){
			tempOPCScodes <- unique(
				CALIBER_DICTMAPS[(dict == 'opcs') & (medcode %in% medcodes) &
					(map_stat %in% mapStatus), code])
			out[CALIBER_DICT$dict=='opcs' & CALIBER_DICT$code %in% tempOPCScodes] <- TRUE
		}
	} else if (dictionary=='icd9'){
		if (exact){
			out <- CALIBER_DICTMAPS[, dict == 'icd9' & code==regexpr]
		} else {
			out <- CALIBER_DICTMAPS[, dict == 'icd9' & grepl(regexpr, code)]
		}
		# can only select Read terms
		if (META['read'][, value]=='TRUE'){
			icdcodes <- unique(CALIBER_DICTMAPS[out, code])
			tempMedcodes <- unique(
				CALIBER_DICTMAPS[(dict =='icd9') &
						(code %in% icdcodes) &
						(map_stat %in% mapStatus), medcode])
			out <- CALIBER_DICT[, dict=='read' & medcode %in% tempMedcodes]
		}
	}
	class(out) <- 'selection'
	out
}



