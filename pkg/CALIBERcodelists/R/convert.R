# Function to convert a selection or codelist from
# one dictionary to another, using the mappings

convert <- function(x, toDictionary = NULL, fromDictionary = NULL, ...){
	# Converts a codelist or selection from one dictionary to
	# another. toDictionary must be supplied when using selections.

	# Possible conversions:
	# NHS mapping: Read -- ICD9, Read -- ICD10, Read -- OPCS
	# US GEM:      ICD9CM -- ICD10CM

	# If codelist, fromDictionary is determined by the
	# codelist.
	
	if (is.codelist(x)){
		fromDictionary <- getSourceDict(x)	
	}
	if (is.null(toDictionary)){
		toDictionary <- ALLDICTS[menu(ALLDICTS,
			title = 'Which dictionary to convert to?')]
	}
	if (is.null(fromDictionary)){
		fromDictionary <- ALLDICTS[menu(ALLDICTS,
			title = 'Which dictionary to convert from?')]
	}
	
	# Ensure that dictionaries and mappings 
	# are loaded before running this function
	loadDICT()
	if (fromDictionary == 'read' & toDictionary %in%
		c('icd9', 'icd10', 'opcs')){
		loadDICTMAPS()
	} else if (toDictionary == 'read' & fromDictionary %in%
		c('icd9', 'icd10', 'opcs')){
		loadDICTMAPS()
	} else if (toDictionary == 'icd10' & fromDictionary == 'icd9'){
		loadICDMAPS()
	} else if (fromDictionary == 'icd10' & toDictionary == 'icd9'){
		loadICDMAPS()	
	} else {
		stop('No mapping available for ' %&% fromDictionary %&%
			' to ' %&% toDictionary)
	}
	
	if (is.codelist(x)){
		return(convertCodelist(x, toDictionary, fromDictionary, ...))
	} else if (is.selection(x)){
		return(convertSelection(x, toDictionary, fromDictionary, ...))
	} else {
		stop('x is not a codelist or selection')
	}
}

convertCodelist <- function(x, toDictionary, fromDictionary, 
	mapStatus = NULL, toSource = NULL){
	# Converts a codelist from one dictionary to another
	# The attributes and categories are carried over from one
	# codelist to the other.
	metadata <- extractMetadataFromCodelist(x)
	if (is.null(toSource)){
		toSource <- SOURCEDICTS[dict == toDictionary, Source][1]
	}
	metadata$Source <- toSource
	cats <- unique(x$category)
	out <- rbindlist(lapply(cats, function(z){
		if (fromDictionary=='read'){
			if (toDictionary=='opcs'){
				newCodes <- convertMedcodes(
					x[category==z, medcode], 'opcs', mapStatus)
				if (length(newCodes) > 0){
					data.table(opcs_code=newCodes, category=z)
				} else {
					data.table(opcs_code=character(0), category=integer(0))
				}
			} else if (toDictionary=='icd10'){
				newCodes <- convertMedcodes(
					x[category==z, medcode], 'icd10', mapStatus)
				if (length(newCodes) > 0){
					data.table(icd_code=newCodes, category=z)
				} else {
					data.table(icd_code=character(0), category=integer(0))
				}
			} else if (toDictionary=='icd9'){
				newCodes <- convertMedcodes(
					x[category==z, medcode], 'icd9', mapStatus)
				if (length(newCodes) > 0){
					data.table(icd9_code=newCodes, category=z)
				} else {
					data.table(icd9_code=character(0), category=integer(0))
				}
			}
		} else if (toDictionary == 'read'){
			newMedcodes <- convertToMedcodes(
				x[category==z, code], fromDictionary, mapStatus)
			if (length(newMedcodes) > 0){
				data.table(medcode=newMedcodes, category=z)
			} else {
				data.table(medcode=integer(0), category=integer(0))
			}
		} else if (toDictionary == 'icd10' & fromDictionary == 'icd9'){
			# Note that from9to10 refers to the mapping for an individual
			# patient's codes, but for codelists the mapping is the opposite
			# way around
			newCodes <- CALIBER_GEM[icd9 %in% x[category==z, code] &
				use == TRUE & from9to10 == FALSE, icd10]
			if (length(newCodes) > 0){
				data.table(icd_code=newCodes, category=z)
			} else {
				data.table(icd_code=character(0), category=integer(0))
			}		
		} else if (toDictionary == 'icd9' & fromDictionary == 'icd10'){
			newCodes <- CALIBER_GEM[icd10 %in% x[category==z, code] & 
				use == TRUE & from9to10 == FALSE, icd9]
			if (length(newCodes) > 0){
				data.table(icd9_code=newCodes, category=z)
			} else {
				data.table(icd9_code=character(0), category=integer(0))
			}	
		}
	}))
	if (nrow(out)==0){
		message('No terms in codelist')
		if (toDictionary=='read'){
			out <- data.table(medcode=integer(0), code=character(0),
				category=integer(0), term=integer(0))
		} else {
			out <- data.table(code=character(0),
				category=integer(0), term=integer(0))
		}
		class(out) <- c('codelist', 'data.table', 'data.frame')
	} else {
		out <- tableToCodelist(out)
	}
	addAttributesToCodelist(out, metadata)
	if (toDictionary=='icd10'){
		out <- expandCodelist(out)
	}
	return(out)
}

convertSelection <- function(x, toDictionary, fromDictionary, ...){
	# Converts all terms in a selection from one dictionary
	# to another. Note that all ICD-10 matching has to be on 
	# string searching because we do not match to dagger terms
	# only to asterisk terms (suffix A) in the ICD10--> Read direction
	# or general terms.
	temp <- as.codelist(x, dictionary=fromDictionary)	
	out <- convertCodelist(temp, toDictionary, fromDictionary, ...)
	return(as.selection(out))
}

convertMedcodes <- function(medcodes,
	toDictionary, mapStatus=NULL){
	if (is.null(mapStatus)){
		mapStatus <- c('A', 'D', 'E', 'T')
	}
	if (toDictionary == 'icd10') {
		CALIBER_DICTMAPS[dict %in% c('icd10', 'icdhead') &
			map_stat %in% mapStatus & medcode %in% medcodes, code]
	} else if (toDictionary == 'icd9') {
		CALIBER_DICTMAPS[dict %in% c('icd9', 'icd9head') &
			map_stat %in% mapStatus & medcode %in% medcodes, code]		
	} else if (toDictionary == 'opcs') {
		CALIBER_DICTMAPS[dict == 'opcs' &
			map_stat %in% mapStatus & medcode %in% medcodes, code]
	}
}

convertToMedcodes <- function(codes,
	fromDictionary, mapStatus=NULL){
	if (is.null(mapStatus)){
		mapStatus <- c('A', 'D', 'E', 'G', 'T')
	}
	if (fromDictionary=='opcs'){
		CALIBER_DICTMAPS[dict == 'opcs' & map_stat %in% mapStatus &
		 	!grepl('D$|A$', code) & code %in% codes, medcode]
	} else if (fromDictionary=='icd10'){
		CALIBER_DICTMAPS[dict %in% c('icd10', 'icdhead') &
		 	map_stat %in% mapStatus &
		 	!grepl('A$', code) & 
		 	grepl(paste('^' %&% codes, collapse='|'), code), medcode]		
	}
}
