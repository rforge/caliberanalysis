explode <- function(x = NULL, level = 3,
	keep = FALSE, minFreqGPRD = 100, ...){
	# Returns a Boolean vector for terms which have the same first few
	# characters as selected terms but which are not currently selected.
	# Arguments: x - a selection object or something which can be coerced
	#                    to a selection
	#            level - the number of characters of the code which should
	#                    match
	#            keep - whether to keep original terms in the result
	#            minFreqGPRD - minimum number of events in GPRD for Read
	#                    codes. This argument is ignored for ICD-10 and
	#                    OPCS codes.

	selected <- as.selection(x, ...)
	loadDICT()
		
	if (!is.selection(selected)){
		stop('Unable to coerce x to a selection object')
	}

	setDictKey()
	sel1 <- copy(CALIBER_DICT[as.logical(selected == TRUE),
		list(dict, partcode = substr(code, 1, level), sel = TRUE)])

	# ICD-10 and ICD9 codes should also match to the headers
	temp <- sel1[dict=='icd10']
	if (nrow(temp) > 0){
		temp[, dict:='icdhead']
		sel1 <- rbind(sel1, temp)
	}
	temp <- sel1[dict=='icdhead']
	if (nrow(temp) > 0){
		temp[, dict:='icd10']
		sel1 <- rbind(sel1, temp)
	}
	temp <- sel1[dict=='icd9']
	if (nrow(temp) > 0){
		temp[, dict:='icd9head']
		sel1 <- rbind(sel1, temp)
	}
	temp <- sel1[dict=='icd9head']
	if (nrow(temp) > 0){
		temp[, dict:='icd9']
		sel1 <- rbind(sel1, temp)
	}

	sel2 <- sel1[!duplicated(sel1)]
	
	if (nrow(sel2) > 0){
		setkey(sel2, dict, partcode)

		# matchto is drawn from the whole of CALIBER_DICT
		matchto <- copy(CALIBER_DICT[,
			list(dict, code, partcode=substr(code, 1, level))])
		setkey(matchto, dict, partcode)	

		# find out which of matchto matches up with the 3-character codes
		matchto[, sel2match:=sel2[matchto][, sel]]
		matchto[is.na(sel2match), sel2match:=FALSE]

		# Restore the order
		setkey(matchto, dict, code)
		
		# Return new terms which were not already selected
		# require mininum number of events if not GPRD
		out <- matchto[, sel2match] & !as.logical(selected) & 
			(CALIBER_DICT[, events>=minFreqGPRD] |
			CALIBER_DICT[, dict!='read'])	
		class(out) <- 'selection'

		# Whether the original selection should be retained
		if (keep==TRUE){
			out <- out %OR% selected
		}
	} else {
		# Whether the original selection should be retained
		if (keep==FALSE){
			# Null selection
			out <- as.selection(rep(FALSE, nrow(CALIBER_DICT)))
		}
	}
	return(out)
}
