# Functions for expanding or contracting ICD10 codelists

contractICD10 <- function(icdCodes){
	# Returns a vector of ICD codes with 3+ characters, choosing
	# the smallest possible codes e.g. I21 encompassing I210, I211, ...
	# Argument: a unique set of ICD10 codes (ALT_CODE format)
	
	# First remove blank ICD codes
	icdCodes <- icdCodes[!is.na(icdCodes)]
	icdCodes <- unique(icdCodes[icdCodes != ''])
	
	# Can make some changes find codes more quickly - use binary
	# search instead of grep
	
	# selectedICDcodes are ALT_CODE codes that actually exist.
	# allow for incorrect X or A or D at the end of codes
	# include any 5-character codes under a 4-character code
	selectedICDcodes <- unique(unlist(lapply(icdCodes, function(x){
		out <- CALIBER_DICT[dict=='icd10'][grepl('^' %&% x, code), code]
		if (length(out)==0){
			# try without terminal x
			temp <- CALIBER_DICT[dict=='icd10' & 
				grepl(sub('[ADX]$', '', x), code), code]
			if (length(temp)==1){
				out <- temp
			}
		}
		out
	})))
	# These are all four letter codes
	
	# Use the first 3 or 4 characters of the codes
	strip4 <- unique(substr(selectedICDcodes, 1, 4))
	strip3 <- unique(substr(selectedICDcodes, 1, 3))
		
	# Now find out whether the same set of codes is selected
	# by any of the stripped codes (level 4).
	
	# Select using the main dictionary, and select on the codes we have
	# and see if they are the same.
	
	checkCodes <- function(x){
		main <- CALIBER_DICT[dict=='icd10'][grepl('^' %&% x, code), code]
		our <- selectedICDcodes[grepl('^' %&% x, selectedICDcodes)]
		# All 4 character codes have to match up exactly
		# You are allowed extra 5 character codes in 'main' because if the
		# 4 character codes match up, all the 5 character codes are included
		# However if 'x' is a 5 character code, the match must be exact
		if (nchar(x)>4){
			if (length(main)==length(our)){
				return (all(sort(main)==sort(our)))
			} else {
				return(FALSE)
			}			
		} else {
			if (all(our[nchar(our)>4] %in% main[nchar(main)>4])){
				if (sum(nchar(our)==4) == sum(nchar(main)==4)){
					return (all(sort(main[nchar(main)==4]) == 
						sort(our[nchar(our)==4])))
				} else {
					return(FALSE)
				}
			} else {
				return(FALSE)
			}
		}
	}
	
	useStrip4 <- sapply(strip4, checkCodes)
	
	# Use all terms in strip4 and all terms not selected by any term in strip4
	useThisCode <- rep(TRUE, length(selectedICDcodes))
	if (any(useStrip4)){
		# no need to include terms which are selected by strip4
		for (parent in strip4[useStrip4]){
			useThisCode[grepl('^' %&% parent, selectedICDcodes)] <- FALSE
		}
		selectedICDcodes <- sort(unique(c(selectedICDcodes[useThisCode],
			strip4[useStrip4])))
	}
	
	# Now consider strip3
	useStrip3 <- sapply(strip3, checkCodes)
	
	useThisCode <- rep(TRUE, length(selectedICDcodes))
	if (any(useStrip3)){
		# no need to include terms which are selected by strip4
		for (parent in strip3[useStrip3]){
			useThisCode[grepl('^' %&% parent, selectedICDcodes)] <- FALSE
		}
		selectedICDcodes <- sort(unique(c(selectedICDcodes[useThisCode],
			strip3[useStrip3])))
	}
	selectedICDcodes
}

####################

contractCodelist <- function(codelist){
	# Returns a codelist with ICD10 codes grouped into category
	# headings if possible.
	# Argument: a codelist in the expanded format

	loadDICT()
	if (!is.codelist(codelist) |
		!(attr(codelist, 'Source') %in% c('HES', 'ONS'))){
		stop('Argument must be a ICD-10 codelist')
	}

	# Remove rows with missing ICD code
	if (any(is.na(codelist$code))){
		codelist <- subset(codelist, !is.na(code))
	}
	if (any(codelist$code=='')){
		codelist <- subset(codelist, code!='')
	}

	if (nrow(codelist) > 0){
		# Save attributes, in order to restore them later
		metadata <- extractMetadataFromCodelist(codelist)
		
		# Encode NA as -2
		codelist[is.na(category), category:=-2L]
	
		if (!identical(key(CALIBER_DICT), c('dict', 'code'))){
			setkey(CALIBER_DICT, 'dict', 'code')
		}
		out <- data.frame(do.call('rbind', lapply(
			as.list(sort(unique(codelist$category))), function(x){
				codes <- contractICD10(codelist[category==x, code])
				# Include all combinations of dict and code
				if (length(codes) > 0){
					tmp2 <- data.table(dict=c(rep('icd10', length(codes)),
						rep('icdhead', length(codes))), code=rep(codes, 2))
					setkey(tmp2, 'dict', 'code')
					tmp <- merge(CALIBER_DICT, tmp2)[, list(code, term)]
					tmp$category <- x
					tmp
				} else {
					return(NULL)
				}
			}
		)))
		names(out) <- c('code', 'term', 'category')
		out <- data.table(out)
	
		# Restore NA
		codelist[category==-2, category:=NA_integer_]
		out[category==-2, category:=NA_integer_]	
	
		# Restore attributes
		out <- addAttributesToCodelist(out, metadata)
		class(out) <- c('codelist', 'data.table', 'data.frame')
		setattr(out, 'Expanded', FALSE) 
		return(out)
	} else {
		# if the codelist is empty, don't do anything
		message('Codelist has no terms')
		setattr(codelist, 'Expanded', FALSE) 
		return(codelist)
	}
}

#######################

expandICD10 <- function(icdCodes, allow5char=FALSE){
	# Expands a contracted ICD-10 code list to include
	# all terms mapped to by these codes. Returns a vector of ICD-10 codes,
	# with the attribute 'hierarchy' stating whether the code is
	# parent, child or normal
	# Arguments: icdCodes - character vector of ICD-10 codes
	#            icd4charOnly - whether to ignore 5-letter ICD-10 codes
	#                    (e.g. body regions affected by musculoskeletal
	#                    disorders), default is not to expand all these terms.
	
	# First remove blank ICD codes
	icdCodes <- icdCodes[!is.na(icdCodes)]
	icdCodes <- unique(icdCodes[icdCodes != ''])
	
	expanded <- sort(unique(c(unlist(lapply(as.list(icdCodes), function(x){
		out <- CALIBER_DICT[dict=='icd10' & (allow5char | nchar(code)<=4)][
			grepl('^' %&% x, code), code]
		if (length(out)==0){
			# try without terminal x
			temp <- CALIBER_DICT[dict=='icd10' & (allow5char | nchar(code)<=4)][
				grepl(sub('X$', '', x), code), code]
			if (length(temp)==1){ # i.e. only allow if it is a unique match
				out <- temp
			}
		}
		return(out)
	})), icdCodes)))
	
	# Find out which are children, parents, headers etc.
	# Assuming that the original list was the 'contracted' form

	# Parents must have more than one match
	if (length(icdCodes) > 0){
		parent <- expanded %in% icdCodes[sapply(icdCodes, function(x){
			sum(grepl('^' %&% x, expanded), na.rm=TRUE) > 1
		})]
		child <- !(expanded %in% icdCodes) & !parent
		hierarchy <- ifelse(parent, 'parent', ifelse(child, 'child', 'normal'))
	} else {
		hierarchy <- character(0)
	}
	attr(expanded, 'hierarchy') <- hierarchy
	return(expanded)
}

##################################

expandCodelist <- function(codelist, ...){
	# Generate a table code, term, category with contracted
	# codelist, based on currently selected terms in icd10
	# If the codelist is not in the 'contracted' format, it is
	# contracted before being expanded, in order to include
	# ICD10 headings. The 'parent' terms in the output
	# hierarchy can be ignored in the output when mathing to 
	# actual terms in e.g. HES.
	# Arguments: codelist - ICD10 codelist to contract
	#            allow5char - whether to expand 5-character ICD codes
	#                    default to only expand to 4 character codes
	loadDICT()
	if (!is.null(attr(codelist, 'Source'))){
		if (!(attr(codelist, 'Source') %in% c('HES', 'ONS'))){
			stop('Codelist must have the Source attribute set to "HES" or "ONS"')
		}
	}

	# Remove rows with missing ICD code
	if (any(is.na(codelist$code))){
		codelist <- subset(codelist, !is.na(code))
	}
	if (any(codelist$code=='')){
		codelist <- subset(codelist, code!='')
	}

	if (nrow(codelist) > 0){
		# Save attributes
		metadata <- extractMetadataFromCodelist(codelist)
	
		# Encode NA as -2
		codelist[is.na(category), category:=-2L]
    # Encode no category as -3
    if (!('category' %in% names(codelist))){
      codelist[, category:=-3L]
    }    
		# Remove any missing ICDcodes
		
		if (!identical(key(CALIBER_DICT), c('dict', 'code'))){
			setkey(CALIBER_DICT, 'dict', 'code')
		}	
		out <- data.frame(do.call('rbind', lapply(
			as.list(sort(unique(codelist$category))), function(x){
				codes <- codelist[category==x, code]
				if (isContractedCodelist(codelist)){
					codes <- expandICD10(codes, ...)
				} else {
					codes <- expandICD10(contractICD10(codes), ...)
				}
				hierarchy <- attr(codes, 'hierarchy')
				tempdt <- data.table(dict=c(rep('icd10', length(codes)),
					rep('icdhead', length(codes))), code=rep(codes, 2),
					hierarchy=rep(hierarchy, 2))
				setkey(tempdt, 'dict', 'code')
				tmp <- merge(CALIBER_DICT, tempdt)[, list(code, term, hierarchy)]
				if (nrow(tmp)==0){
					tmp$category <- integer(0)
				} else {
					tmp$category <- x
				}
				tmp
			}
		)))
    if (ncol(out)==4){
      names(out) <- c('code', 'term', 'hierarchy', 'category')
      out <- data.table(out)
      out[category==-2, category:=NA_integer_]	
    } else {
      out <- data.table(code=character(0),
                        term=character(0),
                        hierarchy=character(0),
                        category=integer(0))
    }
		# Restore NA in original codelist
    if (nrow(codelist) > 0){
      codelist[category==-2, category:=NA_integer_]
    }
    # Remove category if it didn't exist before
    if (all(codelist$category==-3)){
      codelist[, category:=NULL]
    }
    
		# Restore attributes
		out <- addAttributesToCodelist(out, metadata)
		class(out) <- c('codelist', 'data.table', 'data.frame')
		setattr(out, 'Expanded', TRUE) 
		return(out)
	} else {
		message('Codelist has no terms')
		codelist[, hierarchy:=character(0)]
		setattr(codelist, 'Expanded', TRUE) 
		return(codelist)
	}
}

isExpandedCodelist <- function(codelist){
	# Whether a codelist is an expanded ICD10 codelist
	# Returns TRUE or FALSE
	# Argument: codelist to check
	if (is.codelist(codelist)){
		if (!(attr(codelist, 'Source') %in% c('HES', 'ONS'))){
			if (is.null(attr(codelist, 'Expanded'))){
				return(FALSE)
			} else if (attr(codelist, 'Expanded')==TRUE){
				return(TRUE)
			} else {
				return(FALSE)
			}
		} else {
			return(FALSE)
		}
	} else {
		return(FALSE)
	}
}

isContractedCodelist <- function(codelist){
	# Whether a codelist is a contracted ICD10 codelist
	# Returns TRUE or FALSE
	# Argument: codelist to check
	if (is.codelist(codelist)){
		if (attr(codelist, 'Source') %in% c('HES', 'ONS')){
			if (is.null(attr(codelist, 'Expanded'))){
				return(FALSE)
			} else if (attr(codelist, 'Expanded')==FALSE){
				return(TRUE)
			} else {
				return(FALSE)
			}
		} else {
			return(FALSE)
		}
	} else {
		return(FALSE)
	}
}

findICDhead <- function(icdCodes){
	tmp <- sapply(icdCodes, function(icdCode){
		CALIBER_DICT[dict=='icdhead' & code==icdCode, term][1]
	})
	tmp[is.na(tmp)] <- ''
	tmp
}
