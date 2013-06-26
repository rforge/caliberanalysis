extractEntity <- function(data, enttype,
	CALIBER_ENTITY = NULL, CALIBER_LOOKUPS = NULL, ...){
	# Extracts and decodes a specified entity type from a 
	# ffdf or data.table, automatically using CALIBER lookups
	# to decode the data. Returns a data.table.
	
	# Coded integer values are converted to factors
	# YYYYMMDD dates are converted to IDate
	# Medcodes, prodcodes and score method are converted to factors
	# (Read terms, product names or score method) if the
	# CALIBERlookups package is available
	
	# Arguments: data -- ffdf or data.table or data.frame
	#            enttype -- a single entity type to extract
	#            CALIBER_ENTITY -- entity definitions
	#            CALIBER_LOOKUPS -- lookups tables
	#            ... -- arguments to pass to YYYYMMDDtoDate
	
	# Requires the CALIBERlookups package, or lookups to be
	# in the 'data' folder of the current working directory

	if (is.null(CALIBER_ENTITY)){
		# Load the CALIBERlookups package, but do not 'require' it
		# explicity so that R CMD check does not report an error -
		# the CALIBERlookups package will not be available
		# to external users.
		eval(parse(text="require('CALIBERlookups', quietly=TRUE)"))
		data('CALIBER_ENTITY', envir = environment())
	}

	this_enttype <- enttype
	if (is.ffdf(data)){
		use <- as.ffdf(data.frame(enttype = this_enttype, use = TRUE))
		A <- as.data.table(as.data.frame(merge(data, use)))
		A[, use := NULL]
	} else {
		A <- as.data.table(data[data$enttype == this_enttype, ])
	}
	
	# Generate new columns with interpreted data
	# (doesn't translate medcode or prodcode)
	template <- CALIBER_ENTITY[enttype == this_enttype]
	# template is a data.table with 1 row
	
	uselookup <- function(vector, lookupname, categorycol, labelcol,
		stringsAsFactors = TRUE){
		if (length(find.package('CALIBERlookups', quiet = TRUE)) > 0){
			# Load the CALIBERlookups package, but do not 'require' it
			# explicity so that R CMD check does not report an error -
			# the CALIBERlookups package will not be available
			# to external users.
			eval(parse(text = "require('CALIBERlookups', quietly=TRUE)"))
			eval(parse(text = paste('data("', lookupname,
				'", envir = environment())', sep = '')))
			temp <- copy(get(lookupname))
			setkeyv(temp, categorycol)
			if (stringsAsFactors){
				factor(vector, levels = 1:max(vector, na.rm = TRUE),
					labels = temp[J(1:max(vector, na.rm = TRUE)),
						labelcol, with = FALSE, mult='first'][[labelcol]])
			} else {
				temp[J(vector), labelcol, with = FALSE,
					mult='first'][[labelcol]]
			}
		} else {
			stop('CALIBERlookups package unavailable.')
		}
	}

	# Remove any blank data columns
	for (i in 9:(template$data_fields + 1)){
		if ('data' %&% i %in% names(A)){
			A[, 'data' %&% i := NULL, with = FALSE]
		}
	}

	if (template$data_fields > 0){
		# Generate extra columns with interpreted information
		for (i in 1:template$data_fields){
			if (!(('data' %&% i) %in% colnames(A))){
				# This data column is not present in the data
				message('Column data' %&% i %&% ' is missing.')
			} else {
				# Create new variable names
				newname <- sub('\\.$', '',
					make.names(template[['data' %&% i]]))
				datatype <- as.character(template[['data' %&% i %&% '_lkup']])
				# Note that for N_A and P_A, category 0 is not 'missing';
				# however it will be coded to NA for consistency with the
				# other lookups. The original data are retained when
				# creating factors.
				if (datatype == 'Medical Dictionary'){
					# Convert to character instead of factor 
					# to remove unused terms
					temp <- uselookup(A[['data' %&% i]],
						'CALIBER_DICT', 'medcode', 'term',
						stringsAsFactors = FALSE)
					A[, newname := temp, with = FALSE]
					setnames(A, 'data' %&% i, newname %&% '.medcode')
				} else if (datatype ==  'Product Dictionary'){
					# Convert to character to remove unused products
					temp <- uselookup(A[['data' %&% i]],
						'CALIBER_PRODDICT', 'prodcode', 'prodname',
						stringsAsFactors = FALSE)
					A[, newname := temp, with = FALSE]
					# Convert the name of the original data column
					setnames(A, 'data' %&% i, newname %&% '.prodcode')
				} else if (datatype == 'Scoring'){
					temp <- uselookup(A[['data' %&% i]],
						'CALIBER_SCOREMETHOD', 'code', 'scoringmethod')
					A[, newname := temp, with = FALSE]
				} else if (datatype == 'YYYYMMDD'){
					# Find out type of result
					newtype <- rep('missing', nrow(A))
					newtype[istrue(A[['data' %&% i]] > 1800 &
						A[['data' %&% i]] < 2050)] <- 'year'
					newtype[istrue(A[['data' %&% i]] > 180000 & 
						A[['data' %&% i]] < 205000)] <- 'yearmonth'
					newtype[istrue(A[['data' %&% i]] > 18000000 &
						A[['data' %&% i]] < 20500000)] <- 'date'
					A[, newname := YYYYMMDDtoDate(A[['data' %&% i]], ...),
						with = FALSE]
					A[, newname %&% '.datetype' := newtype, with = FALSE]
				} else if (datatype == 'GEN_SDC'){
					newtype <- rep('missing', nrow(A))
					newtype[istrue(A[['data' %&% i]] > 3)] <- 'date'
					A[, newname := GEN_SDCtoDate(A[['data' %&% i]]),
						with = FALSE]
					A[, newname %&% '.datetype' := newtype, with = FALSE]
				} else if (datatype == ''){
					# No conversion necessary, just change the name
					setnames(A, 'data' %&% i, newname)
				} else {
					# need to load lookup tables
					if (is.null(CALIBER_LOOKUPS)){
						if (length(find.package('CALIBERlookups', quiet = TRUE)) > 0){
							# Load the CALIBERlookups package, but do not 'require' it
							# explicity so that R CMD check does not report an error -
							# the CALIBERlookups package will not be available
							# to external users.
							eval(parse(text = "require('CALIBERlookups', quietly=TRUE)"))
							data('CALIBER_LOOKUPS', envir = environment())
						} else {
							stop('CALIBERlookups package unavailable.')
						}	
					}
					if (is.null(CALIBER_LOOKUPS)){
						stop('Unable to fund CALIBER_LOOKUPS')
					}
					# CALIBER_LOOKUPS should exist by now
					if (datatype %in% CALIBER_LOOKUPS$lookup){
						thislookup <- CALIBER_LOOKUPS[lookup == datatype]
						# Add category number in front of description to avoid duplicates
						thislookup[, description := category %&% '. ' %&% description]
						setkey(thislookup, category)
						maxvalue <- max(thislookup$category)
						temp <- factor(A[['data' %&% i]],
							levels = 1:maxvalue,
							labels = thislookup[J(1:maxvalue)]$description)
						A[, newname := temp, with = FALSE]
					} 
				}
			}
		}
	}
	A
}

