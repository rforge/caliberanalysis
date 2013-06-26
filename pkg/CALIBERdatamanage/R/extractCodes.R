extractCodes <- function(data, codelist, enttypes = NULL,
	codename = switch(attr(codelist, 'Source'),
	GPRD = 'medcode', ONS = 'cod', HES = 'icd', OPCS = 'opcs',
	GPRDPROD = 'prodcode'), varname = attr(codelist, 'Name')){
	# Extracts and decodes a specified entity type from a 
	# ffdf or data.table, automatically using CALIBER lookups
	# to decode the data. Returns a data.table.
	
	# Coded integer values are converted to factors
	# YYYYMMDD dates are converted to IDate
	# Medcodes, prodcodes and score method are converted to factors
	# (Read terms, product names or score method) if the
	# CALIBERlookups package is available
	
	# Arguments: data -- ffdf or data.table or data.frame
	#            codelist -- a Read codelist
	#            enttypes -- vector of entity types to extract,
	#                   NULL to extract all
	#            varname -- new variable name
	
	# Requires the CALIBERlookups package, or lookups to be
	# in the 'data' folder of the current working directory

	if (is.null(varname)){
		# Default variable name is category
		varname <- 'category'
	}
	if (varname %in% colnames(data)){
		stop(paste('varname', varname,
			'must not already be present in data'))
	}
	if (!(codename %in% colnames(data))){
		stop(paste('No', codename, 'column in the data'))
	}

	# Prepare codelist
	mycodelist <- copy(codelist)
	catlabels <- copy(attr(codelist, 'Categories'))
	setkey(catlabels, category)
	mylabels <- catlabels[J(1:max(catlabels$category)),
		][, shortname]
	mylabels[is.na(mylabels)] <-
		as.character(which(is.na(mylabels)))
	mylabels[mylabels == ''] <-
		as.character(which(mylabels == ''))
	mycodelist[, category := factor(category,
		levels = 1:length(mylabels), labels = mylabels)]

	#### EXTRACTION FROM DIFFERENT SOURCES ####
	if (attr(codelist, 'Source') == 'GPRD'){
		mycodelist <- mycodelist[, list(medcode, category)]
		# Set name of medcode column to the same as in data
		setnames(mycodelist, 'medcode', codename)
		setnames(mycodelist, 'category', varname)
	} else if (attr(codelist, 'Source') == 'GPRDPROD'){
		mycodelist <- mycodelist[, list(medcode, category)]
		# Set name of medcode column to the same as in data
		setnames(mycodelist, 'prodcode', codename)
		setnames(mycodelist, 'category', varname)
	} else {
		# For OPCS use the code but remove .
		if (attr(codelist, 'Source') == 'OPCS'){
			mycodelist[, code := sub('\\.', '', code)]
		}
	}

	#### ONS, HES, OPCS ####
	if (attr(codelist, 'Source') %in% c('HES', 'ONS')){
		if (is.ffdf(data)){
			tomatch <- data.table(order = 1:nrow(data),
				icd = as.character(as.ram(data[, codename])),
				category = NA_integer_)
		} else {
			tomatch <- data.table(order = 1:nrow(data),
				icd = as.character(data[[codename]]),
				category = NA_integer_)
		}
		minICD <- min(nchar(mycodelist$code))
		maxICD <- max(nchar(mycodelist$code))
		for (i in minICD:maxICD){
			tomatch[, part := substr(tomatch[, icd], 1, i)]
			setkey(tomatch, part)
			temp <- subset(mycodelist, nchar(code) == i)
			setkey(temp, code)
			tomatch[, tempcat := temp[tomatch][, category]]
			tomatch[!is.na(tempcat), category := as.integer(tempcat)]
		}
	} else if (attr(codelist, 'Source') == 'OPCS'){
		# Ensure that opcs column is character
		if (is.ffdf(data)){
			tomatch <- data.table(order = 1:nrow(data),
				opcs = sub('\\.', '', as.character(as.ram(data[, codename]))))
		} else {
			tomatch <- data.table(order = 1:nrow(data),
				opcs = sub('\\.', '', as.character(data[[codename]])))
		}
		setkey(tomatch, opcs)
		setkey(mycodelist, code)
		tomatch[, category := as.integer(mycodelist[tomatch][, category])]
	} 

	if (attr(codelist, 'Source') %in% c('ONS', 'OPCS', 'HES')){
		# Restore original order of tomatch
		setkey(tomatch, order)
		# Extract subset of ffdf or data.table
		if (is.ffdf(data)){
			keep <- ff(!is.na(tomatch$category))
			rownames(data) <- NULL
			out <- subset(data, keep)
			eval(parse(text = paste('out$', varname,
				"<- as.ff(tomatch[!is.na(category), category])", sep = '')))
		} else {
			out <- subset(data, !is.na(tomatch$category))
			out[, varname := tomatch[!is.na(category), category],
				with = FALSE]
		}
	} else {
		# GPRD / GPRDPROD
		# Extract the subset with medcodes or prodcodes
		# of interest (need to do something else for ICD codes)
		if (is.ffdf(data)){
			mycodelist <- as.ffdf(as.data.frame(mycodelist))
		}
		out <- merge(data, mycodelist, by = codename)	
		# Restrict to entity types of interest
		if ((!is.null(enttypes)) & attr(codelist, 'Source') == 'GPRD'){
			if (!('enttype' %in% colnames(data))){
				stop('No enttype column in the data.')
			}
			out <- subset(out, enttype %in% enttypes)
		}
	}

	# Return the data.table or ffdf
	out
}

