extractMedcodes <- function(data, codelist, enttypes = NULL,
	varname = attr(codelist, 'Name')){
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

	if (varname %in% colnames(data)){
		stop('varname must not already be present in data')
	}
	if (!('medcode' %in% colnames(data))){
		stop('No medcode column in the data')
	}

	mycodelist <- copy(codelist)
	catlabels <- copy(attr(codelist, 'Categories'))
	setkey(catlabels, category)
	mylabels <- catlabels[J(1:max(catlabels$category)),
		][, shortname]
	mylabels[is.na(mylabels)] <-
		as.character(which(is.na(mylabels)))

	mycodelist[, category := factor(category,
		levels = 1:length(mylabels), labels = mylabels)]
	mycodelist <- mycodelist[, list(medcode, category)]
	setnames(mycodelist, 'category', varname)

	# Extract the subset with medcodes of interest
	if (is.ffdf(data)){
		mycodelist <- as.ffdf(as.data.frame(mycodelist))
	}
	out <- merge(data, mycodelist, by = 'medcode')	

	# Restrict to entity types of interest
	if (!is.null(enttypes)){
		if (!('enttype' %in% colnames(data))){
			stop('No enttype column in the data.')
		}
		out <- subset(out, enttype %in% enttypes)
	}

	# Return the data.table or ffdf
	out
}

