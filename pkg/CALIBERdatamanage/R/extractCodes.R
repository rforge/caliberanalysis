extractCodes <- function(data, codelist, categories = NULL,
	enttypes = NULL, codename = switch(attr(codelist, 'Source'),
	GPRD = 'medcode', ONS = 'cod', HES = 'icd', OPCS = 'opcs',
	GPRDPROD = 'prodcode'), varname = attr(codelist, 'Name')){
	# Extracts a subset of records with particular Read (medcode), OPCS,
	# ICD-10 or product codes.

	# Arguments: data -- ffdf or data.table or data.frame
	#            codelist -- a codelist
	#            categories -- optional vector of categories to include;
	#                   NULL to include all
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
	if (!('codelist' %in% class(codelist))){
		stop('codelist is not a codelist')
	}
	if (is.null(categories)){
		mycodelist <- copy(codelist)
	} else {
		mycodelist <- subset(codelist, category %in% categories)
	}
	catlabels <- as.data.table(copy(attr(codelist, 'Categories')))
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
		mycodelist <- mycodelist[, list(prodcode, category)]
		# Set name of prodcode column to the same as in data
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
		# Loop through categories, grepping ICD codes
		allcats <- unique(as.integer(mycodelist$category))
		for (i in allcats){
			regexpr <- paste(mycodelist[as.integer(category) == i,
				paste('^', code, sep = '')], collapse = '|')
			if ('parallel' %in% loadedNamespaces()){
				# Use parallel grep
				found <- pvec(tomatch$icd, function(x){
					grepl(regexpr, x)
				})
				tomatch[found, category := i]
			} else {
				tomatch[grepl(regexpr, icd), category := i]
			}
		}
	} else if (attr(codelist, 'Source') == 'OPCS'){
		# Ensure that opcs column is character
		if (is.ffdf(data)){
			tomatch <- data.table(order = 1:nrow(data),
				opcs = as.character(as.ram(data[, codename])))
		} else {
			tomatch <- data.table(order = 1:nrow(data),
				opcs = as.character(data[[codename]]))
		}
		setkey(tomatch, opcs)
		setkey(mycodelist, code)
		tomatch[, category := as.integer(mycodelist[tomatch][, category])]
	} 

	if (attr(codelist, 'Source') %in% c('ONS', 'OPCS', 'HES')){
		# Restore original order of tomatch
		setkey(tomatch, order)

		# Convert category back into a factor
		tomatch[, category := factor(category,
			levels = 1:length(mylabels), labels = mylabels)]

		# Extract subset of ffdf or data.table
		if (is.ffdf(data)){
			keep <- !is.na(tomatch$category)
			rownames(data) <- NULL
			data[[varname]] <- as.ff(tomatch$category)
			out <- subset(data, keep)
		} else {
			out <- subset(data, !is.na(tomatch$category))
			out[, varname := tomatch[!is.na(category), category],
				with = FALSE]
		}

	} else {
		# GPRD / GPRDPROD
		# Extract the subset with medcodes or prodcodes
		if (is.ffdf(data)){
			mycodelist <- as.ffdf(as.data.frame(mycodelist))
		}
		out <- merge(data, mycodelist, by = codename)	
		# Restrict to entity types of interest
		if ((!is.null(enttypes)) & attr(codelist, 'Source') == 'GPRD'){
			if (!('enttype' %in% colnames(data))){
				stop('No enttype column in the data.')
			}
			if (is.ffdf(out)){
				myenttype <- as.ffdf(data.frame(.temp = 1, enttype = enttypes))
				out <- merge(out, myenttype, by = 'enttype')
				out$.temp <- NULL
			} else {
				out <- subset(out, istrue(out$enttype %in% enttypes))
			}
		}
	}

	# Ensure the output is not considered to be a codelist
	setattr(out, 'class', setdiff(attr(out, 'class'), 'codelist'))

	# Return the data.table or ffdf
	out
}

