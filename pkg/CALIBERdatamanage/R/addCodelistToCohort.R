addCodelistToCohort <- function(x, varname, data, codelist,
	categories, entities = 'all', binary = FALSE,
	limit_years = c(-Inf, 0), idcolname = 'anonpatid',
	datecolname = 'eventdate', indexcolname = 'indexdate',
	overwrite = TRUE, description = NULL){
	# Adds to the cohort data.table a column labelled varname
	# containing the value of a category from a list of anonpatid, medcode.
	# Currently only for Read codelists.
	# Arguments: x = a cohort data.table with indexdate column
	#            varname = new variable name
	#            data = data.table containing anonpatid, medcode and eventdate
	#                          and enttype if entity type is specified
	#            codelist = a codelist object for selection of events by medcode
	#            categories = vector of categories to use, in priority order
	#                          (highest priority first). If the result is
	#                          binary, the order of categories does not matter.
	#            entities = which entity types to use, default = 'all',
	#                          or a numeric vector
	#            binary = whether to lump all categories together to make a
	#                          binary variable
	#            limit_years = earliest and latest year relative to index date
	#            overwrite = whether to overwrite this variable if it exists
	#            description = new description for the variable
	
	#### Check that x is a cohort with  ####
	if (!is.cohort(x)){
		x <- as.cohort(x)
	}
	setkeyv(x, attr(x, 'idcolname'))
	
	#### Get medcodes of interest ####
	medcodes <- subset(codelist, category %in% categories)[,
		list(medcode, category)]

	#### Extract the relevant subset of events as data.table ####
	if (is.ffdf(data)){
		temp <- as.ffdf(medcodes)
		# remove the category column (this is done on a copy so does
		# not modify the original)
		data$category <- NULL
		temp <- merge(data, temp)
		if (is.numeric(entities)){
			if (!('enttype' %in% names(temp))){
				stop('No enttype column in data')
			}
			tempent <- as.ffdf(data.frame(enttype = entities, use = TRUE))
			temp <- merge(temp, tempent)
			temp$use <- NULL
		}
		USE <- as.data.table(as.data.frame(temp))
	} else {
		#### Convert to data.table if necessary ####
		if (is.data.frame(data)){
			data <- as.data.table(data)
		}
		
		#### Process the data.table ####
		if (is.null(key(data))){
			changeKeyData <- TRUE
			saveKeyData <- NULL
			setkey(data, 'medcode')
		} else if (!identical(key(data), 'medcode')){
			changeKeyData <- TRUE
			saveKeyData <- key(data)
			setkey(data, 'medcode')
		} else {
			changeKeyData <- FALSE
		}
		
		# If there is a 'category' column in data, ignore it.
		tempname <- NULL
		if ('category' %in% names(data)){
			tempname <- getNewName(names(data))
			setnames(data, 'category', tempname)
		}
		
		if (is.numeric(entities)){
			# Merge data with codelist
			if (!('enttype' %in% names(data))){
				stop('data does not contain an enttype column')
			}
			USE <- copy(data[medcodes][!is.na(anonpatid) & enttype %in% entities,
				list(anonpatid, medcode, category, eventdate)])
		} else if (entities == 'all') {
			# Merge data with codelist
			USE <- copy(data[medcodes][!is.na(anonpatid),
				list(anonpatid, medcode, category, eventdate)])
		} else {
			stop('entities must be a numeric vector or "all"')
		}
		
		#### Restore original indexes and column name ####
		if (changeKeyData){
			setkeyv(data, saveKeyData)
		}
		if (!is.null(tempname)){
			setnames(data, tempname, 'category')
		}
	}

	#### Now use the USE data.table to get the results ####
	if (binary){
		# create a logical vector result
		USE[, value := istrue(category %in% categories)]
		# Select any events with medcode in one of the categories
		addToCohort(x, varname, USE, old_varname = 'value',
			value_choice = function(x) any(istrue(x)),
			limit_years = limit_years, overwrite = overwrite, 
			idcolname = idcolname, datecolname = datecolname,
			description = description)
		x[, eval(parse(text = paste(
			c(varname, ':= istrue(', varname, ')'), collapse='')))]
	} else {
		# Select events
		addToCohort(x, varname, USE, old_varname = 'category',
			value_choice = categories, limit_years = limit_years,
			overwrite = overwrite, idcolname = idcolname,
			datecolname = datecolname, description = description)
	}
	
	# Return a tabulation
	x[, .N, by = eval(varname)]
}

getNewName <- function(existing_names){
	# returns a valid variable name which is not in existing_names
	newname <- 'Z_'
	while (newname %in% existing_names){
		newname <- newname %&% '_'
	}
	newname
}
