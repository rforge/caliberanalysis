addCodelistToCohort <- function(cohort, varname, data, codelist,
	categories, entities = 'all', binary = FALSE,
	limit_years = c(-Inf, 0), idcolname = attr(cohort, 'idcolname'),
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

		# Merge data with codelist
		USE <- merge(data, medcodes, by = 'medcode')		
		if (is.numeric(entities)){
			if (!('enttype' %in% names(data))){
				stop('data does not contain an enttype column')
			}
			USE <- subset(USE,
				subset = !is.na(USE[[idcolname]]) & enttype %in% entities,
				select = c(idcolname, 'medcode', 'category', datecolname))
		} else if (entities == 'all') {
			# Merge data with codelist
			USE <- subset(USE,
				subset = !is.na(USE[[idcolname]]),
				select = c(idcolname, 'medcode', 'category', datecolname))
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

	if (is.null(description)){
		# Use the function call as the description
		thecall <- match.call()
		description <- paste(
			gsub('\n|\t| +', ' ', capture.output(print(thecall))), collapse = ' ')
	}
	
	#### Now use the USE data.table to get the results ####
	if (binary){
		# create a logical vector result
		USE[, value := istrue(category %in% categories)]
		# Select any events with medcode in one of the categories
		out <- addToCohort(cohort, varname, USE, old_varname = 'value',
			value_choice = function(x) any(istrue(x)),
			date_priority = 'all', 
			limit_years = limit_years, overwrite = overwrite, 
			idcolname = idcolname, datecolname = datecolname,
			description = description)
		# Convert to binary
		if (is.ffdf(out)){
			out[[varname]] <- as.ffdf(istrue(as.ram(out[[varname]])))
		} else if (is.data.table(out)){
			# Update by reference
			out[, eval(parse(text = paste(
				c(varname, ':= istrue(', varname, ')'), collapse='')))]	
		}
	} else {
		# Select events
		out <- addToCohort(cohort, varname, USE, old_varname = 'category',
			date_priority = 'all', 
			value_choice = categories, limit_years = limit_years,
			overwrite = overwrite, idcolname = idcolname,
			datecolname = datecolname, description = description)
	}
	invisible(out)
}

getNewName <- function(existing_names){
	# returns a valid variable name which is not in existing_names
	newname <- 'Z_'
	while (newname %in% existing_names){
		newname <- newname %&% '_'
	}
	newname
}
