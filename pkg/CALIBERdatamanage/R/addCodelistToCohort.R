addCodelistToCohort <- function(cohort, varname, data, codelist,
	categories = NULL, enttypes = NULL,
	codename = switch(attr(codelist, 'Source'),
	GPRD = 'medcode', ONS = 'cod', HES = 'icd', OPCS = 'opcs',
	GPRDPROD = 'prodcode'), binary = FALSE,
	limit_years = c(-Inf, 0), idcolname = attr(cohort, 'idcolname'),
	datecolname = c('eventdate', 'evdate', 'dod', 'epistart', 'admidate'),
	indexcolname = 'indexdate', overwrite = TRUE,
	description = NULL, limit_days = NULL, entities = 'all'){
	# Adds to the cohort data.table a column labelled varname
	# containing the value of a category from a list of anonpatid, medcode.

	# Arguments: x = a cohort data.table with indexdate column
	#            varname = new variable name
	#            data = data.table containing anonpatid, medcode and eventdate
	#                          and enttype if entity type is specified
	#            codelist = a codelist object for selection of events by medcode
	#            categories = vector of categories to use, in priority order
	#                          (highest priority first). If the result is
	#                          binary, the order of categories does not matter.
	#            enttypes = which entity types to use, default = NULL (all),
	#                          or a numeric vector
	#            binary = whether to lump all categories together to make a
	#                          binary variable
	#            limit_years = earliest and latest year relative to index date
	#            limit_days = earliest and latest day relative to index date
	#            overwrite = whether to overwrite this variable if it exists
	#            description = new description for the variable
	#            entities = old version of enttypes argument, now deprecated

	# If categories is NULL, use all the categories in the codelist
	if (is.null(categories)){
		categories <- unique(codelist$category)
	}

	if (!identical(entities, 'all')){
		warning('The entities argument is deprecated. Use entttpes instead. enttypes = NULL (default) means that any entity types are allowed.')
		enttypes <- entities
	}
	
	# Find out which date column to use
	# (e.g. CPRD datasets will contain 'eventdate', HES datasets will
	# contain epistart etc.)
	
	test <- as.data.table(data[1, ])
	datecols <- sapply(test, function(x){
		'Date' %in% class(x)
	})
	datecolname <- colnames(test)[colnames(test) %in% datecolname & datecols][1]
	if (length(datecolname) == 0){
		stop('Unable to identify date column in data')
	}

	#### Extract entries of interest ####
	DATA <- as.data.table(extractCodes(data = data, codelist = codelist,
		categories = categories, enttypes = enttypes,
		codename = codename, varname = '.category'))
	# Convert .category from a factor to raw numerical values
	DATA[, .category := as.numeric(.category)]

	if (is.null(description)){
		# Use the function call as the description
		thecall <- match.call()
		description <- paste(gsub('\n|\t| +', ' ',
			capture.output(print(thecall))), collapse = ' ')
	}
		
	#### Now get the results ####
	if (binary){
		# create a logical vector result
		DATA[, value := istrue(.category %in% categories)]
		# Select any events with medcode in one of the categories
		out <- addToCohort(cohort, varname, DATA, old_varname = 'value',
			value_choice = function(x) any(istrue(x)),
			date_priority = 'all', limit_days = limit_days,
			limit_years = limit_years, overwrite = overwrite, 
			idcolname = idcolname, datecolname = datecolname,
			indexcolname = indexcolname, description = description)
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
		out <- addToCohort(cohort, varname, DATA,
			old_varname = '.category',
			date_priority = 'all', limit_days = limit_days,
			value_choice = categories, limit_years = limit_years,
			overwrite = overwrite, idcolname = idcolname,
			datecolname = datecolname, indexcolname = indexcolname,
			description = description)
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
