addCategoryToCohort <- function(cohort, varname, data,
	old_varname = 'category', categories, binary = FALSE,
	limit_years = c(-Inf, 0), idcolname = attr(cohort, 'idcolname'),
	datecolname = 'eventdate', indexcolname = 'indexdate',
	overwrite = TRUE, description = NULL, limit_days = NULL){
	# Adds to the cohort data.table a column labelled varname
	# containing the value of a category or TRUE/FALSE for whether
	# any relevant categories are included.
	# Arguments: x = a cohort data.table with indexdate column
	#            varname = new variable name
	#            data = data.table containing anonpatid, medcode and eventdate
	#                          and enttype if entity type is specified
	#            categories = vector of categories to use, in priority order
	#                          (highest priority first). If the result is
	#                          binary, the order of categories does not matter.
	#            entities = which entity types to use, default = 'all',
	#                          or a numeric vector
	#            binary = whether to lump all categories together to make a
	#                          binary variable
	#            limit_years = earliest and latest year relative to index date
	#            limit_days = earliest and latest day relative to index date
	#            overwrite = whether to overwrite this variable if it exists
	#            description = new description for the variable

	if (is.null(description)){
		# Use the function call as the description
		thecall <- match.call()
		description <- paste(
			gsub('\n|\t| +', ' ', capture.output(print(thecall))), collapse = ' ')
	}

	# If categories are numeric but the data column is factor, change the
	# categories into a vector of factor level names
	if (is.factor(data[[old_varname]]) & is.numeric(categories)){
		categories <- levels(data[[old_varname]])[categories]
	}
	
	if (binary){
		# Select any events with one of the categories
		out <- addToCohort(cohort, varname, data, old_varname = old_varname,
			value_choice = function(x) {istrue(any(x %in% categories))},
			date_priority = 'all', limit_days = limit_days,
			limit_years = limit_years, idcolname = idcolname,
			datecolname = datecolname, indexcolname = indexcolname,
			overwrite = overwrite, description = description)
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
		out <- addToCohort(cohort, varname, data, old_varname = old_varname,
			value_choice = categories, date_priority = 'all',
			limit_days = limit_days, limit_years = limit_years,
			idcolname = idcolname, datecolname = datecolname,
			indexcolname = indexcolname, overwrite = overwrite,
			description = description)
	}
	invisible(out)
}

