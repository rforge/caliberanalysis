addToCohort <- function(cohort, varname, data, old_varname = 'value', 
	value_choice = function(x) max(x, na.rm = TRUE),
	date_priority = c('all', 'first', 'last'),
	limit_years = c(-Inf, 0), date_varname = NULL,
	idcolname = attr(cohort, 'idcolname'),
	datecolname = 'eventdate', indexcolname = 'indexdate',
	overwrite = TRUE, description = NULL, limit_days = NULL){
	# Adds to the x data.table a column labelled varname
	# containing the value of a category from a list of ID,
	# category, eventdate.
	# Arguments: x = a cohort object
	#            varname = new variable name
	#            data = data.table containing ID, value and eventdate
	#                          columns
	#            old_varname = variable name in data, default='value'
	#            value_choice = a vector of values (e.g. categories),
	#                          with the highest priority first
	#                          (i.e. the element which will be
	#                          chosen in preference if there is more than
	#                          one on the chosen date)
	#                          OR a function which takes a vector of
	#                          values and returns a single value, e.g.
	#                          mean, median, max, min, any, all.
	#                          Default is to choose the maximum
	#                          and ignore missing values.
	#            date_priority = if multiple records for a patient, which
	#                          record(s) to use based on date
	#            limit_years = a vector of length 2 for the time limits
	#                          (inclusive) in years before or after index date.
	#                          If NULL, it is calculated as limit_days / 365.25
	#            limit_days = a vector of length 2 for the time limits
	#                          (inclusive) in days before or after index date.
	#                          Takes priority over limit_years if both are supplied.
	#            date_varname = optional name for date variable for the date
	#                          of the event from which the category was drawn.
	#                          Not valid if date_priority is 'any'
	#            overwrite = whether to overwrite the variable if it already
	#                          exists in x, or merely fill in missing values.
	#            description = new description for the variable, defaults to the
	#                          function call
	
	#### Check that cohort is a cohort ####
	if (!is.cohort(cohort)){
		cohort <- as.cohort(cohort)
	}

	if (is.ffdf(cohort)){
		if (varname %in% colnames(cohort)){
			x <- data.table(.v1 = as.ram(cohort[[idcolname]]),
				.v2 = as.ram(cohort[[indexcolname]]),
				.v3 = as.ram(cohort[[varname]]),
				order = 1:nrow(cohort))
			setnames(x, '.v3', varname)
		} else {
			x <- data.table(.v1 = as.ram(cohort[[idcolname]]),
				.v2 = as.ram(cohort[[indexcolname]]),
				order = 1:nrow(cohort))
		}
		setnames(x, '.v1', idcolname)
		setnames(x, '.v2', indexcolname)
	} else {
		x <- cohort
	}
	setkeyv(x, attr(x, 'idcolname'))
	
	#### Function to choose a category ####
	if (is.vector(value_choice)){
		# choose a category
		choiceFun <- function(x){
			out <- chooseByPriority(x, value_choice)
			if (is.null(out)){
				out <- NA
			}
			out
		}
	} else if (is.function(value_choice)){
		# use the function as supplied
		choiceFun <- value_choice
	} else {
		stop(paste('value_choice must be a function (taking a vector', 
			'and returning a single value) or a vector of categories',
			'in priority order'))
	}

	if (is.null(limit_days)){
		if (length(limit_years) != 2){
			stop('limit_years should be a numeric vector of length 2')
		}
		limit_days <- limit_years * 365.25
	}
	limit_days <- sort(limit_days)
	if (length(limit_days) != 2){
		stop('limit_days should be a numeric vector of length 2')
	}

	#### Create a data.table DATA containing data of interest ####
	if (is.ffdf(data) | (is.data.frame(data) & !is.data.table(data))){
		if (old_varname == datecolname){
			DATA <- as.data.table(data[,
				c(idcolname, old_varname)])
			setnames(DATA, old_varname, 'value')
			DATA[, .eventdate := value]
		} else {
			DATA <- as.data.table(data[,
				c(idcolname, old_varname, datecolname)])
			setnames(DATA, old_varname, 'value')
			setnames(DATA, datecolname, '.eventdate')
		}
	} else if (is.data.table(data)){
		if (old_varname == datecolname){
			# Event date is the value to be extracted
			DATA <- copy(data[, c(idcolname, old_varname),
				with = FALSE])
			setnames(DATA, old_varname, 'value')
			DATA[, .eventdate := value]
		} else {
			DATA <- copy(data[, c(idcolname, old_varname, datecolname),
				with = FALSE])
			setnames(DATA, old_varname, 'value')
			setnames(DATA, datecolname, '.eventdate')
		}
	} else {
		stop('data is an unknown type of object. It should be a data.table, data.frame or FFDF data frame')
	}

	date_priority <- date_priority[1]
	if (!(date_priority %in% c('all', 'first', 'last'))){
		stop ('date_priority must be one of all, first, last')
	}
	
	#### Set indexes ####
	setkeyv(x, idcolname)
	setnames(DATA, idcolname, '.id')
	setkey(DATA, .id)

	#### Identifying events of interest ####
	DATA[, indexdate := x[DATA][, indexcolname, with = FALSE]]
	DATA[, daysdiff := as.numeric(.eventdate - indexdate)]
	DATA[, include := istrue(!is.na(.eventdate) &
		daysdiff >= limit_days[1] & daysdiff <= limit_days[2])]
	if (date_priority == 'first'){
		DATA[include == TRUE, keep := (.eventdate == min(.eventdate)),
			by = .id]
	} else if (date_priority == 'last'){
		DATA[include == TRUE, keep := (.eventdate == max(.eventdate)),
			by = .id]
	} else {
		DATA[, keep := include]
	}
	DATA[keep == TRUE, .chosen := choiceFun(value), by = .id]
	# Keep only one row per patient
	chooseFirst <- function(n){
		if (n == 0){
			logical(0)
		} else {
			c(TRUE, rep(FALSE, n - 1))
		}
	}
	DATA[keep == TRUE, use := chooseFirst(.N), by = .id]
	DATA[, use := istrue(use)]
	DATA[, value := NULL]
	DATA[, include := NULL]
	DATA[, keep := NULL]
	
	# Keep relevant rows only
	Y <- DATA[use == TRUE]
	setkey(Y, .id)

	if (!('.chosen' %in% colnames(Y))){
		warning('value_choice returns all null values')
		Y[, .chosen := NA]
	}
	
	#### Updating cohort table ####
	x[, .transferChosen := Y[x][, .chosen]]
	x[, .transferDate := Y[x][, .eventdate]]
	
	if (varname %in% colnames(x) & overwrite == FALSE){
		# update existing variable if missing
		update <- is.na(x[[varname]])
		x[update == TRUE, eval(parse(text = paste(
			varname, ':= .transferChosen', sep = '')))]
		if (!is.null(date_varname) & date_priority != 'all'){
			x[update == TRUE, eval(parse(text = paste(
				date_varname, ':= .transferDate', sep = '')))]
		}
	} else {
		# overwrite variable
		if (varname %in% colnames(x)){
			x[, (varname) := NULL]
		}
		setnames(x, '.transferChosen', varname)
		if (!is.null(date_varname)){
			x[, (date_varname) := NULL]
			setnames(x, '.transferDate', date_varname)
		}		
	}
	
	if ('.transferDate' %in% colnames(x)){
		x[, .transferDate := NULL]
	}
	if ('.transferChosen' %in% colnames(x)){
		x[, .transferChosen := NULL]
	}
	
	#### Add description (if any) ####
	if (is.null(description)){
		# Use the function call as the description
		thecall <- match.call()
		description <- paste(
			gsub('\n|\t| +', ' ', capture.output(print(thecall))), collapse = ' ')
	}
	if (is.ffdf(cohort)){
		modifyDescription(cohort, varname, description)
	} else {
		modifyDescription(x, varname, description)
	}

	#### Add description for date (if any) ####
	if (!is.null(date_varname)){
		if (is.ffdf(cohort)){
			modifyDescription(cohort, date_varname, 'date of ' %&% varname)
		} else {
			modifyDescription(x, date_varname, 'date of ' %&% varname)
		}
	}

	#### Summary message ####
	message(paste('Summary of ', varname, ':', sep = ''))
	if (is.vector(value_choice)){
		# categorical
		message(paste(capture.output(print(x[, .N, by = eval(varname)])),
			collapse = '\n'))
	} else {
		# continuous
		message(paste(capture.output(print(summary(x[[varname]]))),
			collapse = '\n'))
	}

	#### Return the updated cohort ####
	if (is.ffdf(cohort)){
		# restore original order
		setkey(x, order)
		for (i in 1:ncol(x)){
			if (!(colnames(x)[i] %in% c(idcolname, indexcolname))){
				# Transfer column back to original cohort
				if (colnames(x)[i] %in% colnames(cohort)){
					# remove old column
					cohort[[colnames(x)[i]]] <- NULL
				}
				# Flush ff attributes to avoid error
				setattr(x[[i]], 'physical', NULL)
				setattr(x[[i]], 'virtual', NULL)
				# Replace column
				cohort[[colnames(x)[i]]] <- as.ff(x[[i]])
			}
		}
		# Return entire cohort (as ffdf)
		invisible(cohort)
	} else {
		invisible(x)
	}
}
