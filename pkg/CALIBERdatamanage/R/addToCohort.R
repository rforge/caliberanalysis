addToCohort <- function(x, varname, data, old_varname = 'value', 
	value_choice = function(x) max(x, na.rm = TRUE),
	date_priority = c('all', 'first', 'last'),
	limit_years = c(-Inf, 0), date_varname = NULL, idcolname = 'anonpatid',
	datecolname = 'eventdate', indexcolname = 'indexdate', overwrite = FALSE,
	description = NULL){
	# Adds to the x data.table a column labelled varname
	# containing the value of a category from a list of ID,
	# category, eventdate.
	# Arguments: x = a x object
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
	#                          record to use based on date
	#            limit_years = a vector of length 2 for the time limits
	#                          (inclusive) in years before or after index date
	#            date_varname = optional name for date variable for the date
	#                          of the event from which the category was drawn.
	#                          Not valid if date_priority is 'any'
	#            overwrite = whether to overwrite the variable if it already
	#                          exists in x, or merely fill in missing values.
	#            description = new description for the variable
	
	#### Checking input variables ####
	if (!is.cohort(x)){
		x <- as.cohort(x)
	}
	if (!(indexcolname %in% names(x))){
		stop(paste('x must contain a column named', indexcolname))
	}
	
	varname <- make.names(as.character(varname[1]))
	
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

	limit_years <- sort(unique(limit_years))
	if (length(limit_years)!=2){
		stop('limit_years should be a numeric vector of length 2')
	}
	
	if (!is.data.table(data)){
		DATA <- as.data.table(as.data.frame(data))
	} else {
		DATA <- copy(data[, c('anonpatid', old_varname, 'eventdate'),
			with = FALSE])
	}

	setnames(DATA, old_varname, 'value')
	setnames(DATA, datecolname, '.eventdate')

	date_priority <- date_priority[1]
	if (!(date_priority %in% c('all', 'first', 'last'))){
		stop ('date_priority must be one of all, first, last')
	}
	
	#### Set indexes ####
	setkeyv(x, attr(x, 'idcolname')) 
	setnames(DATA, idcolname, '.id')
	setkey(DATA, .id)
	browser()
	#### Identifying events of interest ####
	DATA[, indexdate := x[DATA][, indexcolname, with = FALSE]]
	DATA[, yearsdiff := as.numeric(.eventdate - indexdate)/365.25]
	DATA[, include := istrue(!is.na(.eventdate) &
		yearsdiff >= limit_years[1] & yearsdiff <= limit_years[2])]
	if (date_priority == 'first'){
		DATA[include == TRUE, include := (.eventdate == min(.eventdate)),
			by = .id]
	} else if (date_priority=='last'){
		DATA[include == TRUE, include := (.eventdate == max(.eventdate)),
			by = .id]
	}
	DATA[include == TRUE, .chosen := choiceFun(value), by = .id]
	# Keep only one row per patient
	DATA[include == TRUE,
		include := c(TRUE, rep(FALSE, length(include) - 1)), by = .id]
	
	DATA[, value := NULL]
	# Keep relevant rows only
	Y <- subset(DATA, include == TRUE)
	Y[, include := NULL]
	setkey(Y, .id)

	if (!('.chosen' %in% names(Y))){
		warning('value_choice returns all null values')
		Y[, .chosen := NA]
	}
	
	#### Updating cohort table ####
	x[, .transferChosen:=Y[x][, .chosen]]
	x[, .transferDate:=Y[x][, .eventdate]]
	
	if (varname %in% names(x) & overwrite == FALSE){
		# update existing variable if missing
		update <- is.na(x[[varname]])
		x[update == TRUE, eval(parse(text=paste(
			varname, ':= .transferChosen', sep = '')))]
		if (!is.null(date_varname) & date_priority != 'all'){
			x[update == TRUE, eval(parse(text = paste(
				date_varname, ':= .transferDate', sep = '')))]
		}
	} else {
		# overwrite variable
		if (varname %in% names(x)){
			x[, varname := NULL, with = FALSE]
		}
		setnames(x, '.transferChosen', varname)
		if (!is.null(date_varname)){
			x[, date_varname := NULL, with = FALSE]
			setnames(x, '.transferDate', date_varname)
		}		
	}
	
	if ('.transferDate' %in% names(x)){
		x[, .transferDate := NULL]
	}
	if ('.transferChosen' %in% names(x)){
		x[, .transferChosen := NULL]
	}
	
	#### Add description (if any) ####
	if (!is.null(description)){
		modifyDescription(x, varname, description)
	}

	#### Return a summary table ####
	if (is.vector(value_choice)){
		# categorical
		x[, .N, by = eval(varname)]
	} else {
		# continuous
		summary(x[[varname]])
	}
}
