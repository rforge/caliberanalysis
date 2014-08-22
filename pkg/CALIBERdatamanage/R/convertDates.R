convertDates <- function(data, datecolnames = NULL, verbose = TRUE){
	# Converts dates in data to IDate format
	# Arguments: data = a data.table or ffdf
	#            datecolnames = a vector of column names to convert
	
	if (is.null(datecolnames)){
		datecolnames <- colnames(data)
	} else if (length(datecolnames) == 0) {
		datecolnames <- character(0)
	} else if (identical(datecolnames, '')){
		datecolnames <- character(0)
	} else {
		datecolnames <- intersect(colnames(data), datecolnames)
	}
	
	if (is.ffdf(data)){
		convertDates.ffdf(data, datecolnames = datecolnames,
			verbose = verbose)
	}	else if (is.data.table(data)){
		convertDates.data.table(data, datecolnames = datecolnames,
			verbose = verbose)
	}
}
	

convertDates.data.table <- function(data, datecolnames,
	verbose){
	# Converts dates in data to IDate format
	# Doesn't convert columns with blank names
	# Arguments: data = a data.table
	#            datecolnames = a vector of column names to convert
	initial <- sapply(data, function(x) class(x)[1])
	
	# Update columns by reference
	# Backquote column names to allow names starting with
	# numbers to be used. However, empty column names
	# may not be used.
	for (colname in datecolnames){
		cat('colname', colname, '\n')
		if (colname == ''){
			# this is probably the row number, don't try to convert
		} else {
			thecommand <- paste('`', colname,
				'`:=textToDate(`', colname, '`)', sep = '')
			data[, eval(parse(text = thecommand))]
		}
	}

	if (verbose){
		# Show resultant classes types of columns
		final <- sapply(data, function(x) class(x)[1])
		already <- colnames(data)[initial == 'IDate']
		converted <- colnames(data)[final != initial]
		convertDatesMessage(already, converted)		
	}
	
	# Return the modified dataset
	invisible(data)
}

convertDatesMessage <- function(already, converted){
	if (length(already) == 0){
		message('No columns already IDate')
	} else if (length(already) == 1){
		message(paste('Columns', already, 'already IDate.'))
	} else {
		message(paste('Columns',
			paste(already, collapse = ', '), 'already IDate.'))
	}
	
	if (length(converted) == 0){
		message('No columns converted to IDate')
	} else if (length(converted) == 1){
		message(paste('Column', converted, 'converted to IDate.'))
	} else {
		message(paste('Columns', 
			paste(converted, collapse = ', '), 'converted to IDate.'))
	}
}


convertDates.ffdf <- function(data, datecolnames,
	verbose){
	# Converts dates in data to IDate format
	# optional
	# Arguments: data = a ffdf
	#            datecolnames = a vector of column names to convert

	# Try a small sample first
	sample <- as.data.table(as.data.frame(
		data[1:min(10, nrow(data)), ]))
	initial <- sapply(sample, function(x) class(x)[1])
	convertDates.data.table(sample, datecolnames, FALSE)
	trial <- sapply(sample, function(x) class(x)[1])
	toconvert <- colnames(sample)[initial != trial]
	
	# Update columns by reference
	# Backquote column names to allow names starting with
	# numbers to be used.
	if (length(toconvert) > 0){
		for (col in toconvert){
			data[[col]] <- ff(textToDate(as.ram(data[[col]])))
		}	
	}
	
	if (verbose){
		# List which columns have been converted
		sample <- as.data.table(as.data.frame(
			data[1:min(10, nrow(data)), ]))	
		final <- sapply(sample, function(x) class(x)[1])
		already <- colnames(data)[initial == 'IDate']
		converted <- colnames(data)[final != initial]
		convertDatesMessage(already, converted)
	}
		
	# Return the modified dataset
	invisible(data)
}
