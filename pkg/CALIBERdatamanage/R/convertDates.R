convertDates <- function(data, datecolnames = NULL){
	# Converts dates in data to IDate format
	# optional
	# Arguments: data = a data.table
	#            datecolnames = a vector of column names to convert

	if (!is.data.table(data)){
		stop('data must be a data.table')
	}

	if (is.null(datecolnames)){
		datecolnames <- names(data)
	} else if (length(datecolnames) == 0) {
		datecolnames <- character(0)
	} else if (identical(datecolnames, '')){
		datecolnames <- character(0)
	} else {
		datecolnames <- intersect(names(data), datecolnames)
	}

	# Update columns by reference
	# Backquote column names to allow names starting with
	# numbers to be used.
	for (col in datecolnames){
		data[, eval(parse(text = paste('`', col,
			'`:=textToDate(`', col, '`)', sep = '')))]
	}

	# Show resultant classes types of columns
	sapply(data, function(x) class(x)[1])
}
