# New S3 object type for cohort - one row per patient
# Alphabetical column order

cohort <- function(x, idcolname = c('patid', 'anonpatid', 'id'),
	description = NULL){
	# Designates a data.table as a 'cohort'
	# Creates a data dictionary
	# Data.tables are not copied
	if (is.cohort(x)){
		out <- x
	} else if (is.data.table(x)){
		out <- x
	} else if (is.ffdf(x)){
		out <- x
		setattr(out, 'class', c('cohort', class(out)))
	} else {
		out <- as.data.table(as.data.frame(x))
	}

	# Find ID column
	idcolname <- idcolname[which(idcolname %in% colnames(out))]
	if (length(idcolname) == 1){
		setattr(out, 'idcolname', idcolname)
	} else if (length(idcolname) > 1){
		stop('Multiple potential ID columns, please specify')
	} else {
		stop('Invalid ID column name')
	}
	if (any(is.na(out[[idcolname]]))){
		stop('ID may not be missing')
	}
	if (length(unique(out[[idcolname]])) < nrow(out)){
		stop('ID must be unique')
	}

	if (is.null(description)){
		description <- data.frame(colname = setdiff(colnames(x), idcolname),
			description = '', stringsAsFactors = FALSE)
	} else {
		description <- data.frame(description)
		if (!identical(colnames(description), c('colname', 'description'))){
			stop('Description table must have two columns: colname and description')
		}
	}
	# Keep only the entries in description that are relevant to this cohort
	description <- subset(description, colname %in% names(x))
	description <- description[order(description$colname), ]

	# Change the class to cohort
	if (!('cohort' %in% class(out))){
		setattr(out, 'class', c('cohort', class(out)))
	}
	
	# Set column order to be alphabetical (only works for data.tables)
	if (is.data.table(out)){
		setcolorder(out, c(idcolname, sort(setdiff(names(out), idcolname))))
		setkeyv(out, idcolname)
	}
	setattr(out, 'description', description)
	out
}

purgeDescription <- function(x){
	if (!is.cohort(x)){
		warning('This function only works on cohort objects')
	} else {
		DESC <- attr(x, 'description')
		DESC <- subset(DESC, colname %in% colnames(x))
		setattr(x, 'description', DESC)
		invisible(x)
	}
}

modifyDescription <- function(x, colname, description){
	# Modify the description attribute of a cohort file
	# Arguments: x = a cohort file
	#            colname = vector of column names
	#            description = vector of descriptions
	if (!is.cohort(x)){
		stop('x must be a cohort object')
	}

	# colname and description can be a vector
	if (length(colname) > 1){
		todo <- data.frame(colnames = colname, descriptions = description,
			stringsAsFactors = FALSE)
		for (i in 1:nrow(todo)){
			modifyDescription(x, todo$colnames[i], todo$descriptions[i])
		}
	} else {
		.colname <- colname	
		rm(colname)
		.description <- description
		rm(description)

		DESC <- attr(x, 'description')
		if (.colname %in% DESC$colname){
			DESC[.colname == DESC$colname, 'description'] <- .description
		} else {
			if (!(.colname %in% colnames(x))){
				warning(.colname %&% ' not a column in x')				
			}
			DESC <- copy(rbind(DESC, data.frame(
				colname = as.character(.colname),
				description = as.character(.description),
				stringsAsFactors = FALSE)))
		}
		DESC <- DESC[order(DESC$colname), ]
		setattr(x, 'description', DESC)
	}
	invisible(x)
}

as.cohort <- function(x, ...){
	if (is.cohort(x)){
		x
	} else {
		cohort(x, ...)
	}
}

is.cohort <- function(x){
	if ('cohort' %in% class(x)){
		TRUE
	} else {
		FALSE
	}
}

print.cohort <- function(x, ...){
	# Prints the summary and then the cohort file itself
	summary.cohort(x)
	if (is.data.table(x)){
		setcolorder(x, c(attr(x, 'idcolname'),
			sort(setdiff(names(x), attr(x, 'idcolname')))))
	}
	cat('\nDATA\n')
	if (is.data.table(x)){
		data.table:::print.data.table(x)
	} else if (is.ffdf(x)){
		ff:::print.ffdf(x)
	}
}

summary.cohort <- function(object, ...){
	# Prints a summary of a cohort
	cat('Cohort with', nrow(object),
		'patients; ID column =', attr(object, 'idcolname'), '\n')
	cat('\nCOLUMN DESCRIPTIONS\n')

	getclass <- function(colnames){
		# Get the class of columns in a data.frame, returning
		# NULL if the column does not exist
		trygetclass <- function(z){
			if (z %in% colnames(object)){
				class(object[[z]])[1]
			} else {
				'NULL'
			}
		}
		sapply(colnames, trygetclass)
	}

	description <- attr(object, 'description')
	
	if (is.ffdf(object)){
		object <- as.data.table(as.data.frame(object[1, ]))
	}

	cat(paste(description$colname %&% ' (' %&%
		getclass(description$colname) %&% '): ' %&%
		truncateChar(description$description,
		getOption('width') - nchar(description$colname) -
		nchar(getclass(description$colname)) - 7), collapse='\n'))
	cat('\n')
}

truncateChar <- function(x, maxchar){
	# Truncates a character vector so that each element does not have more
	# than a specified number of characters, adding ... to the end of 
	# truncated terms
	# Arguments: x - character string to truncate
	#            maxchar - length to truncate to
	convert <- nchar(x) > maxchar
	x[convert] <- substr(x[convert], 1, maxchar-3) %&% '...'
	x
}


subset.cohort <- function(x, subset, select, ...){
	# S3 method for subsetting a cohort
	# Arguments: x - cohort
	#            subset - a logical expression for rows to keep,
	#                   default is to keep all rows.
	#            select - a character vector of columns to keep,
	#                   default is to keep all columns.
	#                   The ID column is always kept.
	
	# Ensure that as a minimum, code, medcode if GPRD,
	# term and category are kept. It is only valid as a codelist
	# if these columns are kept
	
	if (!missing(select)){
		select <- c(attr(x, 'idcolname'),
			sort(unique(setdiff(select, attr(x, 'idcolname')))))
	}

	if (missing(subset)) {
		includeRow <- TRUE
	}	else {
		expr <- substitute(subset)
		includeRow <- eval(expr, x, parent.frame())
		if (!is.logical(includeRow)) 
			stop("'subset' must evaluate to a Boolean vector")
		includeRow[is.na(includeRow)] <- FALSE
	}
	
	if (is.data.table(x)){
		out <- data.table:::subset.data.table(x, includeRow, select, ...)
	} else if (is.ffdf(x)){
		out <- ffbase:::subset.ffdf(x, includeRow, select, ...)
	}
	out <- as.cohort(out, idcolname = attr(x, 'idcolname'),
		description = attr(x, 'description')) 
	out <- purgeDescription(out)
	out
}

removeColumns <- function(x, colnames){
	# Removes one or more columns from a cohort file and description
	for (colname in colnames){
		if (is.data.table(x)){
			x[, colname := NULL, with = FALSE]
		} else if (is.ffdf(x)){
			# <- creates a copy of the object and leads to loss of 
			# attributes. Hence it is necessary to store these
			# attributes and restore them afterwards
			x[[colname]] <- NULL
		}
	}
	DESC <- attr(x, 'description')
	DESC <- subset(DESC, colname %in% colnames(x))
	setattr(x, 'description', DESC)
	# Return the dataset, invisibly
	invisible(x)
} 

merge.cohort <- function(x, y, ...){
	# Merges two cohorts, warning if there are common columns
	# Combines the descriptions
	if (!(is.cohort(x) & is.cohort(y))){
		stop('x and y must be cohort objects')
	}
	
	# If one of the objects is a data.table, both must be data.tables
	if (is.data.table(x) & !is.data.table(y)){
		y <- as.data.table(y)
	}
	if (is.data.table(y) & !is.data.table(x)){
		y <- as.data.table(x)
	}

	if (attr(x, 'idcolname') != attr(y, 'idcolname')){
		y <- copy(y)
		if (attr(x, 'idcolname') %in% colnames(y)){
			stop('ID column names in x and y are different')
		} else {
			if (is.data.table(y)){
				setnames(y, attr(y, 'idcolname'), attr(x, 'idcolname'))
			} else if (is.ffdf(y)){
				y[[attr(x, 'idcolname')]] <- y[[attr(y, 'idcolname')]]
			}
		}
	}

	if (length(intersect(colnames(x), colnames(y))) > 1){
		warning('Columns ' %&%
			paste(intersect(colnames(x), colnames(y)), collapse=', ') %&%
			' are in both cohort datasets')
	}

	if (is.ffdf(x)){
		out <- ffbase:::merge.ffdf(x, y, by = attr(x, 'idcolname'), ...)
	} else {
		out <- data.table:::merge.data.table(x, y, by = attr(x, 'idcolname'), ...)
	}
	out <- cohort(out, idcolname = attr(x, 'idcolname'),
		description <- rbind(attr(x, 'description'), attr(y, 'description')))
	out	
}


