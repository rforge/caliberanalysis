# New S3 object type for cohort - one row per patient
# Alphabetical column order

cohort <- function(x, idcolname = c('patid', 'anonpatid', 'id'),
	description = NULL){
	# Designates a data.table as a 'cohort'
	# Creates a data dictionary
	# Data.tables are not copied
	if (!is.data.table(x)){
		out <- as.data.table(as.data.frame(x))
	} else {
		out <- x
	}

	# Find ID column
	idcolname <- idcolname[which(idcolname %in% names(out))]
	if (length(idcolname) == 1){
		setattr(out, 'idcolname', idcolname)
	} else {
		stop('Invalid ID column  name')
	}
	if (any(is.na(out[[idcolname]]))){
		stop('ID may not be missing')
	}
	if (length(unique(out[[idcolname]])) < nrow(out)){
		stop('ID must be unique')
	}

	if (is.null(description)){
		description <- data.table(colname = setdiff(names(x), idcolname),
			description = '')
	} else {
		description <- data.table(description)
		if (names(description) != c('colname', 'description')){
			stop('Description table must have two columns: colname and description')
		}
	}
	# Keep only the entries in description that are relevant to this cohort
	description <- subset(description, colname %in% names(x))
	setkey(description, colname)

	# Change the class to cohort
	if (!('cohort' %in% class(out))){
		setattr(out, 'class', c('cohort', 'data.table', 'data.frame'))
	}
	
	# Set column order to be alphabetical
	setcolorder(out, c(idcolname, sort(setdiff(names(out), idcolname))))
	setkeyv(out, idcolname)
	setattr(out, 'description', description)
	out
}

purgeDescription <- function(x){
	if (!is.cohort(x)){
		stop('This function purges descriptions for nonexistent columns from a cohort object')
	} else {
		DESC <- attr(x, 'description')
		DESC <- subset(DESC, colname %in% names(x))
		setattr(x, 'description', DESC)
		invisible(DESC)
	}
}

modifyDescription <- function(x, colname, description){
	# to modify the description attribute of a cohort file.
	if (!is.cohort(x)){
		stop('x must be a cohort object')
	}

	# colname and description can be a vector
	if (length(colname) > 1){
		todo <- data.table(colnames = colname, descriptions = description)
		for (i in 1:nrow(todo)){
			modifyDescription(x, todo$colnames[i], todo$descriptions[i])
		}
	} else {
		.colname <- colname	
		rm(colname)
		.description <- description
		rm(description)

		DESC <- attr(x, 'description')
		if (.colname %in% DESC[, colname]){
			DESC[.colname == colname, description := .description]
		} else {
			if (!(.description %in% colnames(x))){
				warning(.colname %&% ' not in cohort')				
			}
			DESC <- copy(rbind(DESC, data.table(colname = as.character(.colname),
				description = as.character(.description))))
		}
		setkey(DESC, colname)
		setattr(x, 'description', DESC)
	}
	invisible(attr(x, 'description'))
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
	setcolorder(x, c(attr(x, 'idcolname'),
		sort(setdiff(names(x), attr(x, 'idcolname')))))
	cat('\nDATA\n')
	data.table:::print.data.table(x)
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
			if (z %in% names(object)){
				class(object[[z]])[1]
			} else {
				'NULL'
			}
		}
		sapply(colnames, trygetclass)
	}

	cat(paste(attr(object, 'description')[,
		colname %&% ' (' %&% getclass(colname) %&% '): ' %&%
		description], collapse='\n'))
	cat('\n')
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
	
	out <- data.table:::subset.data.table(x, includeRow, select, ...)
	out <- as.cohort(out, idcolname = attr(x, 'idcolname'),
		description = attr(x, 'description')) 
	return(copy(out))
}

removeColumn <- function(x, colname){
	# Removes one or more columns from a cohort file and description
	for (col in colname){
		x[, col := NULL, with = FALSE]
	}
	DESC <- attr(x, 'description')
	DESC <- subset(DESC, colname %in% names(x))
	setattr(x, 'description', DESC)
	invisible(attr(x, 'description'))
} 

merge.cohort <- function(x, y, ...){
	# Merges two cohorts, warning if there are common columns
	# Combines the descriptions
	if (!(is.cohort(x) & is.cohort(y))){
		stop('x and y must be cohort objects')
	}

	if (attr(x, 'idcolname') != attr(y, 'idcolname')){
		y <- copy(y)
		if (attr(x, 'idcolname') %in% names(y)){
			stop('ID column names in x and y are different')
		} else {
			setnames(y, attr(y, 'idcolname'), attr(x, 'idcolname'))
		}
	}

	if (length(intersect(names(x), names(y))) > 0){
		warning('Columns ' %&%
			paste(intersect(names(x), names(y)), collapse=', ') %&%
			' are in both cohort datasets')
	}
	out <- merge(x, y, by = attr(x, 'idcolname'), ...)
	out <- cohort(out, idcolname = attr(x, 'idcolname'),
		description <- rbind(attr(x, 'description'), attr(y, 'description')))
	out	
}


