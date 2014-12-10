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
	if (is.null(attr(x, 'idcolname'))){
		idcolname <- idcolname[which(idcolname %in% colnames(out))]
	} else {
		idcolname <- as.character(attr(x, 'idcolname'))
	}
	names(idcolname) <- NULL
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
		if (!is.null(attr(x, 'var.labels'))){
			description <- data.frame(colname = colnames(x),
				description = attr(x, 'var.labels'),
				stringsAsFactors = FALSE)
		}
	}

	if (is.null(description)){
		description <- attr(x, 'description')
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
	description <- description[description$colname %in% colnames(x), ]
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

as.cohort <- function(x, ...){
	if (is.cohort(x)){
		x
	} else {
		cohort(x, ...)
	}
}

is.cohort <- function(x){
	if ('cohort' %in% class(x)){
		if (is.null(attr(x, 'idcolname'))){
			FALSE
		} else {
			TRUE
		}
	} else {
		FALSE
	}
}

print.cohort <- function(x, ...){
	# Prints the summary and then the cohort file itself
	summary.cohort(x)	

	cat('\nDATA\n')
	if (is.data.table(x)){
		# Changing the class to data.table in order to invoke
		# print.data.table
		classes <- class(x)
		setattr(x, 'class', c('data.table', 'data.frame'))
		print(x)
		# Restore original classes
		setattr(x, 'class', classes)
	} else if (is.ffdf(x)){
		ff::print.ffdf(x)
	}
}

summary.cohort <- function(object, ...){
	# Prints a summary of a cohort
	if (is.data.table(object)){
		# Order the columns so that ID column is at the
		# front and others are alphabetical
		setcolorder(object, c(attr(object, 'idcolname'),
			sort(setdiff(names(object), attr(object, 'idcolname')))))
		cat('Data.table cohort with ') 
	} else if (is.ffdf(object)) {
		cat('FFDF cohort with ')
	}
	cat(nrow(object), 'patients and ' %&% ncol(object) %&%
		' columns; ID column =', attr(object, 'idcolname'), '\n')

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
	
	if (!is.null(description)){
		if (nrow(description) > 0){
			cat('\nCOLUMN DESCRIPTIONS\n')
			# Add columns without a description
			description <- merge(description,
				data.frame(colname = colnames(object)), by = 'colname',
				all = TRUE)
			description$description[is.na(description$description)] <- ''
			
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
	}
}

truncateChar <- function(x, maxchar){
	# Truncates a character vector so that each element does not have more
	# than a specified number of characters, adding ... to the end of 
	# truncated terms
	# Arguments: x - character string to truncate
	#            maxchar - length to truncate to
	
	# Ensure maxchar is a vector of the same length as x
	maxchar <- maxchar + rep(0, length(x))

	# Split into individual lines
	for (i in 1:length(x)){
		xlines <- strsplit(x[i], '\n')[[1]]
		if (length(xlines) > 1){
			nindent <- min(nchar(strsplit(xlines[1], ':')[[1]][1]) + 2,
				getOption('width') - 20)
			xlines[2:length(xlines)] <- paste(rep(' ', nindent),
				collapse = '') %&% xlines[2:length(xlines)]
		}
		convert <- nchar(xlines) > maxchar[i]
		xlines[convert] <- substr(xlines[convert], 1, maxchar[i] - 3) %&% '...'
		x[i] <- paste(xlines, collapse = '\n')[1]
	}
	x
}

subset.cohort <- function(x, subset, select){
	# S3 method for subsetting a cohort
	# Arguments: x - cohort
	#            subset - a logical expression for rows to keep,
	#                   default is to keep all rows.
	#            select - a character vector of columns to keep,
	#                   default is to keep all columns.
	#                   The ID column is always kept.
	
	# Ensure that as a minimum, code, medcode if GPRD,
	# term and category are kept. It is only valid as a codelist
	# if these columns are kept test
	
	if (missing(select)){
		select <- colnames(x)
	} else {
		select <- c(attr(x, 'idcolname'),
			sort(unique(setdiff(select, attr(x, 'idcolname')))))
	}
	
	if (is.data.table(x)){
		if (missing(subset)) {
			includeRow <- TRUE
		}	else {
			expr <- substitute(subset)
			includeRow <- eval(expr, x, parent.frame())
			if (!is.logical(includeRow)) 
				stop("'subset' must evaluate to a Boolean vector")
			includeRow[is.na(includeRow)] <- FALSE
		}
		out <- x[includeRow, select, with = FALSE]
	} else if (is.ffdf(x)){
		# subset.ffdf does not use ...
		out <- ffbase::subset.ffdf(x, subset)
		# If using the select argument, need to select the relevant
		# vectors by setting the others to NULL
		if (!missing(select)){
			if (is.logical(select) | is.numeric(select)){
				select <- colnames(out)[select]
			} 
			if (is.character(select) & length(select) > 0){
				# Remove unwanted columns
				remove <- setdiff(colnames(out), select)
				for (thecol in remove){
					out[[thecol]] <- NULL
				}
			}
		}
	}
	out <- cohort(out, idcolname = attr(x, 'idcolname'),
		description = attr(x, 'description')) 
	out <- purgeDescription(out)
	out
}


merge.cohort <- function(x, y, by = attr(x, 'idcolname'), ...){
	# Merges two cohorts, warning if there are common columns
	# The ID column name must be identical
	# Combines the descriptions
	if (!(is.cohort(x) & is.cohort(y))){
		stop('x and y must be cohort objects')
	}
	
	# If one of the objects is a data.table, both must be data.tables
	if (is.data.table(x) & !is.data.table(y)){
		y <- as.data.table(y)
	}
	if (is.data.table(y) & !is.data.table(x)){
		x <- as.data.table(x)
	}
	if (attr(x, 'idcolname') != attr(y, 'idcolname')){
		stop('ID column names in x and y are different')
	}

	if (length(intersect(colnames(x), colnames(y))) > 1){
		warning('Columns ' %&%
			paste(intersect(colnames(x), colnames(y)), collapse=', ') %&%
			' are in both cohort datasets')
	}

	classx <- class(x)
	classy <- class(y)

	# Simplify the classes to select the correct merge method
	if (is.ffdf(x) & is.ffdf(y)){
		setattr(x, 'class', 'ffdf')
		setattr(y, 'class', 'ffdf')
	}
	if (is.data.table(x) & is.data.table(y)) {
		setattr(x, 'class', c('data.table', 'data.frame'))
		setattr(y, 'class', c('data.table', 'data.frame'))
	}
	
	# Perform the merge
	out <- merge(x, y, by = by, ...)
	
	# Restore original classes
	setattr(x, 'class', classx)
	setattr(y, 'class', classy)
	
	out <- cohort(out, idcolname = attr(x, 'idcolname'),
		description <- rbind(attr(x, 'description'), attr(y, 'description')))
	out	
}

