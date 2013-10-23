setnames.cohort <- function(x, old, new){
	# Ensures that the idcolname attribute is updated
	# and the description table is updated
	# Arguments: x = a cohort object
	#            old = old column names or positions, or a
	#	                 vector of all new column names if new is missing.
	#            new = new column names

	currentnames <- copy(colnames(x))
	if (is.data.table(x)){
		x <- setnames.data.table(x, old, new)
	} else if (is.ffdf(x)){
		x <- setnames.ffdf(x, old, new)
	}
	newnames <- copy(colnames(x))	
	
	# Update description table
	DESC <- attr(x, 'description')
	names(newnames) <- currentnames
	DESC$colname <- newnames[DESC$colname]
	# If there are any entries for non-existent columns, delete them.
	DESC <- DESC[!is.na(DESC$colname), ]
	setattr(x, 'description', DESC)
	
	# Update idcolname
	oldidcolname <- attr(x, 'idcolname')
	setattr(x, 'idcolname', newnames[oldidcolname])

	invisible(x)
}

setnames.ffdf <- function(x, old, new){
	# A version of setnames which works on ffdf data.frames
	# Arguments: x = a ffdf object
	#            old = old column names or positions, or a
	#	                 vector of all new column names if new is missing.
	#            new = new column names

	currentnames <- copy(colnames(x))
	if (missing(new)){
		# 'old' 
		if (length(new) != length(currentnames)){
			stop('Incorrect number of columns')
		}
		newnames <- old
		old <- currentnames
	} else {
		newnames <- currentnames
		names(newnames) <- currentnames
		# Which names have changed
		newnames[old] <- new
	}
	colnames(x) <- newnames
	invisible(x)
}

setnames.data.table <- function(x, old, new){
	# Calls the setnames function in data.table but
	# does not permit duplicate column names
	if (missing(new)){
		if (length(unique(old)) < length(old)){
			stop('Duplicate column names')
		}
	} else {
		# Check if any new names are in the unchanged names
		if (is.character(old)){
			remain <- setdiff(colnames(x), old)
		} else {
			remain <- setdiff(colnames(x), colnames(x)[old])
		}
		if (any(new %in% remain)){
			stop(paste('Column names',
				paste(new[new %in% remain], collapse =', '),
				'are duplicated'))
		}
	}
	data.table::setnames(x, old, new)
}

setnames <- function(x, old, new){
	# A S3 generic function 
	# Arguments: x = a cohort, ffdf or data.table object
	#            old = old column names or positions, or a
	#	                 vector of all new column names if new is missing.
	#            new = new column names
	UseMethod("setnames")
}

