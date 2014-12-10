
purgeDescription <- function(x){
	# Purge the description attribute
	description <- attr(x, 'description')
	description <- description[description$colname %in% colnames(x), ]
	setattr(x, 'description', description)
	invisible(x)
}

modifyDescription <- function(x, colname, description){
	# Modify the description attribute of a dataset
	# Arguments: x = a matrix, data.frame or data.table
	#            colname = vector of column names
	#            description = vector of descriptions

	if (length(colname) != length(description)){
		stop('colname and description must be the same length')
	}
	for (i in seq_along(colname)){
		thecolname <- colname[i]	
		thedescription <- description[i]

		DESC <- attr(x, 'description')
		if (is.null(DESC)){
			DESC <- data.frame(colname = colnames(x),
				description = '', stringsAsFactors = FALSE)
		}
		if (!identical(colnames(DESC), c('colname', 'description'))){
			warning('Purging description as it is not in the correct format')
			DESC <- data.frame(colname = colnames(x),
				description = '', stringsAsFactors = FALSE)
		}

		if (thecolname %in% DESC$colname){
			DESC[thecolname == DESC$colname,
				'description'] <- thedescription
		} else {
			if (!(thecolname %in% colnames(x))){
				warning(thecolname %&% ' not a column in x')				
			}
			DESC <- copy(rbind(DESC, data.frame(
				colname = as.character(thecolname),
				description = as.character(thedescription),
				stringsAsFactors = FALSE)))
		}
		DESC <- DESC[order(DESC$colname), ]
		setattr(x, 'description', DESC)
	}
	invisible(x)
}
