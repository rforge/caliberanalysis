
removeColumns <- function(x, colnames){
	# Removes one or more columns from a cohort file and description
	for (colname in colnames){
		if (is.data.table(x)){
			x[, (colname) := NULL]
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

removeVariables <- function(x, colnames){
	removeColumns(x = x, colnames = colnames)
} 
