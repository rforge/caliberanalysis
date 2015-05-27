importCSVcodelist <- function(filename, as.is=TRUE, ...){
	# Imports a CSV codelist, automatically trying different delimiters
	# if comma is incorrect. This function should only be used for importing
	# codelists
	temp <- data.table(read.csv(filename, as.is=as.is, ...))
	
	if (ncol(temp)==1){
		# Only detected one column, try TAB delimiter
		try(temp <- data.table(read.delim(filename,
			sep='\t', as.is=as.is, ...)))
	}
	if (ncol(temp)==1){
		# Only detected one column, try semicolon delimiter
		try(temp <- data.table(read.delim(filename,
			sep=' ', as.is=as.is, ...)))
	}
	if (ncol(temp)==1){
		# Only detected one column, try space delimiter
		try(temp <- data.table(read.delim(filename,
			sep=' ', as.is=as.is, ...)))
	}

	if (ncol(temp)==1){
		stop('Unable to load codelist, what I got was:\n' %&%
			paste(show(temp), collapse='\n'))
	}
	return(temp)
}
