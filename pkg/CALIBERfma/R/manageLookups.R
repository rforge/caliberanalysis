# Loads lookup tables into memory, manipulates them
# and saves them.

exportLookup <- function(lookup, filename){
	# Export with Windows line endings, tab delimited with no quotes	
	write.table(lookup, filename, sep='\t', eol='\r\n', quote=FALSE,
		row.names=FALSE, col.names=ifelse(ncol(lookup)==1, FALSE, TRUE))
}

addtoLookup <- function(lookup, valuelist){
	# Arguments: lookup is a data.table containing the lookup table
	#            valuelist is a named list, data.table or data.frame
	#                     containing the non-missing
	#                     elements of the row to be added
	# Returns the data.table with an extra row
	
	valuelist <- as.data.table(valuelist)
	# Add any spare columns
	for (thevar in setdiff(names(lookup), names(valuelist))){
		x <- lookup[1][, thevar, with=FALSE]
		if (is.character(x)){
			x <- ''
		} else {
			is.na(x) <- 1
		}
		valuelist[, thevar:=x, with=FALSE]
	}
	
	# Bind together the existing table and the extra row
	rbind(lookup, valuelist, use.names=TRUE)
}

importLookup <- function(filename,
	use_CALIBERfma_dir_R = FALSE){
	# Imports a lookup from a filename

	if (use_CALIBERfma_dir_R){
		data.table(read.delim(
			paste(getOption('CALIBERfma_dir_R'), filename, sep=''),
			sep='\t', as.is=TRUE, row.names=NULL, header=TRUE))
	} else {
		data.table(read.delim(filename,
			sep='\t', as.is=TRUE, row.names=NULL, header=TRUE))
	}
}
