transferVariables <- function(fromdata, todata, varnames,
	by = 'anonpatid', keep = TRUE, description = NULL){
	# A convenience function for transferring variables from one dataset
	# to another
	# Arguments: fromdata = a data.table from which to get the variables
	#                        only the first value per 'by' variable is used, or
	#                        specify an ordering function to choose which entry
	#                        to transfer, this might already be the case,
	#                        to check
	#            todata = a data.table to transfer variables to
	#            varnames = a vector of variable names to transfer
	#            by = name of key column for datasets
	#            keep = whether to keep the transferred columns in from
	#            description = character vector (one element per variable
	#                        to be transferred, to give a description of
	#                        transferred column in todata if it is a cohort)
	
	if (!is.data.table(fromdata)){
		stop('fromdata must be a data.table')
	}
	if (!is.data.table(todata)){
		stop('todata must be a data.table')
	}
	if (!is.null(description)){
		if (!is.cohort(todata)){
			warning('description is ignored because todata is not a cohort')
		}
		if (!is.character(description)){
			warning('description ignored; it should be a character vector')
			description <- NULL
		}
	}
	if (!is.null(description)){
		# recycle description
		description <- rep(description,
			length(varnames))[1:length(varnames)]
	} else {
		description <- rep('transferVariables ' %&%
			Sys.time(), length(varnames))
	}

	#### Set index ####
	if (is.null(key(todata))){
		changeKeyTo <- TRUE
		saveKeyTo <- NULL
		setkeyv(todata, by)		
	} else if (!identical(key(todata), by)){
		changeKeyTo <- TRUE
		saveKeyTo <- key(todata)
		setkeyv(todata, by)
	} else {
		changeKeyTo <- FALSE
	}
	
	for (varname in varnames){	
		if (varname %in% names(fromdata)){
			if (varname %in% names(todata)){
				warning(paste(varname,
					' in "to" data.table will be over-written', sep=''))
			}
			# Extract entries to transfer (first row per patient)
			totransfer <- eval(parse(text = paste('fromdata[!is.na(',
				varname, '), list(', varname, '=', varname, '[1]), by = ',
				by, ']', sep = '')))
			setkeyv(totransfer, by)
			# Backquote column names in case they start with a number
			todata[, eval(parse(text=paste('`', varname,
				'`:= totransfer[todata, `', varname,
				'`][, `', varname, '`]', sep = '')))]
			if (is.cohort(todata)){
				# add a description
				modifyDescription(todata, varname, description[varname])
			}
			if (!keep){
				if (is.cohort(fromdata)){
					removeColumn(fromdata, varname)
				} else {
					fromdata[, varname := NULL, with = FALSE]
				}
			}
		} else {
			warning(paste(varname,
				' not found in "fromdata" data.table', sep=''))
		}
	}
	
	#### Update description if necessary ####
	if (is.cohort(todata)){
		modifyDescription(todata, varnames, description)
	}
	if (is.cohort(fromdata) & keep == FALSE){
		# Remove from description in fromdata
		purgeDescription(fromdata)
	}
	
	#### Restore original index ####
	if (changeKeyTo){
		setkeyv(todata, saveKeyTo)
	}
	
	return(names(todata))
}
