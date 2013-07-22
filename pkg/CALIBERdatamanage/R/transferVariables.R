transferVariables <- function(fromdata, todata, varnames,
	by = NULL, description = NULL){
	# A convenience function for transferring variables from one dataset
	# to another
	# Arguments: fromdata = a data.table or ffdf from which to get the variables.
	#                        only the first value per 'by' variable is used.
	#            todata = a data.table or ffdf to transfer variables
	#            varnames = a vector of variable names to transfer
	#            by = name of key column for datasets
	#            description = character vector (one element per variable
	#                        to be transferred, to give a description of
	#                        transferred column in todata if it is a cohort)

	#### Ensure description is valid ####	
	if (!is.null(description)){
		if (!is.cohort(todata)){
			warning('description is ignored because todata is not a cohort')
		}
		if (!is.character(description)){
			warning('description ignored; it should be a character vector')
			description <- NULL
		}
	}
	if (is.null(description)){
		thecall <- match.call()
		description <- paste(
			gsub('\n|\t| +', ' ', capture.output(print(thecall))), collapse = ' ')
	}

	# Recycle description if multiple columns are transferred
	description <- rep(description,
		length(varnames))[1:length(varnames)]

	#### Key column ####
	if (is.null(by)){
		if (is.cohort(todata) & is.cohort(fromdata)){
			if (identical(attr(todata, 'idcolname'),
				attr(fromdata, 'idcolname'))){
				by = attr(todata, 'idcolname')	
			} else {
				stop('fromdata and todata have different idcolname')
			}
		} else if (is.cohort(todata)){
			by = attr(todata, 'idcolname')
		} else if (is.cohort(fromdata)){
			by = attr(fromdata, 'idcolname')
		}
	}

	#### Create a vector of indices to transfer     ####   
	#### variables directly from fromdata to todata ####
	TEMP <- data.table(by = as.ram(fromdata[[by]]), order = 1:nrow(fromdata))
	# We only use the first entry per 'by' variable
	TEMP[, use := c(TRUE, rep(FALSE, .N - 1)), by = by]	

	if (any(!TEMP$use)){
		TEMP <- subset(TEMP, use == TRUE)
	}
	
	setkey(TEMP, by)
	tempindex <- as.ram(todata[[by]])
	myindexes <- TEMP[J(tempindex)][, order]

	#### Now do the transfers ####
	for (varname in varnames){	
		if (varname %in% colnames(fromdata)){
			if (varname %in% colnames(todata)){
				warning(paste(varname,
					' in "to" data.table will be over-written', sep=''))
			}

			# Create the variable to transfer (as a vector)
			if (is.ffdf(fromdata)){
				# Can't index nothing using NA because hybrid indexes don't permit NA.
				# Instead use a two-stage process. Index the non-missings and transfer
				# them specifically, leaving the remainder as NA.
				out <- as.ram(fromdata[[varname]])[
					myindexes[!is.na(myindexes)]]
			} else if (is.data.table(fromdata)){
				if (is.character(fromdata[[varname]])){
					# Non-missing entries
					out <- as.factor(fromdata[[varname]])[
						myindexes[!is.na(myindexes)]]
				} else {
					out <- fromdata[[varname]][
						myindexes[!is.na(myindexes)]]
				}
			}

			# Inserting missing entries
			if (length(out) == 0){
				out2 <- rep(NA, nrow(todata))
			} else {
				# Create an out2 vector of the correct data type
				out2 <- rep(out[1], nrow(todata))
				# Make it all missing
				is.na(out2) <- 1:nrow(todata)
				# Add the non-missing values
				out2[!is.na(myindexes)] <- out
			}

			if (is.data.table(todata)){
				todata[, eval(parse(text = paste('`', varname,
					'`:= out2', sep = '')))]
			} else if (is.ffdf(todata)){
				todata[[varname]] <- as.ff(out2)
			}

			if (is.cohort(todata)){
				# Add a description
				modifyDescription(todata, varname, description[varname])
			}
		} else {
			warning(paste(varname,
				' not found in "fromdata" data.table', sep=''))
		}
	}

	invisible(todata)
}
