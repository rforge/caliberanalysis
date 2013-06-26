textToDate <- function(x,
	missingformat = c('0000-00-00', '0000-00-00 00:00:00', '', 'NA')){
	# Automatically detects date format and converts to Date
	# Also deals with null or missing dates, and converts them
	# to missing output
	# x is a character vector or factor

	if ('IDate' %in% class(x)){
		# already IDate
		return(x)
	} else if ('Date' %in% class(x)){
		# Convert to integer date, to save space
		return(as.IDate(x))
	} else if (is.numeric(x) & !is.factor(x)){
		# Can't convert numbers
		return(x)
	} else {
		datestring <- as.character(x)
	
		doconvertdate <- function(datestring){
			# Look at the first date and try potential formats
			formats <- c('%d/%m/%Y', '%d/%m/%y', '%d/%m/%Y %H:%M:%S',
				'%d-%m-%Y', '%d-%m-%y', '%d.%m.%Y', '%d.%m.%y',
				'%d-%m-%Y %H:%M:%S',
				'%Y-%m-%d', '%Y-%m-%d %H:%M:%S',
				'%d %b %Y', '%d %b %y', '%d-%b-%Y', '%m/%d/%Y')
		
			# maxnum = number of dates to try; up to 100 or the total number available
			if (length(datestring) < 100){
				maxnum <- length(datestring)
			} else {
				maxnum <- 100
			}
		
			found <- FALSE
			trynumber <- 0
			while(found == FALSE & trynumber <= length(formats) ){
				trynumber <- trynumber + 1
				# Try to convert the first maxnum dates
				trial <- as.IDate(datestring[1:maxnum], formats[trynumber])
				if (sum(is.na(trial)) == 0){
					found <- TRUE # if no missing values, assume all converted
				}
			}
		
			if (found == FALSE){
				warning('Unable to detect date format')
				datestring
			}	else { 
				if ('parallel' %in% loadedNamespaces()) {
					pvec(datestring, as.IDate, format = formats[trynumber])
				} else {
					as.IDate(datestring, format = formats[trynumber])
				}
			}
		}
	
		relevant <- ifelse(is.na(datestring), FALSE,
			ifelse(datestring %in% missingformat, FALSE, TRUE))
		# If there is no missing data, convert the entire lot
		if (all(relevant)){
			out <- NULL
			try(out <- doconvertdate(datestring))
		} else if (!any(relevant)){
			# all missing, return missing dates
			out <- as.IDate(rep(NA, length(datestring)))
		} else {
			# select relevant indices for conversion
			# 'relevant' is a logical vector stating which dates can be converted
			conversion <- NULL
			try(conversion <- doconvertdate(datestring[relevant]))
			if (is.null(conversion) | is.character(conversion)){
				out <- NULL
			} else {
				out <- as.IDate(rep(NA, length(datestring)))
				out[relevant] <- conversion
			}
		}

		if (is.null(out) | is.character(out)){
			# unsuccessful conversion, return the original (may be a factor)
			return(x)
		} else {
			# successful
			return(out)
		}
	}
}

