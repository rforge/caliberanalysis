# Contains code for loading dictionaries from 
# spreadsheet and exporting to spreadsheet,
# also for testing the algorithm using the
# pre-converted texts, and identifying differences in
# interpretation between different version of the 
# algorithm, for checking.

loadDict <- function(dictionary, dictname){
	# Loads a dictionary from file or data
	if (is.null(dictionary)){
		assign(dictname, NULL)
		suppressWarnings(data(list = dictname, envir = environment()))
		if (exists(dictname)){
			if (!is.null(dictname)){
				cat('\nUsing', dictname, 'in data folder\n')
				dictionary <- get(dictname)
			} else {
				stop(paste('Unable to load', dictname, 'from data folder'))
			}
		}
	} else if (class(dictionary)[1] ==
		paste('drugdose_', dictname, sep = '')){
		# dictionary is already the correct dictionary
	} else if (length(dictionary) == 1) {
		cat('\nLoading', dictname, 'from', dictionary, '\n')
		temp <- NULL
		try(temp <- read.csv(dictionary, header = TRUE, as.is = TRUE))
		if (is.null(temp)){
			stop(paste('Unable to load', dictname, 'from', dictionary))
		}
	}

	# Ensure dictionary is in the correct format
	if (dictname == 'singlewords'){
		dictionary <- as.drugdose_singlewords(dictionary)
	} else if (dictname == 'multiwords'){
		dictionary <- as.drugdose_multiwords(dictionary)
	} else if (dictname == 'patterns'){
		dictionary <- as.drugdose_patterns(dictionary)
	} else if (dictname == 'lookups') {
		dictionary <- as.drugdose_lookups(dictionary)
	}
	dictionary
}

export <- function(x, ...){
	# S3 generic function (also provided by CALIBERcodelists package)
	UseMethod("export")
}

export.drugdose_singlewords <- function(x, filename){
	setattr(x, 'class', NULL)
	out <- as.data.frame(x)
	names(out) <- 'replacement'
	out$words <- row.names(out)
	out$replacement[out$words == out$replacement] <- NA
	write.csv(out, filename, row.names = FALSE)
}

export.drugdose_multiwords <- function(x, filename){
	out <- x
	# Write 'deleteme' for entries to be deleted
	out$replacement[out$replacement == ' '] <- ' deleteme '	
	write.csv(out, filename, row.names = FALSE)
}

export.drugdose_lookups <- function(x, filename){
	out <- x
	# Convert zero back into missing
	for (item in c('qty', 'freq', 'tot', 'priority',
		'time', 'duration')){
		out[, item][out[, item] == 0] <- NA
	}
	# Convert factors back to strings
	out$max <- as.character(out$max)
	out$max[out$max == 'exact'] <- ''
	out$change <- as.character(out$change)
	out$change[out$change == 'nochange'] <- ''
	out$choice <- as.character(out$choice)
	out$choice[out$choice == 'nochoice'] <- ''
	# Blank zero values
	out$priority[out$priority == 0] <- NA
	write.csv(out, filename, row.names = FALSE, na='')
}

export.drugdose_patterns <- function(x, filename){
	out <- x
	# Convert zero back into missing
	for (item in c('qty', 'freq', 'tot', 'priority',
		'time', 'duration')){
		out[, item][out[, item] == 0] <- NA
	}
	# Convert factors back to strings
	out$max <- as.character(out$max)
	out$max[out$max == 'exact'] <- ''
	out$change <- as.character(out$change)
	out$change[out$change == 'nochange'] <- ''
	out$choice <- as.character(out$choice)
	out$choice[out$choice == 'nochoice'] <- ''
	# Blank zero values
	out$priority[out$priority == 0] <- NA
	# Convert doubledose from Boolean to character
	temp <- ifelse(out$doubledose, 'double', '')
	out$doubledose <- temp
	write.csv(out, filename, row.names = FALSE, na = '')
}

as.drugdose_patterns <- function(patterns){
	if (class(patterns)[1] == 'drugdose_patterns'){
		# do nothing
		return(patterns)
	} else {
		if (is.data.table(patterns)){
			# convert it to data.frame
			setattr(patterns, 'class', 'data.frame')
		} else if (is.data.frame(patterns)){
			# do nothing
		} else if (is.character(patterns) & length(patterns) == 1){
			patterns <- read.csv(patterns, as.is = TRUE, row.names = NULL)		
		} else {
			stop('Patterns is not a data.frame or filepath.')
		}
	
		# Standardise names
		thenames <- colnames(patterns)
		thenames[thenames == 'dose_number'] <- 'qty'
		thenames[thenames == 'dose_unit'] <- 'units'
		thenames[thenames == 'dose_frequency'] <- 'freq'
		thenames[thenames == 'dose_inteval'] <- 'time'
		thenames[thenames == 'choice_of_dose'] <- 'choice'
		thenames[thenames == 'dose_max_average'] <- 'max'
		thenames[thenames == 'change_dose'] <- 'change'
		thenames[thenames == 'dose_duration'] <- 'duration'
		colnames(patterns) <- thenames
		
		# Convert missing into blank
		for (item in c('qty', 'freq', 'tot', 'time', 'duration')){
			patterns[, item][is.na(patterns[, item])] <- ''
		}
		
		# Convert words to lower case
		patterns$units <- tolower(patterns$units)
		patterns$words <- paste('', trim(patterns$words), '')
			
		# Convert category variables to factors
		patterns$max[is.na(patterns$max)] <- 'exact'
		patterns$max[patterns$max==''] <- 'exact'
		patterns$max <- factor(patterns$max, c('max', 'average', 'exact'))
		
		patterns$change[is.na(patterns$change)] <- 'nochange'
		patterns$change[patterns$change==''] <- 'nochange'
		patterns$change <- factor(patterns$change,
			c('first', 'second', 'nochange'))
		
		patterns$choice[is.na(patterns$choice)] <- 'nochoice'
		patterns$choice[patterns$choice==''] <- 'nochoice'
		patterns$choice <- factor(patterns$choice,
			c('choice', 'asneeded', 'nochoice'))
		
		# Doubledose Boolean variable
		patterns$doubledose <- (patterns$doubledose=='double')
		
		# Zero for blank priority
		patterns$priority[is.na(patterns$priority)] <- 0
		
		# For patterns, order the dictionary, and replace the
		# row numbers by integers
		patterns <- patterns[order(patterns$category, patterns$order),]
		patterns$order <- 1:nrow(patterns)
	
		setattr(patterns, 'class', c('drugdose_patterns', 'data.frame'))	
		return(patterns)
	}
}

as.drugdose_lookups <- function(lookups){
	if (class(lookups)[1] == 'drugdose_lookups'){
		# do nothing
		return(lookups)
	} else {
		if (is.data.table(lookups)){
			# convert it to data.frame
			setattr(lookups, 'class', 'data.frame')
		} else if (is.data.frame(lookups)){
			# do nothing
		} else if (is.character(lookups) & length(lookups) == 1){
			lookups <- read.csv(lookups, as.is = TRUE, row.names = NULL)		
		} else {
			stop('Lookups is not a data.frame or filepath.')
		}
	
		# Standardise names
		thenames <- colnames(lookups)
		thenames[thenames == 'dose_number'] <- 'qty'
		thenames[thenames == 'dose_unit'] <- 'units'
		thenames[thenames == 'dose_frequency'] <- 'freq'
		thenames[thenames == 'dose_inteval'] <- 'time'
		thenames[thenames == 'choice_of_dose'] <- 'choice'
		thenames[thenames == 'dose_max_average'] <- 'max'
		thenames[thenames == 'change_dose'] <- 'change'
		thenames[thenames == 'dose_duration'] <- 'duration'
		colnames(lookups) <- thenames
		
		# Convert missing into blank
		for (item in c('qty', 'freq', 'tot', 'time', 'duration')){
			lookups[, item][is.na(lookups[, item])] <- ''
		}
		
		# Convert words to lower case
		lookups$words <- tolower(trim(lookups$words))
		lookups$units <- tolower(lookups$units)
			
		# Convert category variables to factors
		lookups$max[is.na(lookups$max)] <- 'exact'
		lookups$max[lookups$max==''] <- 'exact'
		lookups$max <- factor(lookups$max, c('max', 'average', 'exact'))
		
		lookups$change[is.na(lookups$change)] <- 'nochange'
		lookups$change[lookups$change==''] <- 'nochange'
		lookups$change <- factor(lookups$change,
			c('first', 'second', 'nochange'))
		
		lookups$choice[is.na(lookups$choice)] <- 'nochoice'
		lookups$choice[lookups$choice==''] <- 'nochoice'
		lookups$choice <- factor(lookups$choice,
			c('choice', 'asneeded', 'nochoice'))
		
		# In the output and lookups tables, order is the order of
		# output rows if the output is more than one row
		# Default is simplify = TRUE, i.e. one output row.
		
		lookups <- as.data.table(lookups)
		colnames_lookups <- c('words', 'qty', 'units', 'freq', 'tot',
			'max', 'time', 'change', 'choice', 'duration', 'daily_dose')
		
		if (all(colnames_lookups %in% colnames(lookups))){
			lookups <- lookups[, colnames_lookups, with = FALSE]
			setcolorder(lookups, colnames_lookups)
			lookups[, order := as.integer(1L)]
			
			# Convert numbers to numbers
			lookups[, qty := as.numeric(qty)]
			lookups[, freq := as.numeric(freq)]
			lookups[, tot := as.numeric(tot)]
			lookups[, time := as.numeric(time)]
			lookups[, daily_dose := as.numeric(daily_dose)]
			
			setkey(lookups, words)
		} else {
			warning(paste('Lookups ignored because table in incorrect format.\n',
				'It should have columns:', paste(colnames_lookups, collapse = ', ')))
		}
		
		setattr(lookups, 'class', c('drugdose_lookups', 'data.table',
			'data.frame'))
		return(lookups)
	}
}

as.drugdose_multiwords <- function(multiwords){
	if (class(multiwords)[1] == 'drugdose_multiwords'){
		# do nothing
		return(multiwords)
	} else {
		if (is.data.table(multiwords)){
			# convert it to data.frame
			setattr(multiwords, 'class', 'data.frame')
		} else if (is.data.frame(multiwords)){
			# do nothing
		} else if (is.character(multiwords) & length(multiwords) == 1){
			multiwords <- read.csv(multiwords, as.is = TRUE, row.names = NULL)		
		} else {
			stop('Multiwords is not a data.frame or filepath.')
		}
		
		# Order the dictionary, and replace the 'order' column
		# special formatting for multiwords (to detect words with)
		# spaces either side of words, to detect separate wordspace
		multiwords$words <- paste('', trim(tolower(multiwords$words)), '')
		multiwords$replacement <- paste('',
			trim(tolower(multiwords$replacement)), '')
		multiwords$replacement[multiwords$replacement == ' deleteme '] <- ' '
		# Order the entries
		multiwords <- multiwords[order(multiwords$order),]
		multiwords$order <- 1:nrow(multiwords)
		
		setattr(multiwords, 'class', c('drugdose_multiwords', 'data.frame'))	
		return(multiwords)
	}
}


as.drugdose_singlewords <- function(singlewords){
	if (class(singlewords)[1] == 'drugdose_singlewords'){
		# do nothing
		return(singlewords)
	} else {
		if (is.data.table(singlewords)){
			# convert it to data.frame
			setattr(singlewords, 'class', 'data.frame')
		} else if (is.data.frame(singlewords)){
			# do nothing
		} else if (is.character(singlewords) & length(singlewords) == 1){
			singlewords <- read.csv(singlewords, as.is = TRUE, row.names = NULL)		
		} else {
			stop('Singlewords is not a data.frame or filepath.')
		}
	
		wordlist <- tolower(singlewords$replacement)
		wordlist[is.na(wordlist)] <-
			tolower(singlewords$words[is.na(wordlist)])
		wordlist[wordlist==''] <-
			tolower(singlewords$words[wordlist==''])
		thenames <- tolower(singlewords$words)
		if (any(duplicated(thenames))){
			cat('\nThere are duplicate words; singlewords is invalid.')
			cat('\nDuplicates:', paste(thenames[duplicated(thenames)],
				collapse = ' '))
		}
		names(wordlist) <- thenames
		if (!is.null(singlewords$comments)) {
			attr(wordlist, 'comments') <- singlewords$comments
		}
		
		setattr(wordlist, 'class', 'drugdose_singlewords')
		return(wordlist)
	}
}

# Addterm functions S3 generic
addterm <- function(x, ...){
	# S3 generic
	UseMethod("addterm")
}

addterm.drugdose_singlewords <- function(x, ...){
	addtermSinglewords(x, ...)
}

addterm.drugdose_multiwords <- function(x, ...){
	addtermMultiwords(x, ...)
}

addterm.drugdose_lookups <- function(x, words = '',
	qty = NA, units = '', freq = NA, tot = NA, max = 'exact',
	time = NA,	change = 'nochange', choice = 'nochoice',
	duration = NA, daily_dose){
	
	max <- factor(max, c('max', 'average', 'exact'))
	change <- factor(change, c('first', 'second', 'nochange'))
	choice <- factor(choice, c('choice', 'asneeded', 'nochoice'))
	
	toadd <- data.table(words = tolower(words),
		qty = as.numeric(qty), units = tolower(units), 
		freq = as.numeric(freq), tot = as.numeric(tot),
		max = max, time = as.numeric(time),
		change = change, choice = choice,
		duration = as.numeric(duration),
		daily_dose = as.numeric(daily_dose))

	new <- rbind(toadd, x)
	setkey(new, words)
	invisible(new)
}

addterm.drugdose_patterns <- function(x, order = NA, words = '',
	qty = NA, units = '', freq = NA, tot = NA, priority = 0, max = 'exact',
	time = NA,	change = 'nochange', choice = 'nochoice',
	doubledose = FALSE, duration = NA, category = '', comment = ''){
	
	patterns <- x
	
	if (is.na(order)){
		order <- nrow(patterns) + 1
	}

	max <- factor(max, c('max', 'average', 'exact'))
	change <- factor(change, c('first', 'second', 'nochange'))
	choice <- factor(choice, c('choice', 'asneeded', 'nochoice'))
	
	toadd <- data.frame(words = words, qty = qty, units = units, 
		freq = freq, tot = tot, priority = priority, max = max, time = time,
		change = change, choice = choice, doubledose = doubledose,
		duration = duration, category = category, comment = comment,
		order = order)

	# Find the correct position
	before <- (patterns$order < order)
	if (sum(before) == 0){
		# new order is smaller than all the values in the actual 
		# dictionary
		new <- rbind(toadd, patterns)
	} else {
		beforepos <- max(which(before))
		if (beforepos > nrow(patterns)) {
			new <- rbind(patterns, toadd)
		} else {
			new <- rbind(patterns[1:beforepos,],
				toadd, patterns[(beforepos + 1):nrow(patterns),])
		}
	}
	invisible(new)
}

addtermMultiwords <- function(multiwords, order = NA, words = '',
	replacement = '', comment = ''){
	if (is.na(order)){
		order <- nrow(multiwords) + 1
	}
	toadd <- data.frame(words = words, replacement = replacement,
		comment = comment, order = order)
	# Find the correct position
	before <- (multiwords$order < order)
	if (sum(before) == 0){
		# new order is smaller than all the values in the actual 
		# dictionary
		new <- rbind(toadd, multiwords)
	} else {
		beforepos <- max(which(before))
		if (beforepos > nrow(multiwords)) {
			new <- rbind(multiwords, toadd)
		} else {
			new <- rbind(multiwords[1:beforepos,],
				toadd, multiwords[(beforepos + 1):nrow(multiwords),])
		}
	}
	invisible(new)
}

addtermSinglewords <- function(singlewords,
	words = '', replacement = ''){
	# Check that the word does not exists; if it does it will be 
	# over-written
	if (words %in% names(singlewords)){
		new <- singlewords
		singlewords[words] <- replacement
	} else {
		new <- c(singlewords, replacement)
		names(new) <- c(names(singlewords), words)
	}
	invisible(new)
}

