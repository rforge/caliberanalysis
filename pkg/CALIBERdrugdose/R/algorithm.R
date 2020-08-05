# Code for running the dose conversion algorithm
# Incorporates the VB 'partdata' functions.
# Instead of partdata, the dosage string is stored in
# an R character vector.

doseconvert <- function(text, textid = seq_along(text), dosage_mg = NULL,
	simplify = TRUE, singlewords = NULL, multiwords = NULL,
	patterns = NULL, maxifchoice = TRUE, usebuiltinlookups = TRUE,
	customlookups = NULL, cores = 1, noisy = FALSE){
	# converts a set of dosages. dosestrings must be a character vector
	# and ids should be a vector of unique IDs.
	# Cannot use this in noisy mode.
	# Arguments:
	#    text = vector of dosestrings to analyse
	#    textid = vector of identifiers for dosestrings
	#    dosage_mg = vector of mg strength for dosage units. This is used
	#        to calculate an additional column in the output 'daily_mg'
	#        which is the daily dose in mg. If NULL, the 'daily_mg'
	#        column is omitted from the output.
	#    simplify = whether to return only a single row per dosestring
	#    patterns = patterns lookup, loaded from data or imported from
	#        parent / global environment if not available
	#    multiwords = multiwords lookup, loaded from data or imported from
	#        parent / global environment if not available
	#    singlewords = singlewords lookup, loaded from data or imported from
	#        parent / global environment if not available
	#    lookups = lookup table for common dosages to avoid the need to
	#        interpret
	#    maxifchoice = whether to report the maximum if choice of dose,
	#        otherwise report the average
	#    usebuiltinlookups = whether to use the built-in table of common
	#        interpreted dosages that can be looked up rather than
	#        re-interpreted
	#    cores = whether to use multiple cores (using parallel)
	#    customlookups = a table of custom lookups
	#    noisy = whether to print debug information
	# For testing:
	# textid = NULL; simplify = TRUE; singlewords = NULL; multiwords = NULL;
	#    patterns = NULL; uselookups = TRUE; lookups = NULL;
	#    customlookups = NULL; cores = 1

	lookups <- NULL
	if (!is.null(customlookups)){
		custom <- NULL
		try(custom <- as.drugdose_lookups(customlookups))
		if (is.null(custom)){
			warning('Custom lookup table ignored as it is in incorrect format.')
			if (usebuiltinlookups){
				# Load lookups
				lookups <- loadDict(lookups, 'lookups')
			} 
		} else {
			if (usebuiltinlookups){
				# Load lookups and append customlookups
				lookups <- loadDict(lookups, 'lookups')
				lookups <- rbind(lookups, custom)
			} else {
				# Only custom lookups are available
				lookups <- custom
			}
			setkey(lookups, text)
		}
	} else {
		if (usebuiltinlookups){
			lookups <- loadDict(lookups, 'lookups')	
		}
	}
	
	# If no lookups table is in use, lookups remains NULL
	singlewords <- loadDict(singlewords, 'singlewords')
	multiwords <- loadDict(multiwords, 'multiwords')
	patterns <- loadDict(patterns, 'patterns')

	# Set up for multiprocessor analysis
	if (cores > 1 & 'parallel' %in% loadedNamespaces() &
		Sys.info()['sysname'] == 'Linux'){
		options(mc.cores = cores)
		multicore = TRUE
	} else {
		multicore = FALSE
	}
	
	indata <- data.table(dosestrings = as.character(text), textid = textid)
	# Store the original order of the input data so that the output can 
	# match it
	indata[, originalorder := 1:.N]
	# doseid is the ID for a unique dose string
	indata[, doseid := .GRP, by = dosestrings]
	# Generate a unique list of dosestrings to make analysis faster
	uniquedoses <- indata[, list(doseid = doseid[1]),
		by = dosestrings]

	analysis <- function(text){
		x <- trim(tolower(text))
		if (!is.null(lookups)){
			result <- lookups[x, .(qty, units, freq, tot, max, time,
				change, choice, duration, daily_dose), nomatch = 0]
			result[, order := rep(1L, nrow(result))]
		} else {
			#  Zero-row result if no lookup
			result <- data.frame(qty = numeric(0), units = character(0),
				freq = numeric(0), tot = numeric(0),
				max = factor(integer(0), c("max", "average", "exact")),
				time = numeric(0), change = factor(integer(0),
				c("first", "second", "nochange", "combined")),
				choice = factor(integer(0), c("choice", "asneeded",
				"nochoice")), duration = numeric(0), order = integer(0),
				daily_dose = numeric(0))
		}
		if (nrow(result) == 0){
			# wrap in 'try' to avoid stalling if there is an unexpected error
			try(result <- interpret(x, singlewords = singlewords,
				multiwords = multiwords, patterns = patterns,
				simplify = simplify, maxifchoice = maxifchoice,
				noisy = noisy))
		}
		result
	}

	# Analyse the dose strings
	if (multicore){
		results <- mclapply(uniquedoses$dosestrings, analysis)
	} else {
		results <- lapply(uniquedoses$dosestrings, analysis)
	}

	# Combine the results
	results <- lapply(seq_along(results), function(x){
		cbind(results[[x]], data.frame(doseid =
			rep(uniquedoses[x, doseid], nrow(results[[x]]))))
	})
	results <- as.data.table(do.call('rbind', results))
	setkey(indata, doseid)
	setkey(results, doseid)
	results <- results[indata]
		
	# If the original textid were numeric, make them numeric in the output
	if (is.numeric(textid)){
		results[, textid := as.numeric(textid)]
	}

	# Order the output
	setkey(results, originalorder, order)
	# Remove temporary doseid and other unnecessary variables
	results[, c('originalorder', 'doseid') := NULL]
	if ('text' %in% colnames(results)) results[, text := NULL]
	if ('i.textid' %in% colnames(results)) results[, i.textid := NULL]
	setnames(results, 'dosestrings', 'text')
	setcolorder(results, c('textid', 'order', 'text', 'qty',
		'units', 'freq', 'tot', 'max', 'time', 'change',
		'choice', 'duration', 'daily_dose'))
	cat('\nAnalysed', length(text), 'dosage texts.\n')
	
	if (!is.null(dosage_mg)){
		try(results <- calculate_mg(results, dosage_mg))
	}
	results[]
}

calculate_mg <- function(results, dosage_mg){
	# Arguments:
	#    results = output results, as from doseconvert
	#    dosage_mg = numeric vector with the same length as number
	#        of rows in results, with the strength of each dose unit
	#        in mg (e.g. if the dose unit is a 500mcg tablet,
	#        it will be 0.5). It should be missing if not 
	#        meaningful (e.g. for creams)
	# Adds a column 'daily_mg' to the output
	
	if (length(dosage_mg) != nrow(results)){
		stop(paste0('dosage_mg has ', length(dosage_mg),
			'elements but results has ', nrow(results), ' rows'))
	}
	
	results[, daily_mg := as.numeric(NA)]
	# Add dosage_mg column to results for ease of calculation
	results[, dosage_mg := dosage_mg]
	
	# Units that are mg quantities
	results[units == 'mg', daily_mg := daily_dose]
	results[units == 'gram', daily_mg := daily_dose * 1000]
	results[units == 'mcg', daily_mg := daily_dose / 1000]
	
	# Units that are dose units (or blank) e.g. tablets, capsules,
	# metered dose sprays / puffs - not drops
	results[units %in% c('', 'neb', 'pv', 'pre', 'puff', 'spray',
		'suppos', 'sachet', 'blister', 'amp', 'pill', 'cap', 'tab'),
		daily_mg := dosage_mg * daily_dose]
	results[, dosage_mg := NULL]
	
	results[]
}

interpret <- function(instring, singlewords, multiwords,
	patterns, noisy = FALSE, simplify = FALSE, maxifchoice = TRUE,
	id = NULL){
	# returns a data.frame with one (if simplify=TRUE) or
	# potentially many observations if there are multiple time
	# intervals with different doses.
	# If noisy=TRUE, write debug information to console
	
	# STAGE 0. Initialisation
	pd <- c('start', pdInit(instring))
	if (noisy) cat('\n====================\nInitialising: ',
		paste(pd, collapse = ' '))
	
	# STAGE 1. singlewords checking and replacement
	# only up to 1 word will be wordspace'd
	pd_numeric <- isNumeric(pd)
	replace <- singlewords[pd]
	unmatched <- is.na(replace) & !pd_numeric
	
	# Attempt to wordspace the largest unmatched word
	getlength <- nchar(pd[unmatched])
	if (length(getlength) > 0L) {
		# there is at least one unmatched word, so try wordspace
		dothis <- which(nchar(pd) == max(nchar(pd[unmatched])))[1]
		replacement <- wordspace(pd[dothis], singlewords = singlewords)
		if (!all(replacement == '')) {
			# do the replacement, and re-check against dictionaries
			pd <- c(pd[1L:(dothis - 1L)], replacement,
				pd[(dothis + 1L):length(pd)])
			pd_numeric <- isNumeric(pd)
			replace <- singlewords[pd]
			unmatched <- is.na(replace) & !pd_numeric
		}
	}
	
	remove <- (unmatched | replace == ',' |
		(is.na(replace) & !pd_numeric))
	remove[is.na(remove)] <- FALSE
	
	# replace words (not numeric entries)
	pd[!pd_numeric] <- replace[!pd_numeric]
	if (noisy) cat('\nAfter singlewords replacement: ',
		paste(pd, collapse=' '))
	
	# STAGE 2. Remove any unnecessary words or words between
	# two unnecessary words
	if (length(pd) == 3L) {
		if (identical(remove, c(TRUE, FALSE, TRUE))) {remove <- rep(TRUE, 3L)}
	} else if (length(pd) > 3L) {
		# look for sandwiched words
		remove <- remove | c(FALSE, remove[1L:(length(remove) - 2L)] &
			remove[3L:length(remove)], FALSE)
	}
	pd <- pd[!remove]
	# create a single string for numbers replacement and
	# dictionary matching
	pds <- ' ' %&% paste(pd, collapse = ' ') %&% ' '
	if (noisy) cat('\nAfter singlewords removal: ', pds)
	
	# STAGE 3. Numbers replacement phase one.
	# Note that numbersReplace returns whether there was a choice of
	# dose. If maxifchoice is TRUE, the maximum dose is returned.
	# If maxifchoice is FALSE, the average dose is returned
	nrep1 <- numbersReplace(pds, maxifchoice)
	pds <- nrep1$pds
	if (noisy) cat('\nAfter numbers replace 1: ', pds, " Choice: ",
		nrep1$choice)

	# STAGE 4. First dictionary
	# Dictionaries are now written with standard (Unix) regular
	# expressions
	# Use gsub to replace all, useBytes=TRUE for faster matching
	for (i in 1:nrow(multiwords)){
		newpds <- gsub(multiwords[i, 'words'],
			multiwords[i, 'replacement'], pds, useBytes=TRUE)
		if (pds != newpds) {
			if (noisy) cat('\nMultiwords match: ',
				multiwords[i, 'words'])
			if (noisy) cat('\n... phrase updated to: ', newpds)
			pds <- newpds
		}
	}
	
	# STAGE 5. Numbers replacement phase two.
	nrep2 <- numbersReplace(pds, maxifchoice)
	pds <- nrep2$pds
	choice <- nrep1$choice | nrep2$choice
	if (noisy) cat('\nAfter numbers replace 2: ', pds, " Choice: ",
		choice, '\n')
	
	# Convert pd string to pd
	pd <- strsplit(pds, ' ')[[1]]
	pd <- pd[!pd == '']
	
	# STAGE 6.
	# Note that doubledose and max are now in the 'patterns' dictionary
	# The object is called 'thedose' instead of 'overall2'
	# The only modifications made to dose are choice of dose
	# (stored as choice) 
	thedose <- analyseDose(pd, noisy, patterns = patterns)
	if (noisy){
		cat('\nAfter analyseDose:\n')
		print(thedose)
	}
	
	if (choice == TRUE & all(thedose$choice %in% c('nochoice', 'asneeded'))){
		if (noisy) cat('\nRegistering choice of doses\n')
		thedose$choice <- sub('nochoice', 'choice', thedose$choice)
		if (maxifchoice){
			thedose$max <- rep('max', nrow(thedose))
		} else {
			thedose$max <- rep('average', nrow(thedose))
		}
	}
	
	# double dose for 'each ear' or 'each eye'
	if (any(thedose$doubledose)) {
		if (noisy) cat('\nDoubling dose for both ears / eyes')
		thedose$tot <- thedose$tot*2
		thedose$qty <- thedose$qty*2
	}
	# note that the quantity is doubled; this is a change
	# from the previous version
	
	if (any(thedose$units == 'fiveml')) {
		if (noisy) cat('\nConverting fiveml')
		thedose$units <- rep('ml', nrow(thedose))
		thedose$tot <- thedose$tot*5
		thedose$qty <- thedose$qty*5
	}
	
	# Frequency = 0 means that the frequency is not specified

	# Increase any time intervals so that minimum frequency is 1
	changeme <- (thedose$freq < 1 & thedose$freq > 0)
	if (any(changeme)) {
		thedose[changeme, 'time'] <-
			thedose[changeme, 'time'] / thedose[changeme, 'freq']
		thedose[changeme, 'tot'] <-
			thedose[changeme, 'tot'] / thedose[changeme, 'freq']
		thedose[changeme, 'freq'] <- 1	
	}

	# Simplify into a single dose if required
	thedose$id <- id
	if (simplify == TRUE & nrow(thedose) > 1) {
		if (noisy) cat('\nSimplfying into a single dose.\n')
		thedose <- simplifydose(thedose, noisy)
		if (!is.null(id)) {
			# use ID for row names
			row.names(thedose) <- id
		}
	} else {
		# Not simplifying
		if (!is.null(id)) {
			# use ID.X for row names where X is the row number
			row.names(thedose) <- paste(id, 1:nrow(thedose), sep='.')
		}
	}

	# Also add a separate column for dose line order
	thedose$order <- 1:nrow(thedose)
	
	# Add daily dose
	thedose$daily_dose <- 0
	hasdailydose <- !is.na(thedose$tot) & !is.na(thedose$time)
	thedose$daily_dose[hasdailydose] <-
		thedose$tot[hasdailydose] / thedose$time[hasdailydose]
	thedose$daily_dose[thedose$daily_dose == Inf] <- 0
	
	# Convert missing numerical data to zero
	thedose$qty[is.na(thedose$qty)] <- 0
	thedose$freq[is.na(thedose$freq)] <- 0
	thedose$time[is.na(thedose$time)] <- 0
	thedose$tot[is.na(thedose$tot)] <- 0
	thedose$duration[is.na(thedose$duration)] <- 0
	
	if (noisy) cat('\nFinished analysing dose.\n================\n')
	
	# Remove unnecessary columns
	thedose$words <- NULL
	thedose$doubledose <- NULL
	thedose$link <- NULL
	thedose$divpos <- NULL
	thedose$priority <- NULL
	thedose[]
}

simplifydose <- function(regimens, noisy = FALSE) {
	# like combineparts, but combine different dose lines with
	# defined durations (e.g. 1 daily for 1 week then 2 daily for 1 week)
	# to produce output similar to the initial (VB) version of 
	# the dose analysis software
	
	while (nrow(regimens) > 1) {
		# loop through, combining doses until there is only one dose
		if (regimens$link[1] == 'stat') {
			# deal with a 'stat' or loading dose
			# assume frequency is the same as main part of prescription
			if (regimens$time[2] > 0) {
				if (regimens$freq[2] > 0) {
					# most common situation, hopefully
					# frequency is taken from instruction after stat dose
					# stat dose assumed to be taken at the same frequency
					# as main dose (e.g. stat dose before a three times a 
					# day dose would be taken 1/3 of a day before)
					regimens$duration[1] <- 
						regimens$time[2] / regimens$freq[2]
					regimens$time[1] <- regimens$time[2] / regimens$freq[2]
					regimens$freq[1] <- 1
				} else {
					# second dose does not have frequency
					# assume that stat dose section contains frequency
					regimens$duration[1] <- regimens$time[2]
					regimens$time[1] <- regimens$time[2]
				}
			} else {
				# assume stat dose lasts for 1 day
				regimens$freq[1] <- 1
				regimens$time[1] <- 1
				regimens$duration <- 1
			}
			if (noisy){
				cat('\nAfter preparing stat dose line:\n')
				print(regimens[1:2,])
			}
		}
		
		# combine doses if they both have a duration
		# use 2nd dose if one or both have no duration and both are valid
		if (regimens$duration[1] > 0 & regimens$duration[2] > 0) {
			# 1st dose   2nd dose         Combined
			# freq, qty  freq, qty[same]  weighted average freq
			# freq, qty  freq[same], qty  weighted average qty
			# freq, qty  freq, qty        weighted average qty, freq from 1st dose
			# freq, qty  freq             assume 2nd dose qty is the same
			# freq, qty  qty              assume 2nd dose freq is the same
			# freq, qty  [other]          assume 2nd dose is the same as first
			# [missing]  [any]            use freq, qty from 2nd dose
			
			# first fill in missing frequencies, quantities and totals
			# (unless both freq and qty are 0, in which case it might be
			# intentional)
			if (regimens$freq[2] == 0 & regimens$qty[2] > 0){	
				regimens$freq[2] <- regimens$freq[1]
			} else if (regimens$qty[2] == 0 & regimens$freq[2] > 0){
				regimens$qty[2] <- regimens$qty[1]
			} else if (regimens$freq[1] == 0 & regimens$qty[1] > 0){	
				regimens$freq[1] <- regimens$freq[2]
			} else if (regimens$qty[1] == 0 & regimens$freq[1] > 0){
				regimens$qty[1] <- regimens$qty[2]
			}
			if (noisy){
				cat('\nPreparing to combine dose lines:\n')
				print(regimens[1:2,])
			}
			# Check that time is the same between the dose lines
			if (regimens$time[1] != regimens$time[2]){
				regimens$freq[2] <- regimens$freq[2] *
					(regimens$time[1] / regimens$time[2])
				regimens$tot[2] <- regimens$tot[2] *
					(regimens$time[1] / regimens$time[2])
				regimens$time[2] <- regimens$time[1]
				cat('\nStandardising time between dose lines:\n')
				print(regimens[1:2,])
			}
			if (regimens$qty[1] == 0 & regimens$qty[2] == 0){
				# no quantities, only frequencies (e.g. 'apply twice daily')
				cat('\nNo quantities, only frequencies\n')
				totaldose <- sum(regimens$tot[1:2] * regimens$duration[1:2])
				totalfreq <- sum(regimens$freq[1:2] * regimens$duration[1:2])
				regimens$tot[1] <- totaldose / sum(regimens$duration[1:2])
				regimens$freq[1] <- totalfreq / sum(regimens$duration[1:2])
			} else if (regimens$qty[1] == regimens$qty[2] & regimens$freq[1] > 0) {
				# equal qty, nonmissing freq --> calculate average freq
				cat('\nEqual quantities, calculating average frequency\n')
				totalfreq <- sum(regimens$freq[1:2] * regimens$duration[1:2])
				regimens$freq[1] <- totalfreq / sum(regimens$duration[1:2])
				regimens$tot[1] <- regimens$freq[1] * regimens$qty[1]
			} else if (regimens$freq[1] == regimens$freq[2]){
				# equal freq (and nonmissing qty) --> calculate average qty
				cat('\nEqual frequency, calculating average quantity\n')
				totalqty <- sum(regimens$qty[1:2] * regimens$duration[1:2])
				regimens$qty[1] <- totalqty / sum(regimens$duration[1:2])
				regimens$tot[1] <- regimens$freq[1] * regimens$qty[1]
			} else if (regimens$time[2] > 0) {
				cat('\nUse frequency and time from first dose; standardise quantity\n')
				# at least some data in dose line 2
				# use frequency from first dose; scale quantity appropriately
				regimens$qty[2] <- regimens$tot[2] / regimens$freq[1]
				regimens$freq[2] <- regimens$freq[1]
				# now calculate average quantity
				totalqty <- sum(regimens$qty[1:2] * regimens$duration[1:2])
				regimens$qty[1] <- totalqty / sum(regimens$duration[1:2])
				regimens$tot[1] <- regimens$freq[1] * regimens$qty[1]
			} else if (regimens$time[2] == 0) {
				# no data in dose 2; use qty and freq from dose 1
				# (all values should already be in the right place)
				cat('\nNo daily dose in dose 2\n')
			}
			# add durations. 'change' marked as combined doses
			regimens$duration[1] <- sum(regimens$duration[1:2])
			regimens$change[1] <- 'combined'
			if (noisy){
				cat('\nAfter combining dose lines:\n')
				print(regimens[1,])
			}
		} else {
			# use the first dose if:
			# it is a valid dose AND (dose2 not valid OR dose2 is zero)
			if (regimens$time[1] > 0 & (
				regimens$time[2] == 0 | regimens$tot[2] == 0)) {
				# use first dose, as second dose is not complete
				regimens$change[1] <- 'first'
				if (noisy){
					cat('\nKeeping first dose line:\n')
					print(regimens[1,])
				}
			} else {
				# otherwise use second dose
				if (regimens$link[1] == 'stat') {
					if (regimens$qty[1] > 0 & regimens$qty[2] == 0 &
						all(regimens$freq[1:2] > 0)) {
						regimens$qty[2] <- regimens$qty[1]
						regimens$tot[2] <- regimens$freq[2] * regimens$qty[2]
					}
				}
				items <- c('time', 'freq', 'qty', 'tot', 'duration')
				regimens[1, items] <- regimens[2, items]
				regimens$change[1] <- 'second'
				if (noisy){
					cat('\nKeeping second dose line:\n')
					print(regimens[1,])
				}
			}
		}
		# ensure that time is minimum 1 day
		if (regimens$time[1] < 1 & regimens$time[1] > 0){
			if (noisy) cat('\nEnsuring that time interval is 1 day.\n')
			regimens$freq[1] <- regimens$freq[1] / regimens$time[1]
			regimens$tot[1] <- regimens$tot[1] / regimens$time[1]
			regimens$time[1] <- 1
		}
		
		# remove the second row now that it has been
		# incorporated into the first row
		if (nrow(regimens) > 2) {
			regimens <- regimens[c(1, 3:nrow(regimens)),]
		} else {regimens <- regimens[1,]}
	}
	regimens$link <- NULL
	regimens$divpos <- NULL
	regimens
}

analyseDose <- function(pd, noisy = FALSE, patterns = patterns){
	# Analyses the text after pre-processing.
	# Splits the dose into sections, analyses each section,
	# and combines the results
	# divide up dosage instruction into sections divided at:
	# and, or, upto, max, then, reducing to, etc.
	# then analyse each bit separately
	
	# blank if dosage instruction consists of one word
	if (length(pd) < 2) {
		if (noisy){
			cat('\nSingle word: ', pd, '; cannot interpret.\n')
		}
		return(singledosage())
	} else {
		if (noisy){
			cat('\nRunning analyseDose\n')
		}
	}

	# define all possible link points
	pos_link_word <- pd %in% c('and', 'or', 'upto',
		'changeto', 'day', 'morning', 'midday', 'afternoon',
		'evening', 'night', 'stat', 'max', 'miss')
	# filter link points to select the last of a series
	pos_link_word <- pos_link_word &
		!c(pos_link_word[2:length(pos_link_word)], FALSE)
	# Last word is always a link word to designate end of section
	pos_link_word[length(pos_link_word)] <- TRUE
	
	all_sections_have_data <- FALSE
	
	while (all_sections_have_data == FALSE) {
		# identify locations of link words
		divpos <- which(pos_link_word)
		# vector of link words
		link <- pd[divpos]
		# different time of day sections are all classified
		# with the same word
		link[link %in% c('morning', 'midday', 'afternoon',
			'evening', 'night')] <- 'day'

		# check that each section contains some data; if not,
		# combine with previous section and re-analyse
		trial <- list()
		all_sections_have_data <- TRUE
		strings <- restring(pd, divpos)
		for (b in 1:(length(strings))){
			# trial is a list of dosages
			trial[[b]] <- partDose(strings[b],
				noisy = noisy, patterns = patterns)
			
			if (trial[[b]]$tot == 0 & trial[[b]]$time == 0 &
				length(divpos) > 1){
				if (b == 1){
					if (pos_link_word[divpos[b]] == TRUE) {
						pos_link_word[divpos[b]] <- FALSE
						# combine with next section
						all_sections_have_data <- FALSE
						# only set to false if a change has been made
					}
				}
			}

			if (noisy & length(trial) > 1){
				cat('\nAnalysing dose in', length(trial), 'sections:\n')
				for (b in 1:(length(trial))){
					cat('\nSection: ', strings[b], '\n')
					print(trial[[b]])
				}
			}
			
		}
		# If all_sections_have_data is FALSE, re-run this loop
	}

	# That was the first section - ensuring that each part of the
	# interpreted text has some data. Next stage: combine sections.
	# However in this program we will not combine stat and changeto
	# because these can bedone separately if desired, by the
	# simplifydose function called by interpret.

	trial <- do.call('rbind', trial)
	trial$link <- link
	
	if (noisy){
		cat('\nPart extracted dosage information in multiple parts:\n')
		print(trial)
		cat('\nNow combining parts\n')
	}

	trial <- combineParts(trial, "or", noisy)
	trial <- combineParts(trial, "and", noisy)
	trial <- combineParts(trial, "day", noisy)
	trial <- combineParts(trial, "upto", noisy)
	trial <- combineParts(trial, "max", noisy)
	trial <- combineParts(trial, "miss", noisy)

	return(trial)
}

pdInit <- function(instring){
	# Quite slow, takes 0.1 seconds per text!
	# Initialise text, splitting words if there are no
	# spaces between them. Use lower case only.
	instring <- tolower(instring) %&% " "
	# pre-allocate an output character vector which is 
	# at least as long as required
	outstring <- character(nchar(instring))
	
	# looping through words
	i <- 0L
	# looping through letters
	j <- 0L
	
	while (j < nchar(instring)){
		# go to next word
		i <- i + 1L
		while (j < nchar(instring) & (outstring[i] == '' |
			sameWord(outstring[i], substr(instring, j+1, j+1)))) {
			# go to next letter
			j <- j + 1L
			outstring[i] = outstring[i] %&% substr(instring, j, j)
		}
	}
	length(outstring) <- i
	# remove extra words and spaces
	outstring[!(outstring %in% c('', ' ', ',', '.'))]
}

wordspace <- function(word, maxwd = 8, singlewords) {
	# A simpler R-style version of wordspace
	# Recursive method to builds a list of potential matches
	# Choose the first match with no words repeated and no single
	# letter words, or evaluate all and score them. 
	
	# if single letter, exit function
	if (nchar(word) == 1) return(word)
	
	# remove trailing X (e.g. dailyxxxxxxxxxxxxxxxxxx)
	word <- sub('[x]+$', '', word)

	# don't bother if word is more than 13 characters
	if (nchar(word) > 13) return(word)
	
	# now the main part of the function to try possible splits
	maxtrials <- 20
	numtried <- 0
	found <- FALSE
	splits <- matrix(logical(maxtrials * nchar(word)),
		nrow = maxtrials, ncol = nchar(word))
	scores <- numeric(maxtrials)

	pushback <- function(mybool) {
		# switches the last set of contiguous TRUE to FALSE
		# and switches the immediately preceding item to TRUE
		starttrue <- (mybool == TRUE &
			c(FALSE, mybool[1:(length(mybool) - 1)]) == FALSE)
		mybool[max(which(starttrue)) - 1] <- TRUE
		mybool[max(which(starttrue)):length(mybool)] <- FALSE
		mybool
	}
	
	match <- function(mybool) {
		# whether this is a partial (all except last segment)
		# or full match against the singlewords dictionary
		words <- strsplitPos(word, mybool)
		trial <- singlewords[words]
		if (length(trial) == 1) {
			if (is.na(trial)) return(0) else return(50)
		}
		if (!any(is.na(trial[1:(length(trial)-1)]))) {
			# at least a partial match
			if (is.na(trial[length(trial)])) {
				return(1)
			} else {
				# calculate a matchscore according to
				# fewer number of words, fewer one-letter words, lack
				# of repetition: 
				return(50 - (length(words) - length(unique(words)) - 
					sum(nchar(words) == 1)))
			}
		} else return(0)
	}
	
	cursplit <- c(rep(FALSE, nchar(word)-1), TRUE)
	# looking for possible split points
	while (found == FALSE & any(cursplit) & numtried < maxtrials &
		sum(cursplit) < maxwd) {
		# continue while a good match has not been found, current
		# split suggestion includes at least one split, fewer than
		# maximum number of trials have been attempted and total
		# number of split points does not exceed limit
		matchscore <- match(cursplit)
		if (matchscore == 0) {
			# no match
			cursplit <- pushback(cursplit)
		} else if (matchscore > 1) {
			# full match; to record this match
			numtried <- numtried + 1
			splits[numtried,] <- cursplit
			scores[numtried] <- matchscore
			cursplit <- pushback(cursplit)
			if (matchscore == 50) found <- TRUE
		} else {
			# partial match - so try an additional split
			if (cursplit[length(cursplit)] == FALSE) {
				cursplit[length(cursplit)] <- TRUE
			} else {
				cursplit <- pushback(cursplit)
			}
		}
	}
	if (found == TRUE) {
		return(strsplitPos(word, which(splits[numtried,])))
	} else if (numtried > 0) {
		# Selects the first match with highest score
		return(strsplitPos(word, which(splits[
			which(scores == max(scores, na.rm = TRUE))[1],])))
	} else {
		return('')
	}
}

numbersReplace <- function(pds, maxifchoice) {
	# requires pds as a character string
	# (with one space between words)
	# returns a list containing the modified pds string
	# and a TRUE/FALSE variable as to whether
	# there is a choice of dose
	# Arguments:  pds = character string
	#             maxifchoice = Boolean, whether to return maximum dose
	#                 if there is a choice of dose
	
	# default
	choice <- FALSE
	
	# 1O ml etc.
	pds <- gsub(' ([[:digit:]]+) o (ml|microgram|mcg|gram|mg) ',
		' \\10 \\2 ', pds)
	# t d s
	pds <- gsub(' ([[:lower:]]{1}) ([[:lower:]]{1}) ([[:lower:]]{1}) ',
		' \\1\\2\\3 ', pds)
	# 1/2
	pds <- gsub(' 1 / 2 ', ' 0.5 ', pds)
	# 1.5 times 2 ... (not used for 5ml doses, because this is treated as a separate dose units)
	if (!grepl(' ([[:digit:].]+) (times|x) ([[:digit:]]+) 5 ml', pds)) {
		pds <- gsub(' ([[:digit:].]+) (times|x) ([[:digit:]]+) ', 
		' \\1*\\3 ', pds)
	}
	# 1 mg x 2 ... (but not 1 mg x 5 days)
	if (!grepl(' ([[:digit:].]+) (ml|mg|gram|mcg|microgram|unit) (times|x) ([[:digit:]]+) (days|month|week) ', pds)) {
		pds <- gsub(' ([[:digit:].]+) (ml|mg|gram|mcg|microgram|unit) (times|x) ([[:digit:]]+) ',
		' \\1*\\4 \\2 ', pds)	
	}
	# 1 drop or 2 ...
	pd2 <- strsplit(gsub('^[[:print:]]*([[:digit:].]+) (tab|drops|cap|ml|puff|fiveml) (to|-|star) ([[:digit:].]+)[[:print:]]*$',
		'MATCHED \\1 \\4', pds), ' ')
	if ((pd2[1])=='MATCHED') {
		# check that upper dose limit is greater than lower, otherwise
		# the text may not actually represent a dose range
		if (pd2[3] > pd2[1]) {
			pds <- gsub(' ([[:digit:].]+) (tab|drops|cap|ml|puff|fiveml) (to|-|star) ([[:digit:].]+) ',
			' \\1 \\2 or \\4 ', pds)
			# this function does not combine the dose options
			choice <- TRUE
		} else {
			# not a choice, two pieces of information (e.g. '25mg - 2 daily')
			pds <- gsub(' ([[:digit:].]+) (tab|drops|cap|ml|puff|fiveml) (to|-|star) ([[:digit:].]+) ',
	 		' \\1 \\2 \\4 ', pds)			
		}
	}
	# 1 and 2 ...
	pds <- gsub(' ([[:digit:].]+) (and|\\+) ([[:digit:].]+) ',
		' (\\1+\\3) ', pds)
	# 3 weeks ...
	pds <- gsub(' ([[:digit:].]+) (week) ',
		' \\1\\*7 days ', pds)
	# 3 months ... NB assume 30 days in a month
	pds <- gsub(' ([[:digit:].]+) (month) ',
		' \\1\\*30 days ', pds)
	# day 1 to day 14 ...
	pds <- gsub(' day ([[:digit:]]+) (to|-) day ([[:digit:]]+) ',
		' for \\3-\\1 days ', pds)
	# X times day to X times day
	if (maxifchoice){
		pds2 <- gsub(' ([[:digit:]]+) (times|x) day (to|or|-|upto|star) ([[:digit:]]+) (times|x) day ',
			' max(c(\\1,\\4)) times day ', pds)
	} else {
		pds2 <- gsub(' ([[:digit:]]+) (times|x) day (to|or|-|upto|star) ([[:digit:]]+) (times|x) day ',
			' (\\1+\\4)/2 times day ', pds)
	}
	if (pds2 != pds) {
		choice <- TRUE
		pds <- pds2
	}
	# days 1 to 14 ...
	pds <- gsub(' days ([[:digit:]]+) (to|-) ([[:digit:]]+) ',
		' for \\3-\\1 days ', pds)
	# 1 or 2 ... moved to below 'days X to X' because
	# otherwise the day numbers would be averaged
	if (maxifchoice){
		pds2 <- gsub(' ([[:digit:].]+) (to|or|-|star) ([[:digit:].]+) ',
			' max(c(\\1,\\3)) ', pds)
	} else {
		pds2 <- gsub(' ([[:digit:].]+) (to|or|-|star) ([[:digit:].]+) ',
			' (\\1+\\3)/2 ', pds)	
	}
	if (pds2 != pds) {
		choice <- TRUE
		pds <- pds2
	}
	# X times or X times ...
	if (maxifchoice){
		pds2 <- gsub(' ([[:digit:].]+) times (to|or|-|star) ([[:digit:].]+) times ',
			' max(c(\\1,\\3)) times ', pds)
	} else {
		pds2 <- gsub(' ([[:digit:].]+) times (to|or|-|star) ([[:digit:].]+) times ',
			' (\\1+\\3)/2 times ', pds)	
	}
	if (pds2 != pds) {
		choice <- TRUE
		pds <- pds2
	}
	# x days every x days
	pds <- gsub('( for | )([[:digit:]\\.]+) days every ([[:digit:]\\.]+) days ',
		' for \\2 days changeto 0 0 times day for \\3-\\2 days ', pds)
	
	# Evaluate expressions
	pd <- strsplit(pds, ' ')[[1]]
	pd <- pd[!is.na(pd)]
	pd <- pd[pd != '' & pd != ' ']

	# If pd contains an expression that can be converted to a number
	# try to evaluate it. If it contains any letters (except 'max' as
	# part of an expression, e.g. max(2, 3) then ignore. If it is just
	# a single item of punctuation without numbers also ignore.
	dont_evaluate <- (grepl('[a-z]', pd) & !grepl('^max', pd)) |
		pd %in% c('(', ')', ':', '/', '&', '-')
	for (i in seq_along(pd)){
		if (!dont_evaluate[i]){
			try(pd[i] <- as.character(eval(parse(text = pd[i]))))
		}
	}
	
	pds <- paste(' ', paste(pd, collapse = ' '), ' ', sep = ' ')
	list(pds = pds, choice = choice)
}

singledosage <- function(words = '', qty = 0, units = '', freq = 0,
	tot = 0, priority = 0, max = c('exact', 'max', 'average'), time = 0,
	change = c('nochange', 'first', 'second', 'combined'),
	choice = c('nochoice', 'choice', 'asneeded'),
	doubledose = FALSE, duration = 0){
	max <- factor(max, c('max', 'average', 'exact'))[1]
	change <- factor(change, c('first', 'second', 'nochange', 'combined'))[1]
	choice <- factor(choice, c('choice', 'asneeded', 'nochoice'))[1]
	data.frame(words = words, qty = qty, units = units, freq = freq,
		tot = tot, priority = priority, max = max, time = time, change = change,
		choice = choice, doubledose = doubledose, duration = duration)
}

extractNum <- function(pattern_dose, existing_dose, pd_matched,
	noisy = FALSE){
	# new_dose is from the patterns dictionary
	# existing dose is the trial or existing dose
	# pd_matched is a character vector starting from the
	# first matched position

	# units of dose
	if (pattern_dose$units != '' & pattern_dose$priority >=
		existing_dose$priority) {
		existing_dose$priority <- pattern_dose$priority
		existing_dose$units <- sub(pattern_dose$words,
			trim(pattern_dose$units), paste(c(' ', pd_matched, ' '),
			collapse = ' '))
	}
	
	# numerical quantities
	for (item in c('tot', 'freq', 'qty', 'time', 'duration')){
		if (pattern_dose[,item] == '') {
			# blank; ignore
		} else {
			existing_dose[, item] <- as.numeric(sub(
				pattern_dose$words, pattern_dose[, item],
				paste(c(' ', pd_matched, ' '), collapse = ' ')))
		}
		# delete frequency and quantity information if they were based
		# on previous patterns and are incorrect
		if (item == 'tot' & existing_dose$tot > 0 &
			existing_dose$tot != existing_dose$freq*existing_dose$qty &
			existing_dose$freq > 0 & existing_dose$qty > 0) {
			if (noisy){
				cat('\nProvisional total', existing_dose$tot, 'not equal to',
					existing_dose$qty, 'x', existing_dose$freq, '.\n')
			}
			existing_dose$freq <- 0
			existing_dose$qty <- 0
		}
	}

	# doubledose
	if (pattern_dose$doubledose == TRUE) {existing_dose$doubledose <- TRUE}

	# choice of dose
	if (pattern_dose$choice == 'asneeded' |
		(pattern_dose$choice == 'choice' &
		existing_dose$choice != 'asneeded')) {
		existing_dose$choice <- pattern_dose$choice
	}

	# maximum dose
	if (pattern_dose$max == 'max') {existing_dose$max <- 'max'}
	return(existing_dose)
}

partDose <- function(text, noisy=FALSE, patterns) {
	# extracts dosage information for a specified text
	# uses the patterns dictionary, starting with a blank dose
	
	# Sanitize text and append 'start' to the front
	# ensuring that text has one space at the beginning and end
	text <- ' start ' %&% sub('^ +', '', sub(' +$', '', text)) %&% ' '
	text <- sub(' start start', ' start', text)
	trial <- singledosage()

	for (i in 1:nrow(patterns)) {
		# find all matches, and extract information from pattern
		matches <- gregexpr(patterns$words[i], text)[[1]]
		if (matches != -1) {
			# match found
			for (j in length(matches)) {
				# create vector of matched text ignoring initial space
				matchtext <- strsplit(substr(text, matches[j] + 1,
					matches[j] + attr(matches, 'match.length')[j] - 1),
						' ')[[1]]
				if (noisy) {
					cat('\nPattern "', trim(patterns[i,'words']),
						'" matches: "', matchtext, '"\n')
				}

				trial2 <- extractNum(patterns[i,], trial, matchtext,
					noisy = noisy)
				if (noisy) {
					cat('... Existing dose information:\n')
					print(trial)
					cat('... New dose information:\n')
					print(trial2)
					for (item in c('qty', 'units', 'freq', 'max', 'time',
						'change', 'choice', 'doubledose', 'duration')){
						if (trial[1, item] != trial2[1, item] &
							trial2[1, item] != 0) {
							cat('... ', item, trial2[1, item], ' extracted.\n')
						}
					}
					if (trial$tot != trial2$tot){
						cat('\n... Total quantity per time period: ', trial2$tot, '\n')
					}
				}
				trial <- trial2
			}
		}
	}
	
	# calculate total dose
	if (trial$freq > 0 & trial$qty > 0) {
		trial$tot <- trial$qty * trial$freq
	}
	if (trial$tot == 0 & trial$qty == 0) {
		trial$tot <- trial$freq
	}
	if (trial$tot == 0 & trial$freq == 0) {
		trial$tot <- trial$qty
	}
	
	# clean up
	trial$units <- trim(trial$units)
	
	# return the interpreted data
	return(trial)
}

combineParts <- function(trial, linkword, noisy = FALSE) {
	# combines adjacent parts of the dose instructions according to a
	# chosen linkword
	
	# trial is a data.frame with a column 'link' which contains
	# the linkword at the end of that section.

	if (nrow(trial) == 1){
		return(trial)
	} else {
		nparts <- nrow(trial)
	}
	
	# sub-function for combining 2 dosages
	combine2parts <- function(a, b, linkword) {
		if (a$units == 'fiveml' & b$units == 'ml') {
			b$units <- 'fiveml'
			b$tot <- b$tot / 5
			b$qty <- b$qty / 5
		} else if (a$units == 'ml' & b$units == 'fiveml') {
			b$units <- 'ml'
			b$tot <- b$tot * 5
			b$qty <- b$qty * 5			
		} else if (a$units == '') {
			a$units <- b$units
		} else if (b$units == '') {
			# nothing happens
		} else if (a$priority == 5 & b$priority==3) {
			# e.g. 20 mg tab mane and 2 tabs nocte
			# change the second quantity to a frequency
			# to allow correct adding up
			if (a$freq > 0 & (b$freq %in% c(0, 1))) {
				b$freq <- b$qty
				b$qty <- 0
			}
		} else if (a$units != b$units) {
			# if both a and b are valid doses, make b replace a if
			# higher priority (e.g. mcg over puffs) otherwise stick with a.
			if (a$tot > 0 & b$tot > 0 & a$time > 0 & 
				b$time > 0 & a$priority < b$priority) {
				a$units <- b$units
				items <- c('freq', 'qty', 'tot', 'time')
				a[,items] <- b[,items]
			}
		}
	
		# if dose 2 has low quantity but no units,
		# convert it to frequency
		if (a$qty > 5 * b$qty & b$qty >= 1) {
			if (a$freq > 0 & (b$freq %in% c(0,1)) & b$priority < 4) {
				b$freq <- b$qty
				b$qty <- 0
			}
		}

		# if very high frequency, convert it to quantity
		if (a$freq > 10) {
			if (a$qty %in% c(0,1)) {
				a$qty <- a$freq
				a$freq <- 0
			}
		}
		if (b$freq > 10) {
			if (b$qty %in% c(0,1)) {
				b$qty <- b$freq
				b$freq <- 0
			}
		}

		# standardise the time period (but not if first dose is 'STAT')
		if (a$time > 0 & b$time > 0 & a$duration != 1) {
			if (b$freq > 0) {
				b$freq = b$freq * (a$time / b$time)
			} else {
				b$qty = b$qty * (a$time / b$time)
			}
			b$tot = b$tot * (a$time / b$time)
			b$time <- a$time
		}

		if (a$change == 'nochange' & b$change == 'first') {
			a$change <- 'first'
		}
		if (a$change == 'nochange' & b$change == 'second') {
			a$change <- 'second'
		}
		if (a$change == 'first' & b$change == 'second') {
			a$change <- 'second'
		}
		if (a$choice == 'nochoice') {
			a$choice <- b$choice
		}
		if (a$priority == 0) {a$priority <- b$priority}
		if (a$max == 'exact') {a$max <- b$max}
		if (b$qty > 0 & linkword == 'upto') {linkword <- 'or'}
 
		if (linkword == 'miss'){
			# subtract the second dose or frequency
			if (b$qty == 0 | b$qty == a$qty){
				if (b$time == 0){
					a$freq <- a$freq - b$freq
				} else {
					a$freq <- a$freq - b$freq * (a$time / b$time)
					a$tot <- a$qty * a$freq
				}
			}
		} else if (linkword %in% c('and', 'day')) {
			# upto 10mg every day ...
			if (linkword == 'day' & (b$time == 0 | b$max == 'max')) {
				# ignore b: 1 ANY, 2 QTY BUT NO TIME: e.g. 1bd and | sixty
			} else if (b$freq == 0 & b$qty > 0 & b$time == 0) {
				if (a$tot > 0 & a$time > 0) {
					# ignore b
				} else {
					#	e.g. 150mg + 250mg
					if (a$freq == 0 | a$freq == 1) {
						a$qty <- a$qty + b$qty
						a$tot <- a$qty
					} else if (a$qty > 0) {
						a$tot <- a$tot + b$tot
						a$freq <- a$tot / a$qty
					}
				}
			} else if (a$freq == 0 & a$qty > 0 & a$time == 0) {
				#	1 QTY BUT NO TIME, 2 ANY: one and | two twice a day
				a$freq <- b$freq
				a$time <- b$time
				a$qty <- a$qty + b$qty
				a$tot <- a$freq * a$qty
			} else if (a$freq > 0 & a$qty == 0 & b$freq > 0 & b$qty > 0) {
				# FREQUENCIES ONLY: 2 times a day and | once at night
				a$freq <- a$freq + b$freq
				a$tot <- a$tot + b$tot
			} else if (a$freq %in% c(0,1) & a$qty > 0 & 
				b$freq %in% c(0,1) & b$qty > 0) {
				# QUANTITIES ONLY: 3 tablets and | 4 tablets
				a$tot <- a$tot + b$tot
				if (a$time == 0) {
					a$qty <- a$qty + b$qty
				} else {
					a$freq <- a$tot / a$qty
				}
			} else if (a$freq %in% c(0,1) & a$qty > 0 & b$freq > 0 & 
				b$qty > 0) {
				#	1 QUANTITY, 2 FREQUENCY: 
				# 5 ml before breakfast and | twice a day
				a$freq <- 1 + b$freq
				a$tot <- a$freq * a$qty
			} else if (a$freq > 0 & a$qty > 0 & b$freq > 0 & b$qty > 0) {
				# 1 QTY, FREQ; 2 FREQ: 10 ml every morning and | twice a day
				a$freq <- a$freq + b$freq
				a$tot <- a$freq * a$qty
			} else {
				# OTHER: 3 after breakfast and | 2 at night
				a$tot <- a$tot + b$tot
				# stick with temp1.qty; adjust a$freq so totals compute
				if (a$freq > 0 & a$qty > 0) {
					a$freq <- a$tot / a$qty
				}
			}
			# combine duration e.g. 'one bd or two bd for 7 days'
			if (a$duration == 0) {a$duration <- b$duration}
		} else if (linkword == 'or') {	 
			if (b$freq == 0 & b$qty > 0 & b$time ==0) {
				# 1 ANY, 2 QTY BUT NO TIME: 1bd - | sixty -- ignore b
			} else if (a$freq == 0 & a$qty > 0 & a$time == 0) {
				# 1 QTY BUT NO TIME, 2 ANY: one - | two twice a day
				a$freq <- b$freq
				a$time <- b$time
				a$qty <- (a$qty + b$qty) / 2
				a$tot <- a$tot * a$qty
			} else if (a$freq > 0 & a$qty > 0 & b$freq > 0 & b$qty == 0) {
				# FREQUENCIES ONLY: 2 times a day or | once at night
				a$freq <- (a$freq + b$freq) / 2
				a$tot <- (a$tot + b$tot) / 2
			} else if (a$freq %in% c(0,1) & a$qty > 0 & 
				b$freq %in% c(0,1) & b$qty > 0) {
				# QUANTITIES ONLY: 3 tablets or | 4 tablets
				a$qty <- (a$qty + b$qty) / 2
				a$tot <- (a$tot + b$tot) / 2
			} else if (a$freq %in% c(0,1) & a$qty > 0 & b$freq > 0 & 
				b$qty > 0) {
				# 1 QUANTITY, 2 FREQUENCY: 5 ml daily or | twice a day
				a$freq <- (1 + b$freq) / 2
				a$tot <- a$freq * a$qty
			} else if (a$freq > 0 & a$qty > 0 & b$freq > 0 & b$qty > 0) {
				# 1 QTY, FREQ; 2 FREQ: 10 ml every morning or | twice a day
				a$freq <- (a$freq + b$freq) / 2
				a$tot <- a$tot * a$qty			
			} else if (b$tot > 0) {
				# OTHER: 3 after breakfast or | 2 after breakfast
				a$tot <- (a$tot + b$tot) / 2
				# stick with a$qty; adjust a$freq so totals compute
				if (a$qty > 0) {a$freq <- a$tot / a$qty}
			}
			a$max <- 'average'
			# combine duration e.g. 'one bd or two bd for 7 days'
			if (a$duration == 0) {a$duration <- b$duration}
		} else if (linkword %in% c('upto', 'max')) {
			# Dose 2 has freq but no qty; dose 1 has qty but no freq:
			# combine doses and max=2
			# If not, and Dose 2 has duration but nothing else:
			#     combine doses using duration from 2 (max not affected)
			# If not, and total dose 1 > dose 2, or dose 1 incomplete:
			#     use dose 2
			# If not and dose 2 is valid, combine doses
			# Otherwise keep dose 1 and max=1
			#
			# If previous dose is more than maximum or it is an
			# invalid dose, use max
			
			if (a$tot == 0) {
				# no useful info in first dose (a)
				a$max <- 'max'
				a[,c('tot', 'freq', 'qty')] <- b[,c('tot', 'freq', 'qty')]
			} else if (a$freq == 0 & a$qty > 0 & b$freq > 0 & b$qty == 0) {
				# combine doses
				a$max <- 'max'
				a$freq <- b$freq
				a$tot <- a$freq * a$qty
			} else if (b$duration > 0 & b$tot == 0) {
				a$duration <- b$duration
			} else if ((b$tot < a$tot & a$time > 0 & b$time > 0) |
				(a$time == 0 & a$tot > 0) | (b$tot > 0 & a$duration == 1) |
				(a$tot == 0 | b$tot > 0)) {
				# use dose 2
				items <- c('freq', 'qty', 'tot', 'duration')
				a[,items] <- b[,items]
				a$max <- 'max'
			} else {				
				# retain existing first dose
				a$max <- 'average'
			}
		}
		
		if (a$time == 0) {a$time <- b$time}
		a$link <- b$link
		return(a)
	}
	# End of combine2parts sub-function

	# Looping through sections, and combining as necessary
	pos <- 1
	while (pos < nrow(trial)) {
		if (trial$link[pos] == linkword) {
			trial[pos,] <- combine2parts(trial[pos,], trial[pos+1,], linkword)
			if (pos + 2 > nrow(trial)) {
				trial <- trial[1:pos,]
			} else {
				trial <- trial[c(1:pos, (pos+2):nrow(trial)),]
			}
		}
		pos <- pos + 1
	}
	
	if (nrow(trial) < nparts & noisy){
		cat('\nAfter combining', linkword, ':\n')
		print(trial)
		cat('\n')
	}

	return(trial)
}

