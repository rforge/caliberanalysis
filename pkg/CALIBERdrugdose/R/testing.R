# Contains code for loading dictionaries from 
# spreadsheet and exporting to spreadsheet,
# also for testing the algorithm using the
# pre-converted texts, and identifying differences in
# interpretation between different version of the 
# algorithm, for checking.

# Dictionaries are replaced
# first
# singlewords
# second

# 'singlewords' dictionary is just word and replacement, can be 
# a character vector with named elements.
# Format in Excel sheet: words, replacement

# Modified 12 Apr 2012:
# 'second' dictionary renamed 'patterns' and now includes
# all pattern recognition for attributes
# 'first' replaced by 'multiwords' which is
# different from singlewords because terms are ordered
# and intended to be used sequentially.

# Re-written 12 April 2012 so that the three types of dictionary
# have separate code:
# singlewords, multiwords, patterns

# singlewords has the following columns:
# words, replacement

# multiwords has the following columns:
# words, replacement, comments, order

# 'pattern' dictionary has the following columns:
# words -- words to replace
# qty -- dose quantity
# units -- this can also be the replacement text e.g. four=4
# freq -- freq and qty must be analysed separately for liquid
#					doses in mL
# tot -- total dose per time period
# priority -- priority for dose units
# max -- whether max or average dose 1=max, 2=average
# time -- time period over which dose applies, in days
# change -- 0=no change, 1=first, 2=second
# choice -- 1=choice, 2=when needed
# doubledose -- if 'each eye' etc. then double the dose.
# duration -- only used in a few cases
# category -- not in the Excel version; included here so that
#				 all the contents of the Excel table can be stored in R
# comments -- also not in the Excel version
# order -- can be used to re-order entries in the Excel file.
#				 If this column has missing or duplicate entries,
#				 it is ignored.

testdoseconvert <- function(text = NULL, infile = NULL,
	outfile = NULL, outfileerrors = NULL, singlewords = NULL,
	multiwords = NULL, patterns = NULL, uselookups = FALSE,
	lookups = NULL, customlookups = NULL,	noisy = TRUE,
	tolerance = 0.001, ...){
	# Arguments
	# infile = input file, should contain text and ideally textid;
	#   customlookups if text and infile are both NULL, lookups if
	#   customlookups is NULL
	# outfile, outfileerrors = output files, none if NULL
	# singlewords, multiwords, patterns
	# uselookups = whether to use direct lookups
	# noisy = whether to print to console
	# tolerance = how close the daily doses have to be
	#   (to account for rounding)
	
	if (is.null(text)){
		if (!is.null(infile)){
			A <- fread(infile)
			if ('daily_dose' %in% colnames(A)){
				checkdailydose <- TRUE
				if (!('correct_dose' %in% colnames(A))){
					setnames(A, 'daily_dose', 'correct_dose')
				}
			}
		} else if (!is.null(customlookups)){
			A <- importLookups(customlookups)
			checkdailydose <- TRUE
			setnames(A, 'daily_dose', 'correct_dose')
		} else if (!is.null(lookups)){
			A <- importLookups(lookups)
			checkdailydose <- TRUE
			setnames(A, 'daily_dose', 'correct_dose')
		} else {
			A <- data.table(text = '', textid = 1)
			checkdailydose <- FALSE
		}
	} else {
		A <- data.table(text = text, textid = 1)
		checkdailydose <- FALSE
	}

	# Prepare input file
	if (ncol(A) == 1){
		setnames(A, 'text')
		A[, textid := 1:.N]
	} else if ('text' %in% colnames(A)){
		if (!('textid' %in% colnames(A))){
			A[, textid := 1:.N]
		}
	} else if ('words' %in% colnames(A)){
		setnames(A, 'words', 'text')
		if (!('textid' %in% colnames(A))){
			A[, textid := 1:.N]
		}
	} else {
		stop("infile must have a 'text' column")
	}
	
	if (noisy){
		# Analyse one dose at a time
		cat('\n\nAnalysing ' , A$text[1], '\n')
		B <- doseconvert(A$text[1], A$textid[1],
			singlewords = singlewords, multiwords = multiwords,
			patterns = patterns, uselookups = uselookups,
			lookups = lookups, customlookups = customlookups,
			noisy = TRUE, ...)
		print(B)
		if (nrow(A) > 1){
			for (i in 2:nrow(A)){
				cat('\n\nAnalysing ', A$text[i], '\n')
				B <- rbind(B, doseconvert(A$text[i], A$textid[i],
					singlewords = singlewords, multiwords = multiwords,
					patterns = patterns, uselookups = uselookups,
					lookups = lookups, customlookups = customlookups, ...))
				print(B[i, ])
			}
		}
	} else {
		# Analyse multiple doses simultaneously
		B <- doseconvert(A$text, A$textid, singlewords = singlewords,
			multiwords = multiwords, patterns = patterns,
			uselookups = uselookups, lookups = lookups,
			customlookups = customlookups, ...)
	}
	
	suppressWarnings(B[, text := NULL])
	A[, textid := as.numeric(textid)]
	setkey(A, textid)
	B[, textid := as.numeric(textid)]
	setkey(B, textid)
	
	if (noisy & checkdailydose){
		cat('\nIncorrect results:\n')
		print(B[A][abs(correct_dose - daily_dose) > tolerance,
			list(textid, text, freq, qty, time, tot, daily_dose, correct_dose)])
	}
	
	# Write output to file
	if (!is.null(outfile)){
		write.csv(B[A], outfile, row.names = FALSE)
	}
	
	if (checkdailydose){
		if (!is.null(outfileerrors)){
			write.csv(B[A][abs(correct_dose - daily_dose) > tolerance,
				list(textid, text, freq, qty, time, tot, daily_dose, correct_dose)],
				file = outfileerrors, row.names = FALSE)		
		}
	}
	
	# Return the interpreted dataset
	invisible(B[A])
}

test <- function(instring,
	folder = "/home/anoop/Dropbox/Rpackages/CALIBERdrugdose_scratch/dicts/",
	...){
	# Load dictionaries
	cat('\nLoading dictionaries')
	singlewords <- importSinglewords(
		paste(folder, 'singlewords.csv', sep = '/'))
	multiwords <- importMultiwords(
		paste(folder, 'multiwords.csv', sep = '/'))
	patterns <- importPatterns(
		paste(folder, 'patterns.csv', sep = '/'))
	# Interpret the string in noisy mode
	interpret(instring, singlewords = singlewords,
		multiwords = multiwords, patterns = patterns,
		noisy = TRUE, ...)
}
