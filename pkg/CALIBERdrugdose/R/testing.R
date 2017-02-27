# Contains code for testing dose conversion program

testdoseconvert <- function(text = goldstandard$text,
	textid = NULL, simplify = TRUE, singlewords = NULL,
	multiwords = NULL, patterns = NULL, maxifchoice = TRUE,
	usebuiltinlookups = FALSE, customlookups = NULL, cores = 1,
	noisy = ifelse(length(text) == 1, TRUE, FALSE),
	goldstandard = NULL){
	# Arguments:
	# Similar to doseconvert but with additional argument goldstandard
	# goldstandard is a dataset containing textid and interpreted
	# results, against which the results can be compared. 
	# Detailed output ('noisy') is shown by default if only one text is
	# being analysed.
	# Output is a combination of new interpreted result and goldstandard
	# with outcome field outcome = 'actual', 'intended' or 'correct'
	# All errors are also printed to the debug window
	
	tolerance = 0.0001 # tolerance for numerical value comparison

	if (is.null(textid)){
		textid <- seq_along(text)
	}
	INPUT <- data.table(text = text, textid = textid)
	if (noisy){
		# Analyse one dose at a time
		cat('\n\nAnalysing ' , INPUT$text[1], '\n')
		OUTPUT <- doseconvert(INPUT$text[1], INPUT$textid[1],
			singlewords = singlewords, multiwords = multiwords,
			patterns = patterns, usebuiltinlookups = usebuiltinlookups,
			customlookups = customlookups, noisy = TRUE,
			simplify = simplify)
		print(OUTPUT)
		if (nrow(INPUT) > 1){
			for (i in 2:nrow(INPUT)){
				cat('\n\nAnalysing ', INPUT$text[i], '\n')
				OUTPUT <- rbind(OUTPUT, doseconvert(INPUT$text[i], INPUT$textid[i],
					singlewords = singlewords, multiwords = multiwords,
					patterns = patterns, usebuiltinlookups = usebuiltinlookups,
					customlookups = customlookups, noisy = TRUE,
					simplify = simplify))
				print(OUTPUT[i, ])
			}
		}
	} else {
		# Analyse multiple doses simultaneously
		OUTPUT <- doseconvert(INPUT$text, INPUT$textid,
			singlewords = singlewords, multiwords = multiwords,
			patterns = patterns, usebuiltinlookups = usebuiltinlookups,
			simplify = simplify, customlookups = customlookups)
	}
	
	suppressWarnings(OUTPUT[, text := NULL])
	INPUT[, textid := as.numeric(textid)]
	setkey(INPUT, textid)
	OUTPUT[, textid := as.numeric(textid)]
	setkey(OUTPUT, textid)
	
	# If checking against reference
	if (is.null(goldstandard)){
		OUTPUT$outcome <- 'actual'
		return(OUTPUT)
	} else {
		# Compare output with goldstandard and return the comparison
		REFERENCE <- as.data.table(goldstandard)
		if (!('textid' %in% names(REFERENCE))){
			# Add a column for textid
			REFERENCE[, textid := textid]
		}
		if ('order' %in% names(OUTPUT)){
			# Multi-row dosage interpretations
			if (!('order' %in% names(REFERENCE))){
				# Add a column for order
				REFERENCE[, order := 1]
			}
			setkey(OUTPUT, textid, order)
			setkey(REFERENCE, textid, order)
		} else {
			setkey(OUTPUT, textid)
			setkey(REFERENCE, textid)
		}
		
		# Transfer initial text to output
		OUTPUT[, text := REFERENCE[OUTPUT][, text]]

		for (i in 1:nrow(OUTPUT)){
			# Row by row comparison
			OUTPUT[i, outcome := ifelse(
				abs(OUTPUT[i, qty] - REFERENCE[i, qty]) < tolerance &
				OUTPUT[i, units] == REFERENCE[i, units] &
				abs(OUTPUT[i, freq] - REFERENCE[i, freq]) < tolerance &
				abs(OUTPUT[i, tot] - REFERENCE[i, tot]) < tolerance &
				OUTPUT[i, max] == REFERENCE[i, max] &
				abs(OUTPUT[i, time] - REFERENCE[i, time]) < tolerance &
				OUTPUT[i, change] == REFERENCE[i, change] &
				OUTPUT[i, choice] == REFERENCE[i, choice] &
				abs(OUTPUT[i, duration] - REFERENCE[i, duration]) < tolerance &
				abs(OUTPUT[i, daily_dose] - REFERENCE[i, daily_dose]) < tolerance,
				'correct', 'actual')]
		}
		
		REFERENCE[, outcome := OUTPUT[REFERENCE][, outcome]]
		# Mark texts with incorrect interpretation in the reference table
		REFERENCE[outcome == 'actual', outcome := 'intended']
		if (any(REFERENCE[, outcome] == 'intended')){
			OUTPUT <- rbind(REFERENCE[outcome == 'intended'],
				OUTPUT, fill = TRUE)
		}
		OUTPUT[, outcome := factor(outcome, levels = c('correct',
			'actual', 'intended'))]
		OUTPUT <- OUTPUT[order(textid, order, outcome)]
		return(OUTPUT)
	}
}
