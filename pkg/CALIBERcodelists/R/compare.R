compare <- function(oldlist, newlist = NULL, expandICD10 = TRUE,
	expandICD9 = TRUE){
	# Returns a character vector describing the differences between
	# oldlist and newlist, or between oldlist and the current selection
	# in CALIBER_DICT. This is always called by the export function
	# if any codelists have been re-written; output is stored in
	# the message field of META so that it can subsequently be displayed 
	# in the R window rather than in the HTML unless it is explicitly
	# called in the Rmd document.
	# Returns a codelistComparison object, a list containing the comparisons
	# message and instructions for updating the master dictionary to the
	# new codelist if it contained the old codelist.
	# Arguments: oldlist - codelist to compare against
	#            newlist - new codelist
	#            expandICD10 - whether ICD10 codelists should be compared
	#                      in the expanded format (i.e. with individual
	#                      4-character codes rather than 3-character
	#                      headings.
	#            expandICD9 - whether ICD9 codelists should be compared
	#                      in the expanded format

	if (!is.codelist(oldlist)){
		stop('Argument 1 (oldlist) must be a codelist')
	}

	sourceDict <- getSourceDict(oldlist)
	if (is.null(newlist)){
		newlist <- as.codelist(sourceDict)
	} else {
		if (attr(newlist, 'Source') != attr(oldlist, 'Source')){
			stop('Both codelists must have the same source dictionary,' %&% 
				'\nand this must be specified in the metadata')
		}
	}
	
	# Ensure that ICD9 and ICD10 codelists are expanded if necessary
	if ((sourceDict == 'icd10' & expandICD10 == TRUE) |
		(sourceDict == 'icd9' & expandICD9 == TRUE)){
		if (!isExpandedCodelist(newlist)){
			newlist <- copy(expandCodelist(newlist))
		}
		if (!isExpandedCodelist(oldlist)){
			oldlist <- expandCodelist(oldlist)
		}
	}

	if (sourceDict=='read'){
		newl <- copy(newlist[, list(medcode, code, category, term)])
		oldl <- copy(oldlist[, list(medcode, code_old = code,
			cat_old = category, term_old = term)])
		setkey(newl, medcode)
		setkey(oldl, medcode)
	} else if (sourceDict=='product'){
		newl <- copy(newlist[, list(prodcode, code, category, term)])
		oldl <- copy(oldlist[, list(prodcode, code_old = code,
			cat_old = category, term_old = term)])
		setkey(newl, prodcode)
		setkey(oldl, prodcode)
	} else {
		newl <- copy(newlist[, list(code, category, term)])
		oldl <- copy(oldlist[, list(code, code_old = code,
			cat_old = category, term_old = term)])
		# Need to add a code_old column to match Read codelists
		# (although as matching is done by code it is the same as code)
		setkey(newl, code)
		setkey(oldl, code)
	}

	if (nrow(newl)==0){
		message('No selected terms in new list')
		comparison <- oldl
		comparison[, term:='']
		comparison[, category:=0L]
		if (sourceDict=='read'){
			comparison[, code:='']
		}
	} else if (nrow(oldl)==0){
		message('No selected terms in old list')
		comparison <- newl
		comparison[, term_old:='']
		comparison[, cat_old:=0L]
		if (sourceDict=='read'){
			comparison[, code_old:='']
		}
	} else {
		comparison <- merge(oldl, newl, all=TRUE)
	}
	
	comparison[is.na(cat_old), cat_old:=0L]
	comparison[cat_old < 0, cat_old:=0L]
	comparison[is.na(category), category:=0L]
	comparison[category < 0, category:=0L]

	nowExcluded <- comparison[, (category==0 & cat_old>0)]
	nowIncluded <- comparison[, (category>0 & cat_old==0)]
	changeCat <- comparison[, (cat_old!=category & cat_old>0 & category>0)]

	# Comparison of attributes (except timestamp)
	attrFields <- c('Name', 'Version', 'Date', 'Author')
	attr_comparison <- data.table(field=attrFields,
		old=as.character(sapply(attrFields, function(x){attr(oldlist, x)})),
		new=as.character(sapply(attrFields, function(x){attr(newlist, x)})))
	attr_comparison[, same:=(old==new)]
	
	# Comparison of categories in category table
	oldcats <- copy(attr(oldlist, 'Categories'))
	oldcats[is.na(description), description:=shortname]
	oldcats[description=='', description:=shortname]
	setnames(oldcats, 'shortname', 'old_shortname')
	setnames(oldcats, 'description', 'old_description')
	setkey(oldcats, category)

	newcats <- copy(attr(newlist, 'Categories'))
	newcats[is.na(description), description:=shortname]
	newcats[description=='', description:=shortname]
	setnames(newcats, 'shortname', 'new_shortname')
	setnames(newcats, 'description', 'new_description')
	
	cattable_comparison <- merge(oldcats, newcats, by='category')
	cattable_comparison[is.na(old_shortname), old_shortname:='']
	cattable_comparison[is.na(new_shortname), new_shortname:='']
	cattable_comparison[is.na(old_description), old_description:='']
	cattable_comparison[is.na(new_description), new_description:='']
	cattable_comparison[, same_shortname:=(old_shortname==new_shortname)]
	cattable_comparison[, same_description:=(old_description==new_description)]

	instr <- ''
	message <- c('\n===\nDifferences between old and new ' %&% sourceDict %&%
		 ' codelists\n', textTotalTerms(nowExcluded) %&% ' excluded\n')
	if (sum(nowExcluded)>0){
		instr <- instr %&% '## To exclude old terms\n' %&%
			'assigncat(0, "Excluded", dictionary="' %&%
			sourceDict %&% '", codes=c(' %&%
			paste('"' %&% comparison[nowExcluded][,code_old] %&% '"',
				collapse=', ') %&% '))\n'
		if (sourceDict=='read'){
			message <- c(message,
				capture.output(printTerms(comparison[nowExcluded,
				list(medcode, code_old, term=term_old)][order(code_old)])))
		} else {
			message <- c(message,
				capture.output(printTerms(comparison[nowExcluded,
				list(code, term=term_old)][order(code)])))
		}
	}

	message <- c(message, '', textTotalTerms(nowIncluded, 'additional ') %&%
		' included\n')
	if (sum(nowIncluded)>0){
		instr <- instr %&% '\n##\n## To include new terms\n' %&%
			paste('assigncat(' %&% 
			comparison[nowIncluded][,category] %&%
			', dictionary="' %&% sourceDict %&% '", codes="' %&%
			comparison[nowIncluded][,code] %&% '")',
			collapse='\n')
		message <- c(message,
			capture.output(printTerms(comparison[nowIncluded,
			list(code, term, category)][order(category, code)])))
	}

	message <- c(message, '', textTotalTerms(changeCat) %&%
		' changed categories\n')
	if (sum(changeCat)>0){
		instr <- instr %&% '\n##\n## To update categories\n' %&%
			paste('assigncat(' %&% 
			comparison[changeCat][,category] %&%
			', dictionary="' %&% sourceDict %&% '", codes="' %&%
			comparison[changeCat][,code] %&% '")',
			collapse='\n')
		message <- c(message,
			capture.output(printTerms(comparison[changeCat,
			list(code, term, category, cat_old)][order(category, code)])))
	}
	
	out <- list(same_terms=(!any(nowIncluded | nowExcluded | changeCat)),
		same_attr=all(attr_comparison$same),
		same_cattable=all(cattable_comparison$same_shortname &
			cattable_comparison$same_description),
		cattable_comparison=cattable_comparison,
		attr_comparison=attr_comparison,
		terms_comparison=comparison,
		message=message,
		instructions=instr)
	out$identical <- out$same_terms & out$same_attr & out$same_cattable
	class(out) <- 'codelistComparison'
	out
}

print.codelistComparison <- function(x, ...){
	# Print S3 method for a codelistComparison object
	cat(paste(x$message, collapse='\n'))
	invisible(x$message)
}

textTotalTerms <- function(thingToSum, extraText=''){
	# Returns the text 'X terms' or '1 term'
	# Argument: thingToSum - vector / matrix to sum
	#           extraText - additional text to put between the number and 'term'
	total <- sum(thingToSum, na.rm=TRUE)
	if (total==1){
		'1 ' %&% extraText %&% 'term'
	} else if (total==0){
		'No ' %&% extraText %&% 'terms'
	} else {
		total %&% ' ' %&% extraText %&% 'terms'		
	}
}
