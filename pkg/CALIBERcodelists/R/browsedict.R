# browseTerms.selection and browseTerms.codelist allow
# the categories in a selection or codelist to be edited
# without affecting the master dictionary.

browseSelectionNotDICT <- function(x, spreadsheet=NULL){
	# Browse a selection without making any changes to CALIBER_DICT
	# Shows the selection in a spreadsheet, allows editing, and
	# returns the edited selection. The calling function should
	# compare the original and edited selections to devise code
	# for performing the conversion automatically.
	# Argument: x - a selection object
	# Browse the terms in a selection
	message('Opening the selection in a spreadsheet.
Please delete terms you want to exclude, then save the spreadsheet with the same name.')
		
	# include medcode and events columns
	write.csv(CALIBER_DICT[x == TRUE & dict %in% getdictionary(),
		list(dict, medcode,
		code = ifelse(dict == 'read', 'Read ' %&% code, 
		ifelse(dict == 'icd9', 'ICD9 ' %&% code, code)),
		term, events, include = TRUE)][
		order(dict, code)], file=tempdir() %&% '/browsedict.csv',
		row.names = FALSE)
	
	NEW <- browseLoad(spreadsheet)
	if (is.null(NEW)){
		message('Selection not reloaded; no changes made.')
		return(x)
	} else {
		NEW[dict=='read', code:=medcodeToReadCode(medcode)]
		
		# Find out which terms are in the new selection
		if ('include' %in% names(NEW)){
			NEW <- copy(NEW[as.character(include) %in% c('1', 'TRUE',
				'True', 'true', 'T', 'yes', 'YES', 'Yes', 'Y', 'y')])
			NEW[, include:=NULL]
		}
		
		# If no 'include' column, assume all terms are included
		NEW[, include:=TRUE]
		
		setkey(NEW, dict, code)
		setDictKey()
		out <- NEW[CALIBER_DICT][, include]
		out[is.na(out)] <- FALSE
		
		# Ignore changes in icdhead, as selection objects are defined
		# for the expanded form of ICD-10 codelists
		if (all(x[CALIBER_DICT$dict %in% getdictionary()] == 
				out[CALIBER_DICT$dict %in% getdictionary()])){
			message('No changes to selection.')
		} else {
			message('Terms excluded from selection:')
			message(show(CALIBER_DICT[x & NOT(out) & dict %in% getdictionary(),
				list(dict, code, term)]))
		}
		
		# Return the updated selection
		return(as.selection(out))
	}
}

browseCodelist <- function(x, spreadsheet=NULL){
	# Edits the categories within a codelist
	# No effect on CALIBER_DICT
	# If codes are deleted, their category is set to zero.
	# Metadata is not edited in this way
	# Arguments: x - a codelist
	# Returns the modified codelist (x)

	message('Opening the codelist in a spreadsheet.
Any changes made in the spreadsheet can be transferred onto the original
codelist in the R workspace. Terms deleted will have their
category set to zero.')
	tmp <- copy(x)
	if (attr(tmp, 'Source') == 'GPRD'){
		# Ensure that Read codes are not mangled
		tmp[, code:='Read ' %&% code]
	} else if (attr(tmp, 'Source') == 'GPRDPROD'){
		# Encode Multilex codes
		tmp[, code:='Multilex ' %&% code]
	}
	write.csv(tmp[order(category, code)],
		file=tempdir() %&% '/browsedict.csv', row.names=FALSE)
	NEW <- browseLoad(spreadsheet)
	# If NEW was not reloaded, it will be NULL
	if (is.null(NEW)){
		message('Codelist not reloaded; no changes made.')
	} else {
		if ('medcode' %in% names(NEW)){
			# This is the only part of the function that requires
			# the master dictionaries, so check now that CALIBER_DICT
			# is loaded
			loadDICT()
			NEW[, readcode:=medcodeToReadCode(medcode)]
		}
		if ('prodcode' %in% names(NEW)){
			# Remove 'Multilex' text
			NEW[, code:=sub('^Multilex ', '', code)]
		}
		setnames(NEW, 'category', 'new_cat')
		if ('medcode' %in% NEW){
			setkey(NEW, medcode)
			setkey(x, medcode)
		} else {
			setkey(NEW, code)
			setkey(x, code)
		}
		
		newCat <- as.integer(NEW[x][, new_cat])
		newCat[is.na(newCat)] <- 0L
		x[, category:=newCat]
	}
	x
}

browseSelection <- function(x=NULL, spreadsheet=NULL, update_cats=FALSE){
	# If x is NULL, attempt to coerce it to a selection.
	# Returns instructions for changing the categories by Read/ICD-10/OPCS code.
	# Argument: x - a selection or something to coerce to a selection
	#                  (e.g. a character string) or NULL, in which case
	#                  browse all non-NA categories
	#           update_cats - if TRUE, update categories in CALIBER_DICT.
	#                  if FALSE, just edit the current selection

	# Check that master dictionaries are loaded.
	loadDICT()
	
	if (is.null(x)){
		x <- as.selection(CALIBER_DICT[, dict %in% getdictionary() & category>0])
	}
	if (!is.selection(x)){
		x <- as.selection(x)
	}
	if (!is.selection(x)){
		stop('Cannot coerce x to a selection')
	}
	if (all((x & dictis(getdictionary()))==FALSE)){
		# (ignore whether any terms are selected outside dictionaries in use)
		message('no terms selected')
		if (!update_cats){
			return(x)
		}
	} else {
		if (update_cats) {
			return(browseDICT(x, spreadsheet=spreadsheet))
		} else {
			return(browseSelectionNotDICT(x, spreadsheet=spreadsheet))
		}
	}
}
		
browseDICT <- function(x, spreadsheet=NULL){
	# browse a selection of CALIBER_DICT
	message('Opening the codelist(s) under construction in a spreadsheet.
If you want to make changes to the categories, edit the spreadsheet and save
it with the same filename. Remove codes by setting the category to 0.\n')
	# Prefix Read codes by the word 'Read' to stop Excel from mangling them.
	write.csv(CALIBER_DICT[x==TRUE & dict %in% getdictionary(),
		list(dict, medcode,
		code=ifelse(dict=='read', 'Read ' %&% code, code),
		term, events, category)][
		order(dict, code)], file=tempdir() %&% '/browsedict.csv', row.names=FALSE)

	NEW <- browseLoad(spreadsheet)
	if (is.null(NEW)){
		message('Selection not reloaded; no changes made.')
	} else {
		NEW[dict=='read', code:=medcodeToReadCode(medcode)]
		setnames(NEW, 'category', 'new_cat')
		setkey(NEW, dict, code)
		setDictKey()
		
		# Create a vector newCat with the new categories
		newCat <- NEW[CALIBER_DICT][, new_cat]
		# Keep the existing category if newCat is missing
		newCat <- as.integer(ifelse(is.na(newCat), CALIBER_DICT[, category], newCat))
		oldCat <- CALIBER_DICT[, category]
		# Update CALIBER_DICT with the new categories
		CALIBER_DICT[, category:=newCat]
		
		# Use the code of -2 for missing, to prevent problems with
		# missing category numbers when comparing
		oldCat[is.na(oldCat)] <- -2L
		newCat[is.na(newCat)] <- -2L
	
		# Create a vector of all categories currently in use
		# (ignore category -2)
		newCatList <- setdiff(unique(newCat[!is.na(newCat)]), -2L)
		
		# Update categories in META, as per temp
		# Add categories to categories table if necessary
		categories <- retrieveCategories()
		if (!all(newCatList %in% categories$category)){
			# For each new category not in the categories table ...
			for (eachNewCat in newCatList[
				!(newCatList %in% categories$category)]){
				addCategory(eachNewCat)
			}
		}
	
		# Returns a list of commands for changing from old to new
		# Not using compare because the codes may come from more
		# than one source dictionary, in which case as.codelist would not work
		
		# Output character vector - initialise to a size which is
		# larger than necessary
		out <- rep(NA_character_, length(newCatList) * 6)
		i <- 1
		
		for (thecat in newCatList){
			for (theDict in c(ALLDICTS, 'icdhead', 'icd9head')){
				changed <- newCat == thecat & newCat != oldCat &
					CALIBER_DICT[, dict] == theDict
				if (sum(changed) > 0){
					# Command to change categories
					out[i] <- '\n' %&% showAssigncat(thecat, theDict,
						CALIBER_DICT[changed, code])
					
					# Print list of changed codes as comments
					TMP <- data.table(dict=theDict,
						code=CALIBER_DICT[changed, code],
						term=CALIBER_DICT[changed, term])
					setkey(TMP, dict, code)
					out[i+1] <- paste('# ' %&% TMP[, code] %&% ' ' %&%
						truncateChar(TMP[, term], getOption('width') - 12),
						collapse='\n')
					i <- i + 2
				}
			}
		}
		message('Updated categories for ' %&%
			sum(newCat!=oldCat & newCat!=-2) %&% ' terms in master dictionary')
		invisible(paste(out[!is.na(out)], collapse='\n'))
	}
}

medcodeToReadCode <- function(medcodes){
	# Converts a vector of medcodes to Readcodes
	# Requires that CALIBER_DICT is loaded
	
	loadDICT()
	TEMP <- data.table(medcode=medcodes,
		.order=1:length(medcodes), dict='read')
	setkey(CALIBER_DICT, dict, medcode)
	setkey(TEMP, dict, medcode)
	out <- CALIBER_DICT[TEMP][, list(code, .order)][order(.order)][, code]
	# Reset dictionary key
	setDictKey()
	out
}


browseLoad <- function(sprog='soffice'){
	# Loads the selection, dictionary or section of CALIBER_DICT
	
	if (is.null(sprog)){
		if (META['spreadsheet'][,value] != ''){
			# Use previously stored program name
			sprog <- META['spreadsheet'][,value]
		} else if (.Platform$OS=='unix'){
			# Default libreoffice / 
			# OpenOffice for Unix platforms
			sprog <- 'soffice'
		}
	} else {
		# Store this as the default spreadsheet program for the session
		META['spreadsheet', value:=sprog]
	}
	
	if (.Platform$OS=='unix'){
		system(sprog %&% ' ' %&% tempdir() %&% '/browsedict.csv')		
	} else if (.Platform$OS=='windows') {
		shell.exec(tempdir() %&% '\\browsedict.csv')
	}
	
	toReload <- menu(c('Load spreadsheet and accept changes', 'Ignore changes'))
	if (toReload == 1){
		reload <- NULL
		try(reload  <- importCSVcodelist(tempdir() %&% '/browsedict.csv', as.is=TRUE))
		unlink(tempdir() %&% '/browsedict.csv')
		# reload, and ignore blank lines
		if (!is.null(reload)){
			reload <- reload[!is.na(reload$code) | !is.na(reload$term),]
			reload[, code:=sub('ICD9 |Read ', '', code)]
		}
		return(reload)
	} else {
		unlink(tempdir() %&% '/browsedict.csv')
		return(NULL)
	}
}
