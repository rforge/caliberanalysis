# Interactive menu system

exploreCodes <- function(spreadsheet='Default',
	texteditor='Default', showLog = TRUE, direct = NULL){
	# A function to guide the user through exploration and
	# selection of terms for a codelist, with documentation of
	# the process
	# Arguments: spreadsheet - name of spreadsheet program (for viewing
	#                   code selections)
	#            texteditor - name of text editor program (for viewing
	#                   log file)
	#            direct - skip the initial main menu
	#                   direct = 1 to load or use existing codelist
	#                   direct = 2 to create new codelist, ignore metadata

	if (texteditor=='Default'){
		if (grepl('Ubuntu', Sys.info()['version'])){
			texteditor <- 'gedit'
		} else {
			texteditor <- ''
		}
	}
	
	loadDICT()

	# Spreadsheet will be selected automatically by the browsedict function
	if (spreadsheet=='Default'){
		spreadsheet <- NULL
	}
  
	# Close all existing sinks
	while (sink.number() > 0){
		sink()
	}
	sink(tempdir() %&% '/interactive.log', split=TRUE)
	cat('CALIBERcodelists interactive mode\nDate: ', format(Sys.time()), '\n')
	if (any(!is.na(CALIBER_DICT$category))){
		iSetDictionary()
	} else {
		# blank CALIBER_DICT, so no need for iSetDictionary()
		cat('\nSource dictionaries currently in use: ',
			paste(getdictionary(), collapse=','))
	}
	
	wantToExit <- FALSE
	while (!wantToExit) {
		# Iterate through the main menu until user asks to exit.
		wantToExit <- iMainMenu(direct)
	}
	
	cat('\n===\nEND OF INTERACTIVE SESSION')
	cat('\nThis is the log of your session. It is also an R script.
You can re-run it using the "source" function, e.g.\nsource("' %&%
		tempdir() %&% '/interactive.log", echo=TRUE)\n')
	sink()
  
  # Make text into comments and R commands into commands
  sinkToScript(tempdir() %&% '/interactive.log')
  
	if (showLog){
		if (.Platform$OS=='unix'){
			cat(texteditor %&% ' "' %&% tempdir() %&% '/interactive.log" &\n')
			system(texteditor %&% ' "' %&% tempdir() %&% '/interactive.log" &')
		} else if (.Platform$OS=='windows') {
			shell.exec(tempdir() %&% '\\interactive.log')
		}
	}
}

#### MAIN MENU ####

iMainMenu <- function(direct = NULL){
	# return TRUE when user wants to exit.
	if (is.null(direct)){
		todo <- menu(c('Use existing codelist (in memory or from file)',
    	'Create new codelist', 'Process R Markdown file',
			'Convert codelist to standard format', 'Exit'),
		title='\n===\nMAIN MENU: What would you like to do?')
	} else {
		todo <- direct
	}
  
	if (todo %in% c(0,5)){ # exit
		return(TRUE)
	} else {
		if (todo==1){
			iCodelistMenu()
			return(FALSE)
		} else if (todo==2){
			if (!is.null(direct)){
				# saving the codelist will be done in another
				# function, so exit after this
				iCreateCodelistMenu(allowExitWithoutSave = TRUE)
				return(TRUE)	
			} else {
				iCreateCodelistMenu()
				return(FALSE)
			}
		} else if (todo==3){
			iProcessRmd()
			return(FALSE)
		} else if (todo==4){
			iConvertCodelistToStandardFormat()
			return(FALSE)
		}
	}
}

#### MANIPULATING CODELISTS ####

iProcessRmd <- function(){
	cat('\nSelect Rmd file to run\n')
	rmd_file <- file.choose()
	if (!identical(rmd_file, "") & !is.null(rmd_file)){
		iShow('process("' %&% rmd_file %&% '")', do=FALSE)
		try(process(rmd_file))
	}
}

iConvertCodelistToStandardFormat <- function(){
	# Loads a codelist from file, asks for metadata and category
	# information, and saves it using the standard naming convention
	codelistName <- iLoadCodelist()
	if (codelistName != ''){
		cat('\nCodelist loaded.\n')
		print(get(codelistName, envir=.GlobalEnv))
		response <- menu(choices=c('Yes', 'No'),
			title='Is this the correct codelist?')
		if (response==1){
			iEditMetadata(codelistName)
			cat('\nPlease check that the categories are correct.\n')
			iEditCategories(codelistName)
			# Exporting codelist
			cat(
				'\nExporting to .csv and .dta files. Please enter .csv file name' %&%
				'\n(Leave blank for default filename: ' %&%
				getwd() %&% '/' %&% 
				makeCodelistFilename(get(codelistName, envir=.GlobalEnv)) %&%
				')\n')
			filename <- readline()
			if (filename == ""){
				filename <- getwd() %&% '/' %&%
					makeCodelistFilename(get(codelistName, envir=.GlobalEnv))
			}
			try(export(get(codelistName, envir=.GlobalEnv), filename))
			iShow('export(' %&% codelistName %&% ', filename="' %&%
				filename %&% '")')
			try(export(get(codelistName, envir=.GlobalEnv),
				sub('csv$', 'dta', filename)))
			iShow('export(' %&% codelistName %&% ', filename="' %&%
				sub('csv$', 'dta', filename) %&% '")')
		}
	}
}

iCodelistMenu <- function(){
  codelistName <- iFindCodelist()
  while (codelistName != ''){
    # stay in this function while a codelist is selected
    # Options: compare, combine, edit metadata, edit categories, browse
    # export, load another codelist
    codelists <- getCodelistsList()
    if (length(codelists) == 0){
      cat('\n===\nCODELISTS MENU\nNo codelists in global environment.\n')
    } else {
      cat('\n===\nCODELISTS MENU\nCodelists available:\n')
      selected <- codelists==codelistName
      codelists[selected] <- '* ' %&% codelists[selected] %&% ' [SELECTED]'
      codelists[!selected] <- '  ' %&% codelists[!selected]
      cat(paste(codelists, collapse='\n'))
    }
    cat('\n')
    todo <- menu(c('View selected codelist', #1
		'Browse and edit in spreadsheet',
		'Merge with another codelist and overwrite categories', #3
		'Edit metadata',
		'Edit categories', #5
		'Compare to an older codelist',          
		'Save to disk', #7
		'Convert to another dictionary using NHS mapping',
		'Clear from memory', #9
		'Select or load another codelist',
		'Exit to main menu'))
    if (todo %in% c(3, 6)){
      # Need to choose a second codelist
      secondCodelist <- iFindCodelist()
      if (secondCodelist != ''){
        if (todo == 6){
			# Print out comparison - do it in try in case of error
			# (e.g. if they are from different dictionaries)
        	try(print(compare(get(secondCodelist), get(codelistName))))
		} else {
			# Create a new merged codelist
			newCodelistName <- iAskName(codelistName %&%
				'_' %&% secondCodelist)
			if (todo == 3){
				iShow(newCodelistName %&% ' <- merge(' %&%
					codelistName %&% ', ' %&% secondCodelist %&% ')')
				temp <- merge(get(codelistName, envir=.GlobalEnv),
				get(secondCodelist, envir=.GlobalEnv))
			}
			# assign merged codelist to the new name
			assign(newCodelistName, temp, envir=.GlobalEnv)
			}
		}
    } else if (todo == 1){
    	# view codelist
    	cat('\nCODELIST ' %&% codelistName %&% '\n')
    	print(get(codelistName, envir=.GlobalEnv))
    } else if (todo == 4){
      # edit metadata
      iEditMetadata(codelistName)
    } else if (todo == 5){
      # edit categories
      iEditCategories(codelistName)
    } else if (todo == 2){
      # browse and edit in spreadsheet
      temp <- copy(get(codelistName))
      browseCodelist(get(codelistName))
      # Check for differences between the codelists and write
      # code for them
      temp2 <- copy(get(codelistName, envir=.GlobalEnv))
      setkey(temp, code)
   		setnames(temp, 'category', 'old_cat')
      setkey(temp2, code)
      temp3 <- temp[temp2]
      temp3[is.na(old_cat), old_cat:=0L]
      temp3[is.na(category), category:=0L]
      differences <- temp3[, old_cat != category]
      if (any(differences)){
      	cat('\nCommands for modifying codelist:\n\n> \n')
        for (i in seq_along(temp3$category)){
          if (differences[i]==TRUE){
            iShow(codelistName %&% '[code=="' %&%
				temp3$code[i] %&% '", category:=' %&%
				temp3$category[i] %&% 'L]\n# ' %&% temp3$term[i],
				extralineBefore = FALSE)
          }
        }
      }
    } else if (todo == 7){
      # export to file
      cat('\nPlease enter file name to save to\n' %&%
				'(Leave blank for default filename: ' %&%
				makeCodelistFilename(get(codelistName, envir=.GlobalEnv)) %&%
				')\n')
    	filename <- readline()
  		if (filename == ""){
  			filename <- getwd() %&% '/' %&%
  				makeCodelistFilename(get(codelistName, envir=.GlobalEnv))
  		}
      try(export(get(codelistName, envir=.GlobalEnv), filename))
      iShow('export(' %&% codelistName %&% ', filename="' %&%
				filename %&% '")')
    } else if (todo == 8) {
    	# Convert to another dictionary
    	# ICD-10 and OPCS codelists can only be converted to Read
    	fromDictionary <- getSourceDict(get(codelistName))
		if (fromDictionary == 'product'){
			cat('\nThere are no conversion procedures for product codelists.\n')
		} else {
			if (fromDictionary == 'read'){
				toDictionary <- select.list(c('icd10', 'opcs'), graphics=FALSE,
					title='Which dictionary to map to?')
			} else {
				toDictionary <- 'read'
			}
			if (toDictionary != ''){
				newCodelistName <- iAskName(codelistName %&% '_' %&% toDictionary)
				if (newCodelistName != ''){
					# Convert dictionary
					iShow(newCodelistName %&%
						' <- convert(' %&% codelistName %&%
						', toDictionary="' %&% toDictionary %&%
						'", fromDictionary="' %&% fromDictionary %&% '")')
					temp <- convert(get(codelistName, envir=.GlobalEnv),
						toDictionary=toDictionary, fromDictionary=fromDictionary)
					assign(newCodelistName, temp, envir=.GlobalEnv)
				}
			}
    	}
    } else if (todo == 9){
      # clear codelist from memory
      iShow('rm(' %&% codelistName %&% ')')
      rm(list=codelistName, envir=.GlobalEnv)
      # reset codelistName to the first of the list
      temp <- getCodelistsList()
      if (length(temp) > 0){
        codelistName <- temp[1]
      } else {
      	# No codelists available, so exit
      	codelistName <- ''
      }
    } else if (todo == 10){
      # select or load another codelist
      codelistName <- iFindCodelist()
    } else if (todo %in% c(0, 11)){
    	# Exit the while loop
    	codelistName <- ''
    }
  }
}

ifblank <- function(x){
	if (identical(x, "")){
		"blank"
	} else {
		x
	}
}

iEditMetadata <- function(codelistName){
  # Edit metadata interactively
  for (metadataName in c('Name', 'Version', 'Author', 'Date')){
    oldValue <- attr(get(codelistName), metadataName)
    newValue <- readline('New value of ' %&% metadataName %&%
      ' (currently ' %&% ifblank(attr(get(codelistName), metadataName)) %&%
      '); leave blank to keep it as it is: ')
    if (newValue != ''){
    	eval(parse(text='setMetadata(get(codelistName, .GlobalEnv),' %&%
				metadataName %&% '="' %&% newValue %&% '")'))
      iShow('setMetadata(' %&% codelistName %&% ', ' %&%
				metadataName %&% '="' %&% newValue %&% '")')
    }
  }
}

iEditCategories <- function(codelistName = NULL){
  # Edit categories interactively
  notDoneYet <- TRUE
  while (notDoneYet){
  	
  	# Get existing category table
  	if (is.null(codelistName)){
  		# use CALIBER_DICT / META
  		cattable <- retrieveCategoriesFromMETA()
  	} else {
  		cattable <- retrieveCategories(get(codelistName, envir=.GlobalEnv))
  	}
  	
  	# Create a menu out of existing categories and a few extra entries
  	if (nrow(cattable) > 0) {
  		# If any categories currently exist in the categories table
		  categorystrings <- 'Edit category ' %&% cattable$category %&% '. ' %&%
		  	cattable$shortname %&% ' (' %&% cattable$description %&% ')'
  	} else {
  		categorystrings <- character(0)
  	}
	  categorystrings <- c(categorystrings, 'Create new category',
	  	'Remove unused categories',
	  	'Finished editing categories')
	  
  	# Ask the user what they want to do (note that 0 is also exit)
  	todo <- menu(categorystrings)
		if (todo == length(categorystrings) | todo == 0){
			# exit function
			notDoneYet <- FALSE
		} else if (todo == length(categorystrings) - 1){
			# Remove unused categories from category table
			if (is.null(codelistName)){
				iShow('removeUnusedCategories(NULL)', do=TRUE)
			} else {
				iShow('removeUnusedCategories(' %&% codelistName %&% ')')
				removeUnusedCategories(get(codelistName, envir=.GlobalEnv))
			}
		} else {
			# Edit a category
			if (todo == length(categorystrings) - 2){
				# Create a new category number
				catNum <- NA
				try(catNum <- as.integer(readline('New category number: ')))
			} else {
				catNum <- cattable$category[todo]
			}
			# If a valid category number has been selected, 
			if (!is.na(catNum)){
				newShortName <- readline('New shortname: ')
				newDescription <- readline('New description (optional): ')
				# Generate first part of command
				addCatCommand <- 'addCategory(' %&% catNum %&% ', "' %&%
					newShortName %&% '", "' %&% newDescription %&% '"'
				if (is.null(codelistName)){
					# add category to META
					iShow(addCatCommand %&% ')', do=TRUE)
				} else {
					# add category to codelist
					iShow(addCatCommand %&% ', ' %&% codelistName %&% ')')
					addCategory(catNum, newShortName, newDescription,
						get(codelistName, envir=.GlobalEnv))
				}
			}
		}
	}
}

getCodelistsList <- function(){
	# Lists all the codelists in the global environment
	codelists <- sapply(objects(envir=.GlobalEnv), function(x){
		'codelist' %in% class(get(x))
	})
	names(codelists)[codelists==TRUE]
}

iFindCodelist <- function(){
	# Interactive way of selecting a codelist object in the global
	# environment or from file
	# Returns the name of the selection object selected.
	# If from file, it is loaded into the global environment.
	# If no codelist selected, return NULL.
	codelists <- getCodelistsList()
	if (length(codelists) == 0){
		# Load codelist from file
		codelistName <- iLoadCodelist()
	} else {
		codelists <- c(codelists, 'Load codelist from file')
		codelistName <- select.list(codelists, multiple=FALSE,
			graphics=FALSE, title='Which codelist to use?')
		if (codelistName=='Load codelist from file'){
			codelistName <- iLoadCodelist()
		}
	}
	return(codelistName)
}

iLoadCodelist <- function(){
	# Load a codelist from file.
	# Assign it to a new name in the global environment
	# Return empty string if no file chosen or it is not a
	# valid codelist
	mychoice <- ''
	try(mychoice <- file.choose())
	if (mychoice !=''){
		# a file has been chosen
		temp <- NULL
		try(temp <- as.codelist(mychoice))
		if (is.null(temp)){
			# no codelist found
			codelistName <- ''
		} else if (!is.codelist(temp)){
			# no codelist found
			codelistName <- ''
		} else {
			# temp is a codelist
			message(attr(temp, 'Source') %&% ' codelist with '
				%&% nrow(temp) %&% ' terms loaded from\n' %&% mychoice)
			# obtain a name for the new object
			codelistName <- iAskName(mychoice, isFilename = TRUE)
			assign(codelistName, temp, envir=.GlobalEnv)
			iShow(codelistName %&% ' <- as.codelist("' %&%
				mychoice %&% '")')
			print(temp)
		}
		codelistName
	}
	return(codelistName)
}

iAskName <- function(trial = '', isFilename = FALSE){
	if (is.null(trial)){
		trial <- ''
	}
	if (is.na(trial)){
		trial <- ''
	}
	# Remove everything before / or \\ if trial is a filename
	if (isFilename){
		trial <- gsub('.*[/\\\\]', '', trial)
	}
	# Remove termhas etc.
	trial <- gsub('termhas|codematch|dictis|AND|NOT|OR|%|\\(|\\)|"|=|\n|dictionary|read|icd10|opcs|mapStatus|None',
		'', trial)
	# Keep the first word
	trial <- strsplit(trial, ' ')[[1]][1]
	if (is.na(trial)){
		trial <- ''
	}
	# Returns a name of a new R object with no clashes
	if (trial == ''){
		myName <- readline('Please type a name for the new object: ')
		myNewName <- make.names(myName)
	} else {
		myName <- readline('The new object will be named\n' %&%
			make.names(trial) %&%
				'\n\nIf you would like to change the name, please type a new name, otherwise press ENTER: ')
		if (myName=='') {
			myName <- trial
		}
		myNewName <- make.names(myName)
	}
	while (myNewName %in% objects(envir=.GlobalEnv)){
		myNewName <- myNewName %&% '_'
	}
	if (myNewName != myName & myName != '' & myName != ' '){
		message('Name changed to ' %&% myNewName %&% ' to avoid conflicts.')
	}
	myNewName
}

iFindSelection <- function(){
	# Interactive way of selecting a selection object in the global environment
	# Returns the name of the selection object selected.
	selections <- sapply(objects(envir=.GlobalEnv), function(x){
		'selection' %in% class(get(x))
	})
	selections <- names(selections)[selections==TRUE]
	if (length(selections) == 0){
		message('No selection objects available.')
		return('')
	} else {
		return(select.list(selections, multiple=FALSE,
			graphics=FALSE, title='Which selection object to use?'))
	}
}

iAssignCat <- function(selectionName){
	# Assigns categories according to a named selection object
	# Returns the category number
	# Argument: name of a selection object in the global environment.
	cat('Please enter a positive integer category number.\nUse 0 for excluded terms.\n')
	temp <- as.integer(readline('Category number: '))
	categories <- retrieveCategories()
	if (is.na(temp)){
		cat('\nNo category number entered, so no category assigned.\n')
	} else if (temp == 0){
		# Category 0 for excluded terms
		iShow('assigncat(0, "Excluded", ' %&% selectionName %&% ')')
		assigncat(0, 'Excluded', get(selectionName, envir=.GlobalEnv))
	} else {
		if (temp %in% categories$category) {
			cat('\nThe category description is ' %&%
				categories[category==temp][, shortname] %&%
				' (' %&%	categories[category==temp][, description] %&% ')' %&%
				'\nType a new description if you want to change it,
	or leave blank to stick with this description.\n')
		} else {
			cat('\nPlease enter the category description. You can specify brief and
longer descriptions by separating them with |
e.g. MI|myocardial infarction\n')
		}
		temp2 <- readline('')
		iShow('assigncat(' %&% temp %&% ', "' %&% temp2 %&% '", ' %&%
						selectionName %&% ')')
		assigncat(temp, temp2, get(selectionName, envir=.GlobalEnv))
	}
	return(temp)
}

iEnterMetadata <- function(){
	cat('\nEntering metadata for this codelist under construction.
Please press ENTER after typing in each piece of information.\n')
	cat('\n> \n')
	iAssignMetadata('Name', "CALIBER variable name for the codelist, no spaces.")
	iAssignMetadata('Version', 'Version number')
	iAssignMetadata('Author', 'Your name')
	iAssignMetadata('Date', 'Date to write on the codelist')
	cat('\n> \n')
}


iAssignMetadata <- function(which, description){
	temp <- readline('\n' %&% which %&% ': ' %&% description %&% ' \n')
	iShow('setMetadata(' %&% which %&% '="' %&% temp %&% '")', do=TRUE)
}

iSetDictionary <- function(){
	# Initialise the dictionaries interactively
	useCodelist <- FALSE
	response <- select.list(c('read', 'icd10', 'opcs',
		'Initialise using an existing codelist'), multiple=TRUE,
		title='Preparing to reset categories. Which source dictionaries to use?',
		graphics=FALSE)
	if (length(response) > 0){
		if ('Initialise using an existing codelist' %in% response){
			codelistName <- iFindCodelist()
			if (codelistName != ''){
				useCodelist <- TRUE
			}
			response <- setdiff(response, 'Initialise using an existing codelist')
		}
		message('Resetting categories.')
		if (length(response) > 2){
			responseText <- ', c("' %&% paste(response, collapse='", "') %&% '")'
		} else if (length(response) == 0) {
			responseText <- ''
		} else {
			responseText <- ', "' %&% paste(response, collapse='", "') %&% '"'
		}
		if (useCodelist){
			iShow('setdictionary(' %&% codelistName %&%
				responseText %&% ')', do=FALSE)
			if (length(response) > 0){
				setdictionary(get(codelistName), response)
			} else {
				setdictionary(get(codelistName))			
			}
		} else {
			iShow('setdictionary(' %&% responseText %&% ')', do=TRUE)
		}
	}
}


#### CREATE CODELIST ####
# For creating a codelist

iCreateCodelistMenu <- function(allowExitWithoutSave = FALSE){
	# Creating a codelist. Note that the existing codelist does not
	# need to be destroyed - you might be working on an existing
	# partially-built codelist.

	if (allowExitWithoutSave){
		# saving and exporting is to be done in another function
		# as is entry of metadata
		savePrompt <- 'Finish and exit'
	} else {
		savePrompt <- c('Edit metadata',
			'Save/export new codelists and exit to main menu')
	}
	notDoneYet <- TRUE
	while (notDoneYet){
		# Options: Reset categories, and choose dictionary
		#            [also has an option to clear metadata]
		#          Create a new selection --> selection menu
		#          Use an existing selection --> selection menu
		#          Edit metadata
		#          Edit categories
		#          Export codelist(s) and exit to main menu
		#          Exit to main menu
		
		# Introductory message:
		# number of terms assigned to categories
		codelistText <- paste(getdictionary(), collapse=', ') %&% ' codelist'
		codelistText <- toupper(substr(codelistText, 1, 1)) %&% substr(codelistText, 2, 100)
		totalTerms <- nrow(CALIBER_DICT[dict %in% getdictionary() & category>0])
		if (totalTerms == 0){
			cat('\n' %&% codelistText %&% ' under construction currently contain(s) no terms.',
				'\n(No terms have been assigned to categories.)')
		} else {
			cat('\n' %&% codelistText %&% ' under construction contain(s)' , totalTerms, 'terms.')
		}
    
		todo <- menu(c('Reset categories and choose dictionaries', #1
			'Create a new selection', #2
			'Use an existing selection', #3
			'Assign a selection to a category', #4
			'Edit categories', #5
			'Edit codelists under construction in spreadsheet', #6
			'Compare to an older codelist', #7
 			savePrompt),
			title='\n===\nCREATE CODELIST MENU')
		if (todo == 1){
			iSetDictionary()
		} else if (todo == 2){
			# Start with a blank selection
			iSelectionMenu('')
		} else if (todo == 3){
			# Use an existing selection if there is any
			iSelectionMenu(iFindSelection())
		} else if (todo == 4){
			# Assign a selection to a category
			# If there are no selections available, create one
			selectionName <- iFindSelection()
			if (selectionName == ''){
				selectionName <- iSelectionMenu('')
			} else {
				iAssignCat(selectionName)
			}
		} else if (todo == 5){
	  	iEditCategories(NULL)
		} else if (todo == 6){
			# Browse / edit in spreadsheet
			commands <- NULL
			try(commands <- browseDICT(CALIBER_DICT[, category>0]))
			if (is.null(commands)){
				message('Unable to edit codelists')
			} else {
				iShow(commands)
			}
		} else if (todo == 7){
			# Compare with a second codelist
			secondCodelist <- iFindCodelist()
			if (secondCodelist != ''){
				# Print out comparison - do it in try in case of error
				# (e.g. if they are from different dictionaries)
				try(print(compare(get(secondCodelist),
					as.codelist(getSourceDict(get(secondCodelist))))))
			}
		} else if (todo %in% c(0, 8, 9)){
			if (!allowExitWithoutSave){
				if (todo==8){
					# enter / edit metadata in META
					iEnterMetadata()
				} else {
					# save and exit
					for (thedict in getdictionary()){
						# If there are any terms selected
						if (nrow(as.codelist(thedict)) == 0){
							message('No terms in codelist')
						} else {
							# One codelist per dictionary
							cat('\nStoring ' %&% thedict %&% ' codelist in memory\n')
							newCodelistName <- iAskName(
								META['Name'][, value] %&% '_' %&%
								switch(thedict, read='GPRD', icd10='HES', opcs='OPCS'))
							if (newCodelistName != ''){
								assign(newCodelistName, as.codelist(thedict), envir=.GlobalEnv)
								iShow(newCodelistName %&%
									' <- as.codelist("' %&% thedict %&% '")')
							}
							# Optionally export the codelist to file
							filename <- readline('To save ' %&% thedict %&%
								' codelist to disk please type a filename, otherwise leave blank and press ENTER: ')
							if (filename != ''){
								try(export(as.codelist(thedict), filename))
								iShow('export(' %&% newCodelistName %&%
									', "' %&% filename %&% '")')
							}
						}
					}
					notDoneYet <- FALSE
				}
			} else {
				# allow exit without save; 8 is exit
				notDoneYet <- FALSE
			}
		} # end if todo %in% c(0, 8, 9) 
	} # end while
}

#### SELECTIONS ####
# For handling selections

emptySelection <- function(){
	out <- rep(FALSE, nrow(CALIBER_DICT))
	class(out) <- 'selection'
	out
}

iBrowseEditSelection <- function(currentSelectionText){
	# Argument: currentSelection as selection object
	# Abort if nothing is selected.
	if (currentSelectionText!='' & currentSelectionText!='NULL'){
		oldSelection <- eval(parse(text=currentSelectionText))
		newSelection <- browseSelection(oldSelection)
		# If there are any differences, return the 
		# code for the exclusions.
		
		# Ignore terms selected in dictionaries not in use
		oldSelection[CALIBER_DICT[, !(dict %in% getdictionary())]] <- FALSE
		newSelection[CALIBER_DICT[, !(dict %in% getdictionary())]] <- FALSE
		
		# Can't add terms with this function
		newSelection[oldSelection == FALSE] <- FALSE
		
		for (theDict in getdictionary()){
			if (any((oldSelection != newSelection) &
				CALIBER_DICT$dict == theDict)){
				# If there are any deletions in this category, create a
				# command to exclude them by code
				# Create a regular expression (sanitize . in Read terms)
				regexpr <- paste(CALIBER_DICT[
					dict == theDict & oldSelection != newSelection,
					'^' %&% gsub('\\.', '\\\\\\\\.', code) %&% '$'],
					collapse='|')
				# Create a codematch expression
				codematchExpr <- '\n%AND% NOT(codematch("' %&% regexpr %&%
					'",\ndictionary="' %&% theDict %&% '", mapStatus="None"))'
				currentSelectionText <- '(' %&%
					gsub('\n', '\n ', currentSelectionText) %&%
					codematchExpr %&% ')'
			}
		}
	} else {
		message('no terms selected')
	}
	return(currentSelectionText)
}

iLimitDictionary <- function(currentSelectionText = ''){
	# Returns text for limiting to a particular dictionary
	if (length(getdictionary())==1){
		cat('Only ' %&% getdictionary() %&% ' is in use.')
		return(currentSelectionText)
	} else {
		allowableDicts <- select.list(getdictionary(), multiple = TRUE,
			title = 'Choose dictionaries to limit the selection', 
			graphics = FALSE)
		if (length(allowableDicts) == 0){
			return(currentSelectionText)
		} else {
			return('(' %&% gsub('\n', '\n ', currentSelectionText) %&%
				'\n%AND% dictis(' %&% paste('"' %&%
				allowableDicts %&% '"', collapse=', ') %&% '))')
		}
	}
}


iSelectionMenu <- function(selectionName){
	# Must save the selection if you want to keep the selection
	# at the end of this menu
	# Start off with nothing selected (empty selection) if
	#   selectionName is ''
	# Select by term or code within existing selection
	# Select by term or code and add to existing selection
	
	# Print the number of terms in selection
	
	# Add using term text
	# Select by code
	
	# Limit by term text
	# Limit by code
	# Limit to a particular dictionary (ICD-10) or (Read)
	
	# Browse and edit
	# Assign to a category
	# Save and exit to higher menu
	# Exit without saving
	notDoneYet <- TRUE
	
	if (selectionName == ''){
		cat('Starting off with empty selection')
		selectionText <- ''
		# Initial selection
		todo <- menu(c('Select by term', 'Select by code'))
		if (todo==1){
			# Put parantheses around the command to protect it and
			# make it easier to undo subsequent modifications to the
			# codelist selection code
			selectionText <- '(' %&% iSelectTerm('') %&% ')'
		} else if (todo==2){
			selectionText <- '(' %&% iSelectCode('') %&% ')'
		} else {
			notDoneYet <- FALSE # cancelling, exit function
		}
	} else {
		# Starting off with an existing selection object
		cat('Current selection:\n')
		print(get(selectionName, envir=.GlobalEnv))
		selectionText <- selectionName
	}
	
	dictString <- paste(getdictionary(), collapse='/')
	
	while (notDoneYet){
		# Loop through the menu system
		todo <- menu(c('View in console',
			'Browse in spreadsheet', #2
			'Add terms by term text', #3
			'Add terms by ' %&% dictString %&% ' code',
			'Add terms from another selection',
			'Limit selection by term text', #6
			'Limit selection by ' %&% dictString %&% ' code',
			'Limit selection using another selection',
			'Limit to one source dictionary',
			'Remove terms by term text', # 10
			'Remove terms by ' %&% dictString %&% ' code',
			'Remove terms using another selection', #12
			'Assign category to terms in selection', #13
			'Undo last operation', #14
			'Store in memory and exit to CREATE CODELIST MENU'),
			title='\n===\nCREATE SELECTION MENU') #15
		if (todo %in% c(13, 15)) {
			# exit and save selection
			notDoneYet <- FALSE
			if (todo == 13){
				cat('Storing selection in memory before assigning category.')
			}
		} else if (todo == 1){
			cat('\nCode to create selection:\n',
				gsub("get\\(\\'(\\1)\\', \\.GlobalEnv\\)", '\\1',
					selectionText), '\n')
			currSelection <- eval(parse(text=selectionText)) 
			print(currSelection)
			if (length(currSelection) > options()$datatable.print.nrows){
				cat('\nThis selection has ' %&% length(currSelection) %&%
					' terms so not all are shown above;\n' %&% 
					'choose option 2 to view the entire selection in a spreadsheet.\n')
			}
		} else if (todo == 2){
			selectionText <- iBrowseEditSelection(selectionText)
		} else if (todo == 3){
			selectionText <- iSelectTerm(selectionText, 'OR')
		} else if (todo == 4){
			selectionText <- iSelectCode(selectionText, 'OR')
		} else if (todo == 5){
			selectionText <- iCombineSelection(selectionText, 'OR')
		} else if (todo == 6){
			selectionText <- iSelectTerm(selectionText, 'AND')
		} else if (todo == 7){
			selectionText <- iSelectCode(selectionText, 'AND')
		} else if (todo == 8){
			selectionText <- iCombineSelection(selectionText, 'AND')			
		} else if (todo == 9){
			selectionText <- iLimitDictionary(selectionText)
		} else if (todo == 10){
			selectionText <- iSelectTerm(selectionText, 'ANDNOT')
		} else if (todo == 11){
			selectionText <- iSelectCode(selectionText, 'ANDNOT')
		} else if (todo == 12){
			selectionText <- iCombineSelection(selectionText, 'ANDNOT')
		} else if (todo == 14){
			# Try to undo operation by going in one bracket level
			cat('\nCurrent selection command:\n' %&%
			gsub("get\\(\\'(\\1)\\', \\.GlobalEnv\\)", '\\1', selectionText))
			# Enclose this in try in case it causes an error
			try(selectionText <- undoOne(selectionText))
			cat('\n\nAfter undoing last operation:\n' %&%
			gsub("get\\(\\'(\\1)\\', \\.GlobalEnv\\)", '\\1', selectionText))
			cat('\n\nSelection after undoing:\n')
			print(eval(parse(text=selectionText)))
		}
	}
	
	if (selectionText != '' & selectionText != 'NULL'){
		# A selection has been created and needs to be given a name
		newSelectionName <- iAskName(substr(selectionText, 1, 15))
		# Perform the selection
		assign(newSelectionName, eval(parse(text=selectionText)),
			envir=.GlobalEnv)
		# Convert get from global environment statements to
		# ordinary statements for display
		iShow(newSelectionName %&% ' <- ' %&% 
			gsub("get\\(\\'(\\1)\\', \\.GlobalEnv\\)", '\\1', selectionText))
		
		# If plan to assign category, call the assign category function
		if (todo == 13){
			# Assign category to this selection
			iAssignCat(newSelectionName)
		}
		return(invisible(get(newSelectionName)))
	} else {
		return(emptySelection())
	}
}

undoOne <- function(string){
	# Undoes one level of the operation by removing everything
	# inside the outer set of parantheses. Uses the undentation level
	# to decide which ones to remove.
	
	# If there is only one line, return NULL.
	if (grepl('\n', string)){
		# Can't undo if there are no parantheses
		if (substr(string, 1, 1)[[1]]=='('){
			strvec <- strsplit(string, '\n')[[1]]
			# Remove the first parenthesis
			strvec[1] <- substr(strvec[1], 2, nchar(strvec[1]))
			# Remove any non-indented lines
			toremove <- !(grepl('^ ', strvec)) & 
				c(FALSE, rep(TRUE, length(strvec)-1))
			strvec <- strvec[!toremove]
			
			spaces <- function(i){
				paste(rep(' ', i), collapse='')
			}
	
			if (length(strvec) >= 2){
				# Reduce the indentation by 1
				for (i in 1:length(strvec)){
					strvec[2:length(strvec)] <-
						sub('^' %&% spaces(i) %&% '([^ ])', spaces(i-1) %&% '\\1',
						strvec[2:length(strvec)])
				}
			}
			return(paste(strvec, collapse='\n'))
		} else {
			return(string)
		}
	} else {
		return('NULL')
	}
}

iShowSelection <- function(selectionExpr){
	# Argument = text which evaluates to a selection
	# It may be safer to explicitly say that the selection objects
	# should be fetched from the global environment - this will avoid
	# name conflicts
	selectionToShow <- eval(parse(text = selectionExpr))
	invisible(selectionToShow)
}

#### SELECTING BY TERM OR BY CODE OR COMBINING WITH ANOTHER SELECTION ####

iCombineSelection <- function(selectionText = '',
	combineMode = c('AND', 'OR', 'ANDNOT')){
	# Argument: selectionText = existing selection text
	#           combineMode = how to combine with existing selection
	combineMode = combineMode[1]
	otherSelection <- iFindSelection()
	if (otherSelection == ''){
		return(selectionText)
	} else {
		if (selectionText == ''){
			newSelectionText <- otherSelection
			print(get(otherSelection, envir=.GlobalEnv))
		} else {
			# newSelectionText involves indentation of previous
			# parts of the command so that it is easy to undo
			newSelectionText <- '(' %&%
				gsub('\n', '\n ', selectionText) %&% '\n' %&%
				switch(combineMode, AND='%AND% ', OR='%OR% ',
				ANDNOT='%AND% NOT(') %&%
				otherSelection %&% ')' %&% ifelse(combineMode=='ANDNOT', ')', '')
			showSelectionCombination(combineMode,
				selectionText, newSelectionText)
		}
		return(newSelectionText)
	}
}

iSelectTerm <- function(selectionText = '',
	combineMode = c('AND', 'OR', 'ANDNOT')){
	# Argument: selectionText = existing selection text
	#           combineMode = how to combine with existing selection
	combineMode = combineMode[1]
	cat('Please enter a search term. Special symbols:
			.* match any number of any characters (e.g. "stab.*ang" will match "stable angina")
			^  match beginning of phrase (e.g. "^angina" will not match "stable angina")
			|  to separate alternative strings (e.g. "heart|cardiac")
			() to set order of precedence
			(e.g. "(heart|cardi.*) disease" will match "heart disease" or "cardiac disease")\n')
	temp <- readline('Search term: ')
	if (selectionText == '' | selectionText == 'NULL'){
		newSelectionText <- 'termhas("' %&% temp %&% '")'
		print(eval(parse(text=newSelectionText)))
	} else {
		newSelectionText <- '(' %&% 
			gsub('\n', '\n ', selectionText) %&% '\n' %&%
			switch(combineMode, AND='%AND% ', OR='%OR% ', ANDNOT='%AND% NOT(') %&%
			'termhas("' %&% temp %&% '"))' %&% ifelse(combineMode=='ANDNOT', ')', '')
		showSelectionCombination(combineMode,
			selectionText, newSelectionText)
	}
	return(newSelectionText)
}

showSelectionCombination <- function(combineMode,
	selectionText, newSelectionText) {
	if (combineMode == 'AND'){
		toshow <- eval(parse(text=selectionText)) &
			eval(parse(text=newSelectionText))
		cat('Selection limited to:\n')
		print(toshow)
	} else if (combineMode == 'OR'){
		toshow <- NOT(eval(parse(text=selectionText))) &
			eval(parse(text=newSelectionText))
		cat('Additional terms added to selection:\n')
		print(toshow)			
	} else if (combineMode == 'ANDNOT'){
		toshow <- eval(parse(text=selectionText)) &
		NOT(eval(parse(text=newSelectionText)))
		cat('Terms removed from selection:\n')
		print(toshow)						
	}
}

iSelectCode <- function(selectionText = '',
	combineMode = c('AND', 'OR', 'ANDNOT'), sourceDict = ''){
	# Arguments: selectionText = existing selection text
	#            combineMode = how to combine with existing selection
	combineMode <- combineMode[1]
	if (sourceDict == ''){
		if (length(getdictionary())==1){
			sourceDict <- getdictionary()
			cat('\nSearching on ' %&% sourceDict %&% ' code.\n')
		} else {
			cat('\nWhich coding dictionary to use for search?\n')
			sourceDict <- select.list(getdictionary(), multiple=FALSE,
					graphics=FALSE)
		}
	} 
	
	if (sourceDict == ''){
		return(selectionText)
	} else {
		cat('Please enter a pattern to match. Special symbols:
				.  match any single character
				[] to enclose alternative characters (e.g. I[12]0 will match I10, I20X etc.)
				|  to separate alternative strings (e.g. RR|RF will match QRR1, QRF1 etc.)\n')
		temp <- readline('Code search pattern: ')
		if (selectionText == '' | selectionText == 'NULL'){
			newSelectionText <- 'codematch("' %&% temp %&%
				'", dictionary="' %&% sourceDict %&% '")'
			print(eval(parse(text=newSelectionText)))
		} else {
			newSelectionText <- '(' %&% 
				gsub('\n', '\n ', selectionText) %&% '\n' %&%
				switch(combineMode, AND='%AND% ', OR='%OR% ', ANDNOT='%AND% NOT(') %&%
				'codematch("' %&% temp %&% '", dictionary="' %&%
				sourceDict %&% '"))' %&% ifelse(combineMode=='ANDNOT', ')', '')
			showSelectionCombination(combineMode,
				selectionText, newSelectionText)
		}
	}
	return(newSelectionText)
}

#### UTILITIES ####

sinkToScript <- function(infile, outfile=NULL){
	# Converts a R sink file into a script, commenting
	# out everything except lines starting with > (R prompt)
	# which are left without a prompt so that it is turned into
	# a R script.
	if (is.null(outfile)){
		outfile <- infile
	}
	temp <- '# ' %&% scan(infile, what='character', sep='\n')
	temp <- sub('^# > ', '', temp)
	write(temp, outfile)
}

iShow <- function(x, do=FALSE, extralineBefore = TRUE,
	extralineAfter = TRUE){
	# show a command on screen, and optionally do it
	toShow <- gsub('\n', '\n> ', x)
	if (extralineBefore){
		toShow <- '\n> ' %&% toShow
	}
	if (extralineAfter){
		toShow <- toShow %&% '\n> '
	}
	cat('\n> ' %&% toShow %&% '\n')
	if (do==TRUE){
		eval(parse(text=x))
	}
}


