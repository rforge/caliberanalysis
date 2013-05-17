# Functions for the selection class

selection <- function(x=NULL, category=NULL, categories=NULL){
	# Coerces x to a selection
	# can extract a selection from CALIBER_DICT with particular categories
	
	loadDICT()
	if (is.null(categories)){
		categories <- category
	}
	rm(category) # to avoid name conflicts

	if (is.null(x)){
		if (is.null(categories)){
			# all terms with a non-zero, nonmissing category
			out <- CALIBER_DICT[, category>0]
		} else {
			out <- CALIBER_DICT[, category %in% categories]
		}		
		out[is.na(out)] <- FALSE
		class(out) <- 'selection'
	}	else if (is.codelist(x)){
		out <- codelistToSelection(x, categories=categories)
	} else 	if (is.character(x)){
		out <- termhas(x)
	} else if (length(x)==nrow(CALIBER_DICT)){
		out <- as.logical(x)
		out[is.na(out)] <- FALSE
		class(out) <- 'selection'
	}
	out
}

as.selection <- function(x=NULL, ...){
	if (is.selection(x)){
		return(x)
	} else {
		return(selection(x, ...))
	}
}

is.selection <- function(x){
	# Returns whether x is a selection object
	if ('selection' %in% class(x)){
		return(TRUE)
	} else {
		return(FALSE)
	}
}

codelistToSelection <- function(x, categories=NULL){
	# Convert a codelist into a selection object
	# Add a dictionary column
	codelist <- copy(x)
	codelist$dict <- getSourceDict(x)
	if (is.null(categories)){
		codelist$select <- TRUE
	} else {
		codelist$select <- codelist$category %in% categories		
	}
	setkey(codelist, dict, code)
	setDictKey()
	out <- codelist[CALIBER_DICT][, select]
	out[is.na(out)] <- FALSE
	class(out) <- 'selection'
	out
}

print.selection <- function(x, ...){
	printSelection(x, ...)
}

printSelection <- function(x, ...){
	# Prints the selected terms in the form of a codelist
	whichdicts <- getdictionary()
	if (length(whichdicts)==0){
		whichdicts <- ALLDICTS
	}
	if ('read' %in% whichdicts){
		cat('\nRead terms:\n')
		printTerms(CALIBER_DICT[x & dict=='read', list(medcode, code, term, events)])
	}
	if ('icd10' %in% whichdicts){
		cat('\nICD-10 terms:\n')
		printTerms(CALIBER_DICT[x & dict=='icd10', list(code, term)])
	}
	if ('opcs' %in% whichdicts){
		cat('\nOPCS terms:\n')
		printTerms(CALIBER_DICT[x & dict=='opcs', list(code, term)])
	}
	invisible(x)
}

printTerms <- function(x, ...){
	# Prints the table portion of a codelist, using the maximum
	# available width

	# Calculate the width without term column
	temp <- lapply(x[1, setdiff(names(x), 'term'), with=FALSE],
				 function(z){nchar(as.character(z))})
	# Calculate the total width without term column
	termwidth <- max(getOption('width') - 6 - 
		sum(pmax(nchar(names(temp)), unlist(temp))) - length(temp), 20)
	x2 <- copy(x)
	x2[, term:=truncateChar(term, termwidth)]
	show(x2)
}

length.selection <- function(x){
	# Number of terms in a selection
	sum(x & dictis(getdictionary()))
}
