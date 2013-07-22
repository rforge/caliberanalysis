retrieveCategories <- function(codelist = NULL){
	# Retrieve category table from a codelist or META
	if (is.null(codelist)){
		retrieveCategoriesFromMETA()
	} else {
		temp <- copy(attr(codelist, 'Categories'))
		if (is.null(temp)){
			temp <- data.table(category=integer(0),
				shortname=character(0), description=character(0))
		} else {
			# Remove categories with missing category number
			if (any(is.na(temp$category))){
				temp <- temp[!is.na(category)]
			}
		}
		# Sort by category
		setkey(temp, category)
		return(temp)
	}
}

retrieveCategoriesFromMETA <- function(){
	# Returns the current category table from META
	# Ensure that dictionary is loaded
	loadDICT()
	
	theshortnames <- strsplit(META[item == 'shortname'][, value] %&% '|',
		'\\|')[[1]]
	thedescriptions <- strsplit(META[item == 'description'][, value] %&% '|',
		'\\|')[[1]]

	temp <- strsplit(META[item == 'category'][, value] %&% '|', '\\|')[[1]]
	thecats <- integer(length(temp))
	thecats[!temp %in% c('', 'NA')] <- as.integer(temp[!temp %in% c('', 'NA')])

	out <- data.table(category = thecats,
		shortname = theshortnames,
		description = thedescriptions)
	out <- subset(out, !is.na(category) & category > 0)
	setcolorder(out, c('category', 'shortname', 'description'))
	setkey(out, 'category')
	actualCats <- unique(CALIBER_DICT[!is.na(category), category])
	for (actualCat in actualCats){
		# if a category is not included, add a line to out
		if (!(actualCat %in% out$category)){
			out <- copy(rbind(out, data.table(category=actualCat,
				shortname='', description='')))
		}
	}
	setkey(out, category)
	out
}

saveCategories <- function(numbers, shortnames=NULL,
	descriptions=NULL, cattable=NULL, codelist=NULL){
	# Arguments: numbers - integer vector
	#            shortnames - brief description (ideally not more than
	#                    10-12 characters)
	#            descriptions - longer description
	#            cattable - new categories data.table
	#            codelist - codelist to update by reference
	if (is.null(cattable)){
		cattable <- data.table(category=numbers,
			shortname='', description='')
		if (!is.null(shortnames)){
			cattable[, shortname:=shortnames]
		}
		if (!is.null(descriptions)){
			cattable[, description:=descriptions]
		}
	} else {
		cattable <- copy(data.table(cattable)[,
			list(category, shortname, description)])
	}

	# Ensure the category table is sorted by category
	setkey(cattable, category)
	
	if (is.null(codelist)){
		saveCategoriesToMETA(cattable)
	} else {
		saveCategoriesToCodelist(cattable, codelist)
	}
}

saveCategoriesToCodelist <- function(cattable, codelist){
	# Saves the category table to META. The entire new category table
	# must be supplied, not just categories to add.
	# Arguments: cattable - category data.table
	setattr(codelist, 'Categories', cattable)
	invisible(codelist)
}

removeUnusedCategories <- function(codelist = NULL){
	# Removes unused categories and category 0 from codelist
	# or CALIBER_DICT (if codelist is NULL).
	if (is.null(codelist)){
		categoriesInUse <- unique(CALIBER_DICT[dict %in% getdictionary()
			& category>0, category])
		cattable <- retrieveCategoriesFromMETA()
		cattable <- cattable[cattable$category %in% categoriesInUse, ]
		saveCategoriesToMETA(cattable)
	} else {
		categoriesInUse <- unique(codelist$category)
		cattable <- retrieveCategories(codelist)
		cattable <- cattable[cattable$category %in% categoriesInUse, ]
		saveCategoriesToCodelist(cattable, codelist)
	}	
}

saveCategoriesToMETA <- function(cattable){
	# Saves the category table to META. The entire new category table
	# must be supplied, not just categories to add.
	# Arguments: newcategory - integer, category numbers or a data.table
	#                    with columns category, shortname, description
	#            shortnames - brief description (ideally not more than
	#                    10-12 characters)
	#            descriptions - longer description
	temp <- list(as.character(cattable$category),
		as.character(cattable$shortname),
		as.character(cattable$description))
	temp <- sapply(temp, paste, collapse='|')
	META[item=='category', value:=temp[1]]
	META[item=='shortname', value:=temp[2]]
	META[item=='description', value:=temp[3]]
	invisible(META[item %in% c('category', 'shortname', 'description')])
}

addCategory <- function(number, shortname = "", description = NULL,
	codelist = NULL){
	# Updates or adds a single row to the category table in META
	# or updates a codelist
	# Arguments: number - integer, category number
	#            shortname - brief description (ideally not more than
	#                    10-12 characters), single character
	#            description - longer description, single character
	categories <- retrieveCategories(codelist)
	
	# Rename to avoid name conflicts
	add_shortname <- shortname
	rm(shortname)
	add_description <- description
	rm(description)
	
	if (is.null(add_description)){
		add_description <- add_shortname
	}
	if (number %in% categories$category & add_shortname!=''){
		categories[category==number, shortname:=add_shortname]
		categories[category==number, description:=add_description]
		saveCategories(cattable=categories, codelist=codelist)
	} else if (number %in% categories$category & add_description!=''){
		categories[category==number, description:=add_description]
		saveCategories(cattable=categories, codelist=codelist)
	} else if (!number %in% categories$category) {
		newcat <- categories[1:(nrow(categories)+1)]
		newcat[nrow(newcat), category:=as.integer(number)]
		newcat[nrow(newcat), shortname:=add_shortname]
		newcat[nrow(newcat), description:=add_description]
		setkey(newcat, category)
		saveCategories(cattable=newcat, codelist=codelist)
	}
}
