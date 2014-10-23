# Function for combining two codelists and returning the results
# To be accessed as a S3 'merge' method for codelist objects

merge.codelist <- function(x, y=NULL, ...){
	# Returns the combination of the two codelists
	# with new overwriting old, including attributes.
	# You may wish to compare the codelists first using compare().
	# Argument: x - codelist to be updated
	#           y - new terms to overwrite old
	#               categories (old category will
	#               remain if new category is missing)

	# If y is NULL, it defaults to CALIBER_DICT
	# (same source dictionary as x)

	loadDICT()
	if (is.null(y)){
		y <- as.codelist(getSourceDict(x))
	} 

	if (attr(x, 'Source') != attr(y, 'Source')){
		stop('Cannot combine codelists from different sources')
	}

	oldAttr <- extractMetadataFromCodelist(x)
	newAttr <- extractMetadataFromCodelist(y)
	
	for (theAttr in c('Name', 'Version', 'Author', 'Date')){
		if (is.null(newAttr[[theAttr]])){
			if (!is.null(oldAttr[[theAttr]])){
				newAttr[[theAttr]] <- oldAttr[[theAttr]]
			}
		} else if (identical(newAttr[[theAttr]], '') |
			length(newAttr[[theAttr]])==0){
			if (!is.null(oldAttr[[theAttr]])){
				newAttr[[theAttr]] <- oldAttr[[theAttr]]
			}
		}
	}

	# Update category descriptions / add new categories
	oldCats <- retrieveCategories(x)
	newCats <- retrieveCategories(y)
	
	if (nrow(oldCats) > 0 & nrow(newCats) > 0) {
		updatedCats <- rbind(newCats, oldCats[category %in%
			setdiff(oldCats$category, newCats$category), ])[order(category)]
		newAttr$Categories <- updatedCats
	} else if (nrow(oldCats > 0)) {
		updatedCats <- oldCats
	} else {
		updatedCats <- newCats
	}

	if ('hierarchy' %in% names(x)){
	  x[, hierarchy:=NULL]
	}
	if ('hierarchy' %in% names(y)){
	  y[, hierarchy:=NULL]
	}
	setcolorder(x, c(CODELIST_COLORDER,
	                   setdiff(names(x), CODELIST_COLORDER)))
	setcolorder(y, c(CODELIST_COLORDER,
	                 setdiff(names(y), CODELIST_COLORDER)))
  
	if (attr(x, 'Source')=='GPRD'){
		old <- subset(x, medcode %in%
			setdiff(x$medcode, y$medcode))
		out <- rbind(old, y, fill = TRUE)
		setkey(out, medcode)
	} else {
		old <- subset(x, code %in%
			setdiff(x$code, y$code))
		out <- rbind(old, y, fill = TRUE)
		setkey(out, code)
	}

	class(out) <- c('codelist', 'data.table', 'data.frame')

	# Update attributes (Timestamp is now)
	newAttr$Timestamp <- format(Sys.time(), '%H.%m %d-%b-%y')
	addAttributesToCodelist(out, newAttr)
	return(out)
}
