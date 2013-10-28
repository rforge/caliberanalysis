showhtml <- function(show_not_selected = TRUE){
	# Show codelist results in HTML document.
	# For ICD10, format parents in bold and children in italic.
	# Writes the current codelist to HTML, returns a HTML string.
	# Argument: whether to show terms that are not selected

	# Ensure that dictionaries are loaded
	loadDICT()

	varname <- META['Name'][, value]
	if (is.na(varname)){
		varname <- ''
	}
	if (varname==''){
		varname <- 'variable'
	}
	
	if (show_not_selected){
		# Mark unselected but related terms
		CALIBER_DICT[explode(!CALIBER_DICT[,
			category < 1 | is.na(category)]), category:=-1L]
	}
		
	# Get category table
	catTableForContents <- retrieveCategories()
	catTableForHeadings <- copy(catTableForContents)
	
	# catTableForContents has hyperlinks
	catTableForContents[category==-1, shortname:='Not selected']
	catTableForContents[category==-1,
		description:='Not selected but code shares the first 3 characters with an included term']
	catTableForContents[category==0, shortname:='Excluded']
	catTableForContents[category==0,
		description:='Terms initially selected then excluded']
	catTableForContents[, description:='<a href="#cat' %&%
		category %&% '">' %&% sanitizehtml(description) %&% '</a>']
	for (thecat in catTableForContents$category){
		catTableForContents[category==thecat,
			nterms:=sum(CALIBER_DICT$category==thecat &
			CALIBER_DICT$dict %in% getdictionary(), na.rm=TRUE)]
	}
	
	out <- character(10)
	
	# title:
	# Codelists for XXXX
	# or ICD10 codelist for XXX
	out[1] <- '<h1>' %&% ifelse(sum(META[ALLDICTS][, value] != 'FALSE')==1,
		toupper(META[ALLDICTS][value=='TRUE', item]) %&%
		' codelist for ', 'Codelists for ') %&% varname %&% '</h1>'
	out[2] <- '<p>Generated on ' %&% Sys.time() %&%
		' using the CALIBERcodelists package, version ' %&%
		packageVersion('CALIBERcodelists') %&%
		' and source dictionaries:</p>\n<p>' %&% 
		paste(sapply(toupper(getdictionary()), function(x){
			attr(CALIBER_DICT, 'VERSION_' %&% x)
		}), collapse='</p>\n<p>')

	out[3] <- '</p><h2>Categories:</h2>\n' %&%
		'<table>\n' %&%
		'<tr><th>Category</th><th>Short name</th><th>Description</th><th>Number of terms</th></tr>\n'
	
	# Contents table
	out[4] <- paste(htmlTableFragment(catTableForContents,
			sanitizeFUN=function(x) x), collapse='\n') %&% '</table>\n'
	
	# warning that ICD-10 numbers may be inaccurate
	if ('icd10' %in% getdictionary()){
		out[4] <- out[4] %&% '\n<p><i>Note: The number of terms for ICD-9 and ICD-10 include all 5-character sub-codes and may be greater than the number of terms shown below</i></p>'	
	}
	
	out[5] <- '<h2>Terms included in variable definition</h2>\n'
	
	# Categories included in codelist
	out[6] <- paste(sapply(setdiff(catTableForHeadings$category, c(0, -1)),
		function(i){
			'<h3><a id="cat' %&% i %&% '"></a>Category ' %&% i %&% ': ' %&%
			sanitizehtml(catTableForHeadings[category==i, description]) %&%
			'</h3>\n' %&%
			htmlCodelistTable(CALIBER_DICT[category==i], showcat=TRUE)
		}), collapse='\n')
	
	# Terms excluded (category = 0)
	out[7] <- '<h2><a id="cat0"></a>Codes and terms initially selected then excluded in variable definition</h2>\n'
	if (nrow(CALIBER_DICT[category==0]) > 0){
		out[8] <- htmlCodelistTable(CALIBER_DICT[category==0], showcat=FALSE)
	} else {
		out[8] <- '<p>(no terms)</p>\n'
	}
	
	if (show_not_selected==TRUE){
		# Terms not selected, but with same first 3 characters as an included term
		out[9] <- '<h2><a id="cat-1"></a>Codes not selected which share the first 3 characters with an included term</h2>\n'
		if (nrow(CALIBER_DICT[category==-1]) > 0){
			out[10] <- htmlCodelistTable(CALIBER_DICT[category==-1], showcat=FALSE)
		} else {
			out[10] <- '<p>(no terms)</p>\n'
		}
	}
	
	cat(paste(out, collapse='\n'))
	invisible(out)
}


htmlCodelistTable <- function(data_to_show, showcat=TRUE,
	tablestyle='<table>\n'){

	# Produces a set of htmlTableFragments for all dictionaries in use
	out <- list()
	mytable <- copy(data_to_show)
	setkey(mytable, code)
	
	# A vector of dictionary codes to select the appropriate
	# subset of mytable
	dict <- mytable$dict
	
	# All terms in this table should be in the same category
	thisCategory <- mytable[1, category]
	
	if (showcat){
		mytableRead <- mytable[, list(code, term, category, events)]
		mytable <- mytable[, list(code, term, category)]
	} else {
		mytableRead <- mytable[, list(code, term, events)]		
		mytable <- mytable[, list(code, term)]
	}
	
	# Whether to put titles for each dictionary
	moreThanOneDict <- sum(META[ALLDICTS][, value]=='TRUE') > 1
	
	if (META['read'][, value] != 'FALSE'){
		# Table of Read terms
		if (moreThanOneDict){
			out[[1]] <- '<h4>Read terms</h4>'	
		}
		if (sum(dict=='read')==0){
			out[[2]] <- '<p>(no terms)</p>'
		} else {
			temp <- mytableRead[dict=='read']
			out[[2]] <- tablestyle %&% 
				htmlTableFragment(as.list(names(temp)), header=TRUE)
			out[[3]] <- paste(htmlTableFragment(temp),
				collapse='\n') %&% '\n</table>'
		}
	}

	if (META['icd10'][, value] != 'FALSE'){
		# Table of ICD10 terms, contracted and expanded with parent/child terms
		# highlighted appropriately
		if (moreThanOneDict){
			out[[4]] <- '<h4>ICD10 terms</h4>'
		}

		if (sum(dict=='icd10')==0){
			out[[5]] <- '<p>(no terms)</p>'
		} else {
			temp <- mytable[dict == 'icd10']
			setattr(temp, 'Source', SOURCEDICTS[dict == 'icd10', Source][1])
			temp[, category:=thisCategory]
			temp <- expandCodelist(temp)[,
				list(code, term, category, hierarchy)]
			if (!showcat){
				temp[, category:=NULL]
			}
			# rawcode is used for ordering only
			temp[, rawcode:=code]
			setkey(temp, rawcode)
			temp[hierarchy=='parent', code:='<b>' %&% code %&% '</b>']
			temp[hierarchy=='parent', term:='<b>' %&% term %&% '</b>']
			temp[hierarchy=='child', code:='<i>' %&% code %&% '</i>']
			temp[hierarchy=='child', term:='<i>' %&% term %&% '</i>']
			out[[5]] <- tablestyle %&%
				htmlTableFragment(
				as.list(setdiff(names(temp), c('rawcode', 'hierarchy'))),
				header=TRUE)
			out[[6]] <- paste(htmlTableFragment(
				temp[, setdiff(names(temp), c('rawcode', 'hierarchy')), with=FALSE]),
				collapse='\n') %&% '\n</table>'
		}
	}

	if (META['icd9'][, value] != 'FALSE'){
		# Table of ICD9 terms
		if (moreThanOneDict){
			out[[7]] <- '<h4>ICD9 terms</h4>'
		}

		if (sum(dict=='icd9')==0){
			out[[8]] <- '<p>(no terms)</p>'
		} else {
			temp <- mytable[dict == 'icd9']
			out[[9]] <- tablestyle %&%
				htmlTableFragment(as.list(names(temp)), header=TRUE)
			out[[10]] <- paste(htmlTableFragment(temp),
				collapse='\n') %&% '\n</table>'
		}
	}

	if (META['opcs'][, value] != 'FALSE'){
		# Table of OPCS terms
		if (moreThanOneDict){
			out[[10]] <- '<h4>OPCS terms</h4>'
		}
		if (sum(dict=='opcs')==0){
			out[[11]] <- '<p>(no terms)</p>'
		} else {
			temp <- mytable[dict=='opcs']
			out[[11]] <- tablestyle %&%
				htmlTableFragment(as.list(names(temp)), header=TRUE)
			out[[12]] <- paste(htmlTableFragment(temp),
				collapse='\n') %&% '\n</table>'
		}
	}

	out$sep <- ''
	do.call('paste', out)
}

htmlTableFragment <- function(data, startrow='<tr>',
	endrow='</tr>', header=FALSE, sanitizeFUN=sanitizehtml){
	# A different sanitizeFUN can be specified
	startrow %&% do.call('paste', lapply(data, function(x){
		ifelse(header, '<th>', '<td>') %&% sanitizeFUN(x) %&%
			ifelse(header, '</th>', '</td>')
	})) %&% endrow
}

sanitizehtml <-
function(text){
	# Only allows the paired HTML tags <b> and <i> 
	text <- gsub('&', '&amp;', text)
	text <- gsub('<', '&lt;', text)
	text <- gsub('>', '&gt;', text)
	text <- gsub('&lt;b&gt;([[:print:]]*)&lt;/b&gt;', '<b>\\1</b>', text)
	text <- gsub('&lt;i&gt;([[:print:]]*)&lt;/i&gt;', '<i>\\1</i>', text)
	return(text)
}


