assignmetadata <- function(field, newvalue){
	# Writes metadata to the META table
	# Enforces that version must be numeric and
	# Date must be a date. Not exported; users are
	# expected to call setMetadata instead.
	# Arguments: field = which field (string)
  #            newvalue = new value (string)
	
	if (field=='Date'){
		newvalue <- sanitizeDate(newvalue)
	} else if (field=='Version'){
		newvalue <- as.character(as.numeric(newvalue))
		if (is.na(newvalue)){
			newvalue <- ''
		}
	} else if (field %in% c('Author', 'Name')) {
		newvalue <- as.character(newvalue)
	} else {
		stop(field %&% ' is not a valid metadata field')
	}
	META[item==field, value:=newvalue]
}

setMetadata <- function(codelist=NULL, Name=NULL,
	Version=NULL, Author=NULL, Date=NULL, Categories=NULL,
	Source=NULL){
	# Note that Source cannot currently be updated in META
	# So ONS codelists need to be edited on export.
	if (is.null(codelist)){
		# updating metadata in META
		if (!is.null(Name)){
			assignmetadata("Name", Name)
		}
		if (!is.null(Version)){
			assignmetadata('Version', Version)
		}
		if (!is.null(Author)){
			assignmetadata('Author',
				paste(as.character(Author), collapse=', '))
		}
		if (!is.null(Date)){
			assignmetadata("Date", Date)
		}		
	} else if (!is.codelist(codelist)){
		stop("First argument to setMetadata must be NULL or a codelist")
	} else {
		# updating metadata in a codelist
		if (!is.null(Name)){
			setattr(codelist, "Name", as.character(Name))
		}
		if (!is.null(Version)){
			Version <- as.character(as.numeric(Version))
			if (length(Version) == 1){
				if (is.na(Version)){
					warning('Version must be a number. Use 0, 0.1, ... for unapproved codelists and 1, 2, ... for approved codelists.')
				} else {
					setattr(codelist, "Version", Version)
				}
			} else {
				# Version does not have length 1
				warning('Version must be a number. Use 0, 0.1, ... for unapproved codelists and 1, 2, ... for approved codelists.')
			}
		}
		if (!is.null(Author)){
			setattr(codelist, "Author",
				paste(as.character(Author), collapse=', '))
		}
		if (!is.null(Date)) {
			setattr(codelist, "Date", sanitizeDate(Date))
		}
		if (!is.null(Source)){
			# Currently source can only be changed for ICD-10 codelists
			if ((Source %in% c('HES', 'ONS')) &
				(attr(codelist, 'Source') %in% c('HES', 'ONS'))){
				setattr(codelist, 'Source', Source)
			}
		}
	}

	# Updating categories
	if (!is.null(Categories)){
		saveCategories(cattable=Categories, codelist=codelist)
	}
	invisible(codelist)
}

