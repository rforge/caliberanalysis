getCodelist <- function(folder = NULL, codelistname = NULL){
	# Finds the latest version of a CALIBER codelist from
	# a folder
	if (is.null(codelistname) & !is.null(folder)){
		# search in the current working directory
		codelistname <- folder
		folder <- getwd()
	} else if (is.null(folder)){
		folder <- getwd()
	}
	# Find all the files in the repository
	repository <- dir(folder, recursive = TRUE, full.names = TRUE)
	# Get all versions of the codelist
	candidates <- repository[grepl('[\\\\/]' %&% codelistname %&%
		"\\.codelist\\.[0-9]+\\.(csv|dta)$", repository)]
	# Extract the version numbers
	version <- as.numeric(sub('^.*codelist\\.([0-9]+)\\.(csv|dta)$', '\\1',
		candidates))
	# Get the codelist with the maximum version
	# If more than one codelist, print a warning
	
	out1 <- NULL
	out1 <- try(as.codelist(candidates[version == max(version)][1]))

	if (length(candidates[version == max(version)]) == 2){
		out2 <- NULL
		out2 <- try(as.codelist(candidates[version == max(version)][1]))
		if (!is.null(out2)){
			if (compare(out1, out2)$same_cattable &
				compare(out1, out2)$same_terms){
				if (!compare(out1, out2)$same_attr){
					warning('Two codelists found with same terms but different metadata;\nonly one will be returned.')					
				}
			} else {
				warning('More than one codelist found; only one will be returned.')
			}
		}
	} else if (length(candidates[version == max(version)]) > 1){
		warning('More than one codelist found; only one will be returned.')
	}

	out1
}

