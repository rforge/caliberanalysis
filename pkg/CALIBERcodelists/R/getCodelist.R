getCodelist <- function(folder, codelistname){
	# Finds the latest version of a CALIBER codelist from
	# a folder
	repository <- dir(folder, recursive = TRUE, full.names = TRUE)
	# Get all versions of the codelist
	candidates <- repository[grepl(codelistname %&%
		'\\.codelist\\.[0-9]+\\.(csv|dta)$', repository)]
	# Extract the version numbers
	version <- as.numeric(sub('^.*codelist\\.([0-9]+)\\.(csv|dta)$', '\\1', candidates))
	# Get the codelist with the maximum version
	try(as.codelist(candidates[version == max(version)][1]))
}

