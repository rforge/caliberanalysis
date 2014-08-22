# Imports a text file to FFDF
# Option to load multiple files, or load a single file
importFFDF <- function(filename, datecolnames = NULL,
	verbose = TRUE,	sep = NULL,	zipname = NULL,
	nrowcheck = 30,  ...){
	# Loads a set of files to FFDF, assuming that
	# all dates are non-missing (direct date conversion in 
	# read.csv.ffdf). Direct date conversion based on first
	# nrowcheck rows.
	
	if (!is.null(zipname)){
		# Unzip and get actual filenames
		filename <- mapply(getzipfilename, filename, zipname)
	} else {
		for (i in seq_along(filename)){
			if (grepl('\\.zip$|\\.ZIP$', filename[i])){
				filename[i] <- getzipfilename(filename[i])
			}
		}
	}
	
	# Use the importDT function to try to find out which
	# columns have dates
	temp <- importDT(filename[1], nrows = nrowcheck,
		convertLogical = FALSE, datecolnames = datecolnames,
		sep = sep, verbose = FALSE, ...)
	colnames <- names(temp)
	colClasses <- sub('IDate', 'Date',
		sapply(temp, function(x){class(x)[1]}))
	colClasses[colClasses == 'character'] <- 'factor'
	# Ignore integer and logical colclasses in case they
	# need to be converted
	colClasses[colClasses == 'integer'] <- NA
	colClasses[colClasses == 'logical'] <- NA	
	
	# If all dates are missing, colClass is NA. If some dates
	# are missing, colClass is Factor.
	fracmissing <- sapply(temp, function(x){
		sum(is.na(x)) / length(x)
	})

	colClasses[colClasses == 'Date' &
		fracmissing > 0 & fracmissing < 1] <- 'factor'
	colClasses[colClasses == 'Date' & fracmissing == 1] <- NA
	
	# Import to ffdf
	if (verbose) {
		message('Importing ' %&% filename[1])
	}
	datafile <- read.csv.ffdf(NULL, filename[1],
		colClasses=colClasses, ...)
	
	# Append ffdf if necessary
	if (length(filename) > 1){
		for (i in 2:(length(filename))){
			temp <- read.csv(filename[i], nrow=1)
			if (!identical(names(temp), colnames)){
				stop('Column names must be identical in all files\n' %&%
					'but the column names were:\nfile 1: ' %&%
					paste(colnames, collapse=' ') %&% '\nfile ' %&%
					i %&% ': ' %&% paste(names(temp), collapse=''))
			}
			if (verbose){
				message('Importing ' %&% filename[i])
			}
			datafile <- read.csv.ffdf(datafile, filename[i],
				colClasses = colClasses)
		}
	}
	
	if (verbose){
		message(paste('Imported to ffdf with', nrow(datafile),
			'rows and', ncol(datafile), 'columns'))
		message('Column classes after attempted date conversion:')
		message(paste(capture.output(print(
			sapply(as.data.table(datafile[1, ]),
				function(x) class(x)[1]))), collapse = '\n'))
	}
	# Set row names to NULL to save memory
	rownames(datafile) <- NULL
	datafile
}
