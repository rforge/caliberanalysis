
importDT <- function(filename, datecolnames = NULL,
	verbose = TRUE,	sep = NULL, zipname = NULL,
	key = NULL, convertLogical = TRUE, fread = TRUE,
	nrows = NULL, ...){
	# Imports a Stata file or CSV file or zipped file to data.table
	# Arguments: filename = the file to load from
	#            key = character vector of new data.table key columns
	#            convertLogical = whether to convert a vector of 0 and 1
	#                  to logical (TRUE/FALSE/NA), in order to save space
	#            ... other arguments to pass to read.delim
	loadmessage <- paste('Loaded from', filename)
	
	# Load Stata dataset or text file or zipped file
	if (!is.null(zipname) | grepl('\\.zip$|\\.ZIP$', filename)){
		filename <- getzipfilename(filename, zipname)
	}
	
	if (grepl('\\.dta$|\\.DTA$', filename)){
		# Load from Stata file
		temp <- read.dta(filename, ...)
		# Convert to data.table, keeping all attributes intact
		datafile <- data.table(temp)
		
		for (eachattr in intersect(names(attributes(temp)),
			c('datalabel', 'time.stamp', 'formats', 'types',
				'val.labels', 'var.labels', 'version'))){
			setattr(datafile, eachattr, attr(temp, eachattr)) 
		}
			
	} else if (grepl('\\.gz$|\\.GZ$', filename)) {
		# fread cannot read gzipped files
		datafile <- textfileToDT(filename, verbose = verbose,
			datecolnames = datecolnames, sep = sep,
			fread = FALSE, nrows = nrows, ...)
	} else {
		# Load from text file
		datafile <- textfileToDT(filename, verbose = verbose,
			datecolnames = datecolnames, sep = sep,
			fread = fread, nrows = nrows, ...)
	}

	if (convertLogical){
		# Convert 0,1 to logical to save memory
		for (i in names(datafile)){
			if (identical(class(datafile[[i]]), 'integer')){
				if (all(datafile[[i]][1:10] %in% c(0, 1, NA))){
					if (all(datafile[[i]] %in% c(0, 1, NA))){
						if (verbose){
							message(paste('Converting', i, 'to logical.'))
						}
						datafile[, eval(parse(text=paste('`', i,
							'`:=as.logical(`', i, '`)', sep = '')))]
					}
				}
			}
		}
	}
	
	if (!is.null(key)){
		# set data table key
		if (verbose){
			message(paste(c('Setting key columns:', key), collapse=' '))
		}
		setkeyv(datafile, key)
	}
	setattr(datafile, 'filesource', loadmessage)
	datafile
}

textfileToDT <- function(filename, verbose = TRUE, 
	datecolnames = NULL, sep = NULL, fread = TRUE, nrows = nrows, ...){
	# convert text tile to data.table and do date conversion
	# Only use fread if importing all rows, otherwise it is slow
	# because it reads the entire file.
	if (is.null(nrows)){
		nrows <- -1
	}

	if (fread & nrows == -1){
		if (is.null(sep)){
			sep <- 'auto'
		}
		# There is currently a bug in fread (in data.table 1.8.8)
		# so set verbose = FALSE
		datafile <- fread(filename, sep = sep,
			verbose = FALSE, nrows = nrows, ...)
		# Convert column names into data.frame style
		# column names (i.e. row.names = X instead of blank,
		# no duplicates, etc.)
		setnames(datafile, readColNames(colnames(datafile)))
	} else {
		if (is.null(sep)){
			sep <- findDelimiter(filename)
		}
		datafile <- data.table(read.delim(filename, as.is = TRUE, 
			sep = sep, nrows = nrows, ...))
	}
	# convert dates
	if (verbose){
		message(paste('Imported to data.table with', nrow(datafile),
			'rows and', ncol(datafile), 'columns'))
		message('Column classes after attempted date conversion:')
		message(paste(capture.output(print(
			convertDates(datafile, datecolnames))), collapse = '\n'))
	} else {
		convertDates(datafile, datecolnames, verbose = verbose)
	}
	datafile
}

findDelimiter <- function(filename){
	# Read the first line of file 
	trial <- readLines(filename, n = 1)
	if (grepl('\t', trial)){
		'\t'
	} else if (grepl(';', trial)){
		';'
	} else {
		','
	}
}


# Functions for importing text files to data.table

getzipfilename <- function(filename, zipname = NULL){
	# unzips to temp directory and extracts
	if (is.null(zipname)){
		# filename is the zipname, and the actual file is the first
		# one in the zip file
		zipname <- filename
		filename <- unzip(zipname, list=TRUE, junkpaths=TRUE)$Name[1]
	}
	unzip(zipname, exdir=tempdir(), junkpaths=TRUE)
	paste(tempdir(), filename, sep='/')
}

readColNames <- function(colnames){
	# Converts fread column names into read.delim-style column names, i.e.
	# blank column names or duplicates are not allowed; if a duplicate is found
	# it has .<number> appended, where the number is the lowest integer such that
	# there are no duplicates. Row names are numbered X.

	findName <- function(newname, existingnames){
		# returns a version of newname which is not in existingnames
		if (newname %in% existingnames){
			thenumber <- 1
			outname <- paste(newname, thenumber, sep = '.')
			while (outname %in% existingnames){
				thenumber <- thenumber + 1
				outname <- paste(newname, thenumber, sep = '.')
			}
			outname
		} else {
			newname	 			
		}
	}

	# Replace blank column names by 'X'
	colnames[colnames == ''] <- 'X'
	if (length(colnames) > 1){
		for (i in 2:length(colnames)){
			if (colnames[i] %in% colnames[1:(i-1)]){
				colnames[i] <- findName(colnames[i], colnames[-i])
			}
		}
	}
	colnames
}

