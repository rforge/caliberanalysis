# Performs FMA analysis on a text file or a single text
# Uses the FMA command-line binary located in
# Returns the text output for a single text, or the 
# FMA GOLD format output for a table of texts.



doAnalysis <- function(freetext = NULL,
	medcode = NULL, infile = NULL, outfile = NULL,
	medcodefile = NULL, logfile = NULL,
	fmadir_R = getOption('CALIBERfma_dir_R',
		paste(tempdir(), '/fma/', sep='')),
	fmadir_win = getOption('CALIBERfma_dir_win',
		paste(gsub('/', '\\\\', tempdir()), '\\fma\\', sep=''))){
	# Carries out analysis. If logfile is not given, the
	# output is printed to screen. If outfile is not given,
	# the output is returned as a data.table.
	
	# Prepare configuration file
	config <- data.table(item=c('infile', 'medcodefile', 'outfile',
		'logfile', 'lookups', 'freetext', 'medcode'), value='')
	
	# Input file
	if (is.null(infile)){
		# not using infile
	} else if (is.data.frame(infile)){
		# Columns must be pracid, textid, text
		if (ncol(infile) != 3 | !identical(as.vector(sapply(infile, typeof)),
			c('integer', 'integer', 'character'))){
			stop('Infile must have three columns: pracid (integer), textid (integer), text (character)')
		}
		write.table(infile, paste(fmadir_R, 'infile.txt', sep=''), 
			eol='\r\n', sep='\t', row.names=FALSE, col.names=FALSE, quote=FALSE)
		config[item=='infile', value:=paste(fmadir_win, 'infile.txt', sep='')]	
	} else if (is.character(infile)){
		file.copy(infile, paste(fmadir_R, 'infile.txt', sep=''),
			overwrite=TRUE)
		config[item=='infile', value:=paste(fmadir_win, 'infile.txt', sep='')]
	}

	# Medcodes
	if (is.null(medcodefile)){
		# not using medcode file
	} else if (is.data.frame(medcodefile)){
		write.csv(medcodefile, paste(fmadir_R, 'medcodefile.csv', sep=''),
			eol='\r\n', quote=FALSE, row.names=FALSE)
		config[item=='medcodefile',
			value:=paste(fmadir_win, 'medcodefile.csv', sep='')]	
	} else if (is.character(medcodefile)){
		file.copy(medcodefile, paste(fmadir_R, 'medcodefile.csv', sep=''),
			overwrite=TRUE)
		config[item=='medcodefile',
			value:=paste(fmadir_win, 'medcodefile.csv', sep='')]	
	}
	
	# Freetext
	if (!is.null(freetext)){
		config[item=='freetext', value:=freetext]
		if (!is.null(medcode)){
			config[item=='medcode', value:=as.character(as.integer(medcode))]
		}
	}

	# Output file, lookups and logfile
	config[item=='logfile', value:=paste(fmadir_win, 'logfile.log', sep='')]
	config[item=='outfile', value:=paste(fmadir_win, 'outfile.csv', sep='')]
	config[item=='lookups', value:=fmadir_win]
	
	# Write configuration file
	write.table(config, paste(fmadir_R, 'config.txt', sep=''),
		eol='\r\n', sep='  ', quote=FALSE, row.names=FALSE, col.names=FALSE)

	# Run FMA (wait while it completes)
	thecommand <- paste(getOption('CALIBERfma_command'), 
		gsub('\\\\', '\\\\\\\\',
			gsub('\\\\\\\\', '\\\\', paste(fmadir_win, 'config.txt', sep=''))))
	
	message(paste('Command:', thecommand))
	message(print(config))
	
	system(thecommand)

	# Log file
	if (is.null(logfile)){
		# If no logfile stated, output to console
		logfile <-  scan(paste(fmadir_R, 'logfile.log', sep=''),
			what='character', sep='\n', blank.lines.skip = FALSE)
		cat(paste(logfile, collapse='\n'))
		cat('\n')
	} else {
		# output to file
		file.rename(paste(fmadir_R, 'logfile.log', sep=''), logfile)
	}

	# Return the output as a fmaTest object
	if (is.null(freetext)){
		# if freetext is supplied, the output is in the log file
		# otherwise the output is as a fmaTest object
		output <- NULL
		try(output <- fread(paste(fmadir_R, 'outfile.csv', sep='')))
		unlink(paste(fmadir_R, 'outfile.csv', sep=''))
		if (!is.null(outfile)){
			# Write the output to file
			write.csv(output, outfile, row.names=FALSE, eol='\r\n', quote=FALSE)
		}
		if (is.null(output)){
			output <- data.table(pracid=integer(0), textid=integer(0),
				origmedcode=integer(0), medcode=integer(0), enttype=integer(0),
				data1=double(0), data2=double(0), data3=double(0), data4=double(0))
		}
		return(fmaTest(infile, output, medcodefile))
	}
}

