downloadFMA <- function(filename = "fma15command.exe"){
	# Downloads program from GitHub
	# Only works on Linux
	
	if (!Sys.info()['sysname']=='Linux'){
		stop('This function only works on Linux and requires git')
	}

	windows_dirname <- paste("Z:", gsub('/', '\\\\', tempdir()),
		'\\fma\\', sep='')
	directory <- paste(tempdir(), '/fma/', sep='')
	options(CALIBERfma_command=paste("wine ", directory,
		filename, sep=''))

	# Set up temporary folders, download Git repositories,
	# and move relevant files to tempdir/fma
	message('Pulling FMA from GitHub repositories into a temporary folder')
	unlink(paste(tempdir(), '/fma', sep=''), recursive = TRUE)
	system(paste('cd', tempdir(), '; mkdir fma; cd fma; mkdir lookups;',
		'mkdir program; cd lookups; git init;',
		'git pull https://github.com/anoopshah/freetext-matching-algorithm-lookups.git;',
		'mv * ../; cd ../program; git init;',
		'git pull https://github.com/anoopshah/freetext-matching-algorithm.git;',
		'mv binaries/* ../'))

	# R version of path to FMA lookups
	options(CALIBERfma_dir_R = directory)
	options(CALIBERfma_dir_win = windows_dirname)
	
	cat('\nFiles downloaded from GitHub to', directory,
		'and CALIBERfma options set.\n')
	cat('\nCALIBERfma_dir_R =', getOption('CALIBERfma_dir_R'))
	cat('\nCALIBERfma_dir_win =', getOption('CALIBERfma_dir_win'))
	cat('\nCALIBERfma_command =', getOption('CALIBERfma_command'))
	cat('\n\nNow you can use the program by calling doAnalysis()\n')
}
