# Sets up your system to use the installed
# Freetext Matching Algorithm program.

cat('\nThis script will ask for the location of the FMA program, lookups
and temporary directory. Leave blank to keep default settings.
If using Wine, the folders must be accessible from a Wine virtual
Windows drive (e.g. Z:\\)\n')

CALIBERfma_command_example <-
	paste("wine ", tempdir(), "/fma/fma15command.exe", sep='')
cat('\nCommand line for invoking the FMA program\n')
cat(paste('(default', CALIBERfma_command_example, ')'))
response <- readline()
if (response == ""){
	options(CALIBERfma_command = CALIBERfma_command_example)
} else {
	options(CALIBERfma_command = response)
}

CALIBERfma_dir_win_example <- 
	paste("Z:", gsub('/', '\\\\', tempdir()), '\\fma\\', sep='')
cat('\nFolder containing FMA (Windows format, for FMA program)\n')
cat(paste('(default', CALIBERfma_dir_win_example, ')'))
response <- readline()
if (response == ""){
	options(CALIBERfma_dir_win = CALIBERfma_dir_win_example)
} else {
	options(CALIBERfma_dir_win = response)
}

CALIBERfma_dir_R_example <-
	paste(tempdir(), '/fma/', sep='')
cat('\nFolder containing FMA (Unix format, for R and wine)\n')
cat(paste('(default', CALIBERfma_dir_R_example , ')'))
response <- readline()
if (response == ""){
	options(CALIBERfma_dir_R = CALIBERfma_dir_R_example)
} else {
	options(CALIBERfma_dir_R = response)
}

