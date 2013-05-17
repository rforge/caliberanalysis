# Script for creating a codelist in the interactive way.
# Output is a R Markdown document with .txt extension (so that it is
# opened using appropriate software in Windows).

cat('Creating a Markdown document for a codelist. Press ESCAPE to exit.\n')

if (.Platform$OS=='unix'){
	sprog <- readline('What is your spreadsheet program (default == "soffice")')
	CALIBERcodelists:::META['spreadsheet', value:=sprog]
}

WORKING_DIRECTORY <-
	readline(paste("\nThe working directory is", getwd(),
	"\nIf you would like to change it, please type a new one, otherwise leave blank and press ENTER:"))

if (!(WORKING_DIRECTORY=='')){
	setwd(WORKING_DIRECTORY)
	message('Working directory changed to ' %&% getwd())
}

FILENAME <- readline("Please enter the filename for the new R Markdown document: ")
if (!grepl('\\.rmd\\.txt$|\\.rmd$', tolower(FILENAME))){
	FILENAME <- FILENAME %&% '.Rmd.txt'
}

cat('This script will create a template document which you can re-run using:

process("' %&% FILENAME %&% '").\n')

# Writing metadata to file
TEMP <- readline('Title of the codelist documentation: ')
write(TEMP %&% '\n==================\n', file=FILENAME)
TEMP <- readline('Codename of the new variable (no spaces allowed): ')
write('Name    | ' %&% TEMP %&% '\n:-------|:---------------------\nVersion | 0',
	file=FILENAME, append=TRUE)
SOURCES <- select.list(c('GPRD', 'HES', 'ONS', 'OPCS'), multiple=TRUE,
	title='Which data sources is this codelist for? ')
write('Source  | ' %&% paste(SOURCES, collapse=', '), file=FILENAME, append=TRUE)
TEMP <- readline('Author: ')
write('Author  | ' %&% TEMP, file=FILENAME, append=TRUE)
write('Date    | ' %&% format(Sys.Date(), '%d %b %Y'), file=FILENAME, append=TRUE)
write('\nGeneral description of codelist\n---------\n', file=FILENAME, append=TRUE)
TEMP <- readline('Description of codelist: ')
write(as.character(TEMP), file=FILENAME, append=TRUE)

SOURCEDICTS <- sub('GPRD', 'read', sub('HES|ONS', 'icd10', sub('OPCS', 'opcs', SOURCES)))

setdictionary(SOURCEDICTS)
# Start the R code section
write(as.character('\n\n```{r}\nsetdictionary(' %&%
	paste(('"' %&% SOURCEDICTS %&% '"'), collapse=', ') %&%
	')\n'), file=FILENAME, append=TRUE)

cat('Now you will be able to create a codelist in interactive mode, and
the instructions will be saved as part of the Rmd document. 

No need to assign metadata or export the codelist as this is done automatically.\n')

# Interactive codelist selection process using explore
exploreCodes(showLog = FALSE, direct = 2)

cat('Writing instructions to R Markdown document ...')
instrFile <- scan(tempdir() %&% '/interactive.log',
	what='character', sep='\n')
instr <- instrFile[!grepl('^#', instrFile)]
write(paste(instr, collapse='\n'), file=FILENAME, append=TRUE)

# Exporting codelist
if (!('ONS' %in% SOURCES)){
	write(as.character('\nexportall()\n```\n\n'),
		file=FILENAME, append=TRUE)
} else if ('HES' %in% SOURCES){
	# export both HES and ONS
	write(as.character(
		'\nexportall(icd10_source=c("hes", "ons"))\n```\n\n'),
		file=FILENAME, append=TRUE)
} else {
	# export ONS
	write(as.character(
		'\nexportall(icd10_source="ons")\n```\n\n'),
		file=FILENAME, append=TRUE)
}

process(FILENAME)

cat('The initial iteration of the codelist has been generated. Open ' %&%
	sub('Rmd.txt$|Rmd$|rmd$|RMD$', 'html', FILENAME) %&%
	' to see the documentation.
After manually modifying the markdown document, re-run it using
process("' %&% FILENAME %&% '").')

