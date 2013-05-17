process <- function(filename, varname = NULL,
	showR = FALSE, showhtml = TRUE, show_not_selected = TRUE,
	css = customCSS()){
	# Process a file, create codelist and HTML etc.
	# If it is an R file, it is simply source'd with a sink.
	# If it is R markdown, it is converted to md and then to html.
	# Text files (.txt) are assumed to contain R markdown.
	# No R output is shown by default (echo=FALSE, results=hide)
	# except the name of the package and dictionaries used.
	# The working directory is assumed to be the directory
	# containing the Markdown file; the new HTML file and codelists
	# are created in this directory.
	# Arguments: filename - path to a Rmd file documenting a codelist
	#                   selection process
	#            varname - variable name (required for export), this
	#                   would usually be provided in the Rmd file
	#            showR - whether to show R code and output by default
	#            showhtml - whether to show HTML codelist at the end
	#                   ignored if the file contains a showhtml command
	#            show_not_selected - argument to pass to showhtml,
	#                   ignored if showhtml = FALSE
	
	# Clear all metadata
	META[, value:='']
	capture.output(setdictionary(ALLDICTS))
	loadDICT()
	
	filetype <- sub('^[[:print:]]*\\.([^\\.]*)$', '\\1', filename)
	# If it is not a complete filepath, assume that the file is
	# in the current working directory
	if (!grepl('^[[:alpha:]]:', filename) & !grepl('^[\\~/]', filename)){
		# using the current working directory
		filename <- getwd() %&% '/' %&% filename
	}

	# Extract metadata from the Rmd file (version, variable name, )
	rmd <- scan(filename, what=character(), sep='\n', blank.lines.skip = FALSE)
	searchfor <- function(thing){
		use <- grep('^[ \t]*' %&% thing %&% '[ \t]*\\|([[:print:]]*)', rmd)
		if (length(use)>0){
			out <- sub('^[ \t]*' %&% thing %&% '[ \t]*\\|', '', rmd[use[1]])
			out <- sub('^[ \t]*', '', sub('[ \t]$', '', out))
			assignmetadata(thing, out)
		}
	}
	lapply(list('Name', 'Version', 'Author', 'Date'), searchfor)

	# Locate sections of R code
	startR <- grep('^```\\{r', rmd)
	endR <- which(rmd=='```')
	if (length(startR) == length(endR)){
		# must be the same number of start as end sections
		hasShowHTML <- any(mapply(function(x, y){
			any(grepl('showhtml\\(', rmd[x:y]))
		}, startR, endR))
		
		if (!hasShowHTML & showhtml){
			rmd <- c(rmd, '```{r echo=FALSE, results="asis"}',
				'showhtml(show_not_selected = ' %&%
				ifelse(show_not_selected, 'TRUE', 'FALSE') %&% ')',
				'```')
		}
	}

	# Hide all R output if no options specified in R chunk
	if (!showR){
		rmd[grepl('^```\\{r\\}', rmd)] <- '```{r echo=FALSE, results="hide"}'
	}
	
	if (is.null(varname)){
		varname <- META[item=='Name'][,value]
	}
	noVarname <- FALSE
	if (is.null(varname)){
		noVarname <- TRUE
	} else if (is.na(varname)){
		noVarname <- TRUE
	} else if (varname == ''){
		noVarname <- TRUE
	}
	
	if (noVarname){
		stop('No variable name given. Please put a Name|varname entry in the R markdown file')
	}

	if (tolower(filetype) %in% c('r', 'log')){
		sink(filename %&% 'log')
		cat('Running ' %&% filename %&% ' at ' %&% format(Sys.time()) %&% '\n')
		source(filename, echo=TRUE)
		sink()
	} else if (tolower(filetype) %in% c('rmd', 'txt')){
		# knit must run a file in the correct directory because
		# the working directory is changed to the directory in which
		# the Rmd file resides.
		# write(rmd, file=tempdir() %&% '/temp.rmd', ncolumns=1, sep='\n')
		# knit(tempdir() %&% '/temp.rmd', tempdir() %&% '/temp.md')
		tempFilename <- filename
		while (file.exists(tempFilename)){
			tempFilename <- tempFilename %&% '.TMP'
		}
		write(rmd, file=tempFilename, ncolumns=1, sep='\n')
		knit(tempFilename, tempdir() %&% '/temp.md')
		# Create HTML file with correct extension
		markdownToHTML(tempdir() %&% '/temp.md',
			sub('\\.[^\\.]*$', '.html', sub('\\.txt$|\\.TXT$', '', filename)),
			stylesheet = css)
		
		# Clean up
		unlink(tempFilename)
		unlink(tempdir() %&% '/temp.md')
	}
	
	# Compare with previous codelists (comparison stored in message)
	# and show results on screen
	cat(paste(META[item=='message'][,value], collapse='\n'))
	invisible(META[item=='message'][,value])
}

customCSS <- function(){
	"
	body, td {font-family: sans-serif;
		font-size: 10pt;
		margin: 8pt;
	}
	
	h1, h2, h3, h4 {
		display:block;
	  padding:.1em 5px;
	  margin:1em -5px .35em -5px;
	  height:auto;   
	}

	h1 { 
		background-color: #FFFFD8;
		font-size: 130%;
		border-width: 1px 1px 1px 1px;
	  border-style:solid;
	}
	
	h2 {
		background-color: #E8E8E8;
		font-size: 120%; 
	}
	
	h3 {
		color: rgb(0%, 0%, 50%);
		font-size: 110%; 
		border-width: 0 0 1px 0;
	  border-style:solid;
	}
	
	h4 { 
		font-size: 100%;
		border-width: 0 0 1px 0;
	  border-style:solid;
	}
	
	a:visited {
		color: rgb(50%, 0%, 50%);
	}
	
	pre {	
		margin-top: 0;
		max-width: 95%;
		border: 1px solid #ccc;
		white-space: pre-wrap;
	}
	
	pre code {
		display: block; padding: 0.5em;
	}
	
	code.r, code.cpp {
		background-color: #F8F8F8;
	}
	
	th {
		background-color: #E8E8E8;
		text-align:left;
		padding-left: 10px;
		padding-left: 5px;
	}

	td {
		padding-left: 10px;
		padding-left: 5px;
	}
	
	tr:nth-child(odd) {
    background-color: #F4F4F4;
	}

	tr:nth-child(even) {
    background-color: white;
	}

	blockquote {
		color:#666666; margin:0;
		padding-left: 1em;
		border-left: 0.5em #EEE solid;
	}
	
	@media print {
		* { 
			background: transparent !important; 
			color: black !important; 
			filter:none !important; 
			-ms-filter: none !important; 
		}
		
		body { 
			font-size:10pt; 
			max-width:90%; 
		}

		hr { 
			visibility: hidden;
			page-break-before: always;
		}
		
		pre, blockquote { 
			padding-right: 1em; 
			page-break-inside: avoid; 
		}
		
		tr, img { 
			page-break-inside: avoid; 
		}
		
		img { 
			max-width: 100% !important; 
		}
		
		@page :left { 
			margin: 15mm 20mm 15mm 10mm; 
		}
		
		@page :right { 
			margin: 15mm 10mm 15mm 20mm; 
		}
		
		p, h2, h3 { 
			orphans: 3; widows: 3; 
		}
		
		h2, h3 { 
			page-break-after: avoid; 
		}
	}
	"
}
