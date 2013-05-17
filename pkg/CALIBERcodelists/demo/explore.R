# Script for exploring codelists
# Use the interactive function

if (.Platform$OS=='unix'){
	sprog <- readline('What is your spreadsheet program (default == "soffice")')
	if (sprog==''){sprog <- 'Default'}
	textedit <- readline('What is your text editor (default == "gedit")')
	if (textedit==''){textedit <- 'Default'}
} else {
	sprog <- 'Default'
	textedit <- 'Default'
}

exploreCodes(spreadsheet=sprog, texteditor=textedit)
