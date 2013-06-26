summaryTable <- function(template, output = NULL,
	datatable = NULL, latex = FALSE, sep = NULL, ...){
	# Generates a summary table from a data.table and a
	# template stating what text to include in the table
	# 
	# If the output is a .tex file, all the formatting
	# functions use LaTeX output
	# Arguments: template = a character matrix or
	#                tab or comma-delimited file
	#                containing instructions on how to
	#                construct the table. If comma separated,
	#                the extension must be .CSV
	#            output = output file name
	#            datatable = a data.table contains the
	#                data to be summarised (or it can be
	#                written in the template)

	# Format of template:
  # The first column should state whether a
	# horizontal line should be drawn below this row (1 / 0)
	# or 1 / blank or TRUE / FALSE

	if (is.character(template) & length(template) == 1){
		if (is.null(sep)){
			sep <- findDelimiter(template)
		}
		temp <- as.matrix(read.delim(template, header = FALSE,
			sep = sep, as.is = TRUE))
	} else {
		temp <- as.matrix(template)
	}
	
	# Name of data.table - use DT as the alias in this function
	if (is.null(datatable)){
		DT <- NULL
		try(DT <- get(temp[1, 2], envir=.GlobalEnv))
	} else {
		DT <- datatable
	}
	if (!is.data.table(DT)){
		stop('Not a data.table')
	}

	# Initialise the output to be the same as the template
	out <- temp
	
	# Cycling through columns
	for (j in 3:ncol(temp)){
		# Cycling through rows
		if (temp[1, j] != '<text>'){
			if (temp[1, j] == ''){
				# include all observations
				subset_expr <- expression(TRUE)
			} else {
				subset_expr <- parse(text=temp[1, j])
			}
			thetext <- ''
			for (i in 2:nrow(temp)){
				if (temp[i, 2]=='<text>'){
					# this row contains text only, so keep as is
				} else {
					# the row might contain a function
					if (temp[i, j]=='.'){
						# use previous function
					} else {
						thetext <- temp[i, j]
					}
					if (grepl('\\($', thetext)){
						# this is a function with an argument
						# the argument is in column 2
						try(out[i, j] <- DT[eval(subset_expr),
							eval(parse(text=thetext %&% temp[i, 2] %&% ')'))])
					} else {
						try(out[i, j] <- DT[eval(subset_expr),
							eval(parse(text=thetext))])
					}
				}
			}
		}
	}
	
	# Remove the first column and first two rows from the output
	out <- out[2:nrow(out), 3:ncol(out)]
	
	# Locate the horizontal lines
	hline <- which(temp[, 1] %in% c('1', 'T', 'TRUE')) - 1

	if (is.null(output)){
		if (latex == FALSE){
			out
		} else {
			exportTable(out, hline.after = hline, ...)
		}
	} else if (grepl('tex$|TEX$', output) | latex == TRUE){
		# output to LaTeX (returns an xtable object)
		exportTable(out, filename = output,
			hline.after = hline, ...)
	} else if (grepl('CSV$|csv$', output)) {
		# output to CSV file
		write.csv(out, filename = output, row.names = FALSE, ...)
	}
}


exportTable <- function(data, filename = NULL, align='l', 
	include.rownames = FALSE, sanitize.text.function = function(x) x, 
	blank = '$-$', digits = 3, booktabs = FALSE, ...){
	# Export a LaTeX table to filename
	# Columns which state multicolumn get converted to multicolumn
	
	sanitize <- function(x){
		x[is.na(x)] <- ''
		temp <- gsub('_', '\\\\_', x)
		temp <- gsub('\\\\\\\\_', '\\\\_', temp)
		temp <- gsub('&', '\\\\&', temp)
		temp <- gsub('\\\\\\\\&', '\\\\&', temp)
		temp <- gsub('%', '\\\\%', temp)
		temp <- gsub('\\\\\\\\%', '\\\\%', temp)
		temp <- gsub('<', '\\\\textless ', temp)
		temp <- gsub('>', '\\\\textgreater ', temp)
		temp
	}
	
	is.wholenumber <-	function(x, tol = .Machine$double.eps^0.5){
		abs(x - round(x)) < tol
	}
	
	# Convert numeric to character, and LaTeXify
	for (i in 1:ncol(data)){
		if (is.numeric(data[,i])){
			if (all(is.wholenumber(data[,i]), na.rm=TRUE)){
				# don't do any formatting - keep them as whole numbers
				data[,i] <- as.character(round(data[,i]))
			} else {
				data[,i] <- sapply(data[,i], function(x){
					temp <- '$' %&% sub('e-([[:digit:]]*)', '\\\\times 10^{-\\1}',
						format(x, scientific=4, nsmall=1, digits=digits)) %&% '$'
					if (temp=='$Inf$') {temp <- '$\\infty$'}
					if (temp %in% c('$NA$', '$NaN$')) {temp <- blank}
					temp
				})
			}
		} else {
			# Sanitize text if not already done so
			use <- !istrue(substr(data[,i], 1, 1)=='$')
			data[use,i] <- sanitize(data[use,i])
		}
	}
	
	# Alignment
	if (nchar(align) < (ncol(data) + 1)){
		# repeat last character if not enough align characters
		align=paste(c(align, rep(substr(align, nchar(align),
			nchar(align)), ncol(data))), collapse='')
	}
	if (nchar(align) > (ncol(data)+1)){
		# truncate align if too many
		align <- substr(align, 1, ncol(data)+1)
	}
	invisible(capture.output(x <- print(xtable(data, align=align), 
		floating=FALSE,
		sanitize.text.function=sanitize.text.function,
		include.colnames=FALSE,
		include.rownames=include.rownames,
		sanitize.rownames.function=sanitize,
		sanitize.colnames.function=sanitize, ...)))
	# replace MULTI1l with \multicolumn{1}{l} ...
	# replace MULTI2c with \multicolumn{2}{c} ... (and remove one &)
	x <- gsub('MULTI1([lrc])([^&]*)', '\\\\multicolumn{1}{\\1}{\\2}', x)
	x <- gsub('MULTI2([lrc])([^&]*)&', '\\\\multicolumn{2}{\\1}{\\2}', x)
	for (i in 3:9){
		replacement <- '\\\\multicolumn{' %&% i %&% '}{\\1}{\\2}'
		pattern <- 'MULTI' %&% i %&% '([lrc])([^&]*)&' %&%
			paste(rep('[[:space:]]+&', i-2), collapse='')
		x <- gsub(pattern, replacement, x)
	}
	
	if (booktabs){
		# Convert \hline to \toprule, \midrule, \bottomrule
		x <- strsplit(x, '\\\\hline')[[1]]
		# paste back using \toprule, \midrule etc.
		x <- x[1] %&% '\\toprule' %&% 
			paste(x[2:(length(x)-1)], collapse='\\midrule') %&% 
			'\\bottomrule' %&% x[length(x)]
	}
	
	if (is.null(filename)){
		return(x)
	} else {
		write(x, file=filename)
		invisible(x)
	}
}


