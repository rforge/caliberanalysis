as.data.table.ffdf <- function(x, keep.rownames = FALSE){
	# Converts ffdf to data.table
	as.data.table(as.data.frame(x), keep.rownames = keep.rownames)
}

as.ffdf.data.table <- function(x, vmode = NULL, col_args = list(), ...){
	# Converts data.table to ffdf
	# Converts characters to factors if necessary
	if (any(sapply(x, function(z) is.character(z)))){
		message('Converting character vectors to factor in the original data.table')
		toconvert <- colnames(x)[sapply(x, function(z) is.character(z))]
		for (i in toconvert){
			x[, eval(parse(text = paste('`', i,
				'` := factor(`', i, '`)', sep = '')))]
		}
	}
	as.ffdf.data.frame(x, vmode = vmode, col_args = col_args, ...)
}
