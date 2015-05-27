as.data.table.ffdf <- function(x, keep.rownames = FALSE, ...){
	# Converts ffdf to data.table
	as.data.table(as.data.frame(x), keep.rownames = keep.rownames, ...)
}

as.data.table.cohort <- function(x, keep.rownames = FALSE, ...){
	# Converts cohort to data.table
	if (is.data.table(x)){
		x
	} else {
		out <- as.data.table(as.data.frame(x),
			keep.rownames = keep.rownames, ...)
		setattr(out, 'idcolname', attr(x, 'idcolname'))
		setkeyv(out, attr(x, 'idcolname'))
		setattr(out, 'description', attr(x, 'description'))
		if (!is.cohort(out)){
			setattr(out, 'class', c('cohort', class(out)))
		}
		out
	}
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

as.ffdf.cohort <- function(x, vmode = NULL, col_args = list(), ...){
	# Converts cohort to data.table
	if (is.ffdf(x)){
		x
	} else {
		# Converts data.table cohort to ffdf
		# Converts characters to factors if necessary
		if (any(sapply(x, function(z) is.character(z)))){
			message('Converting character vectors to factor in the original data.table')
			toconvert <- colnames(x)[sapply(x, function(z) is.character(z))]
			for (i in toconvert){
				x[, eval(parse(text = paste('`', i,
					'` := factor(`', i, '`)', sep = '')))]
			}
		}
		out <- as.ffdf.data.frame(x, vmode = vmode, col_args = col_args, ...)
		setattr(out, 'idcolname', attr(x, 'idcolname'))
		setattr(out, 'description', attr(x, 'description'))
		if (!is.cohort(out)){
			setattr(out, 'class', c('cohort', class(out)))
		}
		out
	}
}

