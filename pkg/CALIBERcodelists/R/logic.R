# Logic functions, including S3 methods for selection objects

`%AND%` <- function(cond1, cond2){
	# A version of & which respects the selection class.
	if (is.character(cond1)){
		cond1 <- termhas(cond1)
	}
	if (is.character(cond2)){
		cond2 <- termhas(cond2)
	}
	out <- cond1 & cond2
	if (is.selection(cond1) & is.selection(cond2)){
		out[is.na(out)] <- FALSE
		class(out) <- 'selection'
	}
	out
}

`&.selection` <- function(e1, e2){
	out <- as.logical(e1) & as.logical(e2)
	class(out) <- 'selection'
	out
}

`%OR%` <- function(cond1, cond2){
	# A version of | which respects the selection class,
	# and allows cond1 and cond2 to be strings which are
	# converted to selections using as.selection
	if (is.character(cond1)){
		cond1 <- termhas(cond1)
	}
	if (is.character(cond2)){
		cond2 <- termhas(cond2)
	}
	out <- cond1 | cond2
	if (is.selection(cond1) & is.selection(cond2)){
		out[is.na(out)] <- FALSE
		class(out) <- 'selection'
	}
	out
}


`|.selection` <- function(e1, e2){
	out <- as.logical(e1) | as.logical(e2)
	class(out) <- 'selection'
	out
}

NOT <- function(cond1){
	if (is.character(cond1)){
		cond1 <- termhas(cond1)
	}
	if (is.selection(cond1)){
		out <- !cond1
		out[is.na(out)] <- FALSE
		class(out) <- 'selection'
		return(out)
	} else {
		return(!cond1)
	}
}

`!.selection` <- function(x){
	out <- !as.logical(x)
	class(out) <- 'selection'
	out
}

