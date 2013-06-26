istrue <- function(x){
	# whether x is true, return FALSE if NA
	x[is.na(x)] <- FALSE
	x
}


isfalse <- function(x){
	# whether x is FALSE, return FALSE if NA
	x[is.na(x)] <- TRUE
	!x
}