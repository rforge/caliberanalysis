# Utility functions for doseconvert program
# Each function stands alone
# Incorporates useful string functions from 
# the VB program strfunc

"%&%" <- function(a, b){
	paste(c(a, b), collapse='')
}

trim <- function(stringvector){
	# removes spaces from the beginning and end of
	# string vector
	gsub('^[[:space:]]+', '',
		gsub('[[:space:]]+$', '', stringvector))
}

sameWord <- function(partstring, nextcharacter){
	# whether nextcharacter is part of the same word as partstring
	# not vectorised
	if (nextcharacter == " ") return (FALSE)	
	if (isNumeric(partstring) &
		isNumeric(partstring %&% nextcharacter)){
		return(TRUE)
	}
	if(isText(partstring) & isText(nextcharacter)){
		return(TRUE)
	} else return(FALSE)
}

isText <- function(instring){
	# whether a text string contains only letters
	# all inputs are in lower case
	# not vectorised
	all(strsplit(instring, '')[[1]] %in% tolower(LETTERS))
}

isNumeric <- function(instring){
	suppressWarnings(!is.na(as.numeric(instring)))
}

strsplitPos <- function(text, splitpoints){
	# splits a string into a vector based on a vector of splitpoints
	# splitpoints are the start of each segment
	if (length(splitpoints)==nchar(text)) {
		# it is probably a Boolean vector
		splitpoints <- which(splitpoints)
	}
	splitpoints <- unique(c(1, splitpoints))
	splitpoints <- sort(splitpoints[splitpoints <= nchar(text)])
	if (length(splitpoints)==0) return(text)
	# generate a matrix of start and end positions for substrings
	splitpoints <- cbind(splitpoints,
											 c(splitpoints[-1]-1, nchar(text)))
	apply(splitpoints, MARGIN=1,
				function(x) substr(text, x[1], x[2]))
}

docalc <- function(myvector) {
	# finds and does calculations in a character vector
	# calculates if there is a +, *, - or / mathematical operator
	todo <- grep('\\*|\\+|/|-', myvector)
	for (i in todo){
		myvector[i] <- as.character(eval(parse(text=myvector[i])))
	}
	myvector
}

restring <- function(charvector, splitpoints, sep=' ') {
	# creates a vector with elements joined by sep
	# splitpoints are the ends of the groups to join
	splitpoints <- sort(unique(c(splitpoints, length(charvector))))
	# the last element of charvector is a mandatory splitpoint
	if (length(splitpoints)==1) {
		# a single splitpoint must mean it is at the end of the vector
		# no split, instead join the entire vector
		paste(charvector, collapse=sep)
	} else {
		splitpoints <- cbind(c(1,
			splitpoints[1:(length(splitpoints)-1)]+1), splitpoints)
		mapply(function(start, end) {
			paste(charvector[start:end], collapse=sep)
			}, start=splitpoints[,1], end=splitpoints[,2])	
	}
}

is.data.table <- function(x){
	'data.table' %in% class(x)
}
