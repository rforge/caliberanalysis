# Show the result of FMA input and output
# To be used for program development
# Output as a fmaTest object (list of text analysis output)
# Each element is a list

fmaTest <- function(infile, outfile, medcodefile = NULL,
	lookupfolder = NULL){
	# inputfile must be tab separated, with no column header,
	# pracid, textid, text
	# outputfile must have columns pracid, textid, origmedcode, medcode
	# medcodesfile must have columns pracid, textid, medcode
	if (is.data.frame(infile)){
		IN <- data.table(infile)
	} else {
		IN <- data.table(read.delim(infile,
			header=FALSE, sep='\t', as.is=TRUE))
	}
	
	if (!('pracid' %in% names(IN))){
		setnames(IN, names(IN)[1], 'pracid')	
	}
	if (!('textid' %in% names(IN))){
		setnames(IN, names(IN)[2], 'textid')	
	}
	if (!('text' %in% names(IN))){
		setnames(IN, names(IN)[3], 'text')	
	}
	
	if (is.data.frame(outfile)){
		OUT <- data.table(outfile)
	} else {
		OUT <- fread(outfile)
	}

	if (nrow(OUT) == 0){
		OUT[, order:=integer(0)]
	} else {
		OUT[, order:=1:nrow(OUT)]
	}
	OUT[, thenames:=paste(pracid, textid, origmedcode)]
	setkey(OUT, medcode)
	
	# Load dictionary from lookups folder
	DICT <- outputTerms(lookupfolder)
	setkey(DICT, medcode)
	OUT[, readterm:=DICT[OUT][, term]]
	setkey(OUT, order)
	
	if (is.null(medcodefile)){
		IN[, thenames:=paste(pracid, textid, 0)]
	} else {
		if (is.data.frame(medcodefile)){
			MEDCODES <- data.table(medcodefile)[,
				list(pracid, textid, medcode)]
		} else {
			MEDCODES <- fread(medcodefile)[,
				list(pracid, textid, medcode)]
		}
		setkey(MEDCODES, pracid, textid)
		IN <- merge(IN, MEDCODES, by=c('pracid', 'textid'), all.x=TRUE)
		IN[, thenames:=paste(pracid, textid, medcode)]
	}

	output <- lapply(IN$thenames, function(x){
		# Create a list of input text and fma output
		list(pracid=as.integer(strsplit(x, ' ')[[1]][1]),
			textid=as.integer(strsplit(x, ' ')[[1]][2]),
			medcode=as.integer(strsplit(x, ' ')[[1]][3]),
			readcode=DICT[medcode==as.integer(strsplit(x, ' ')[[1]][3]), readcode],
			readterm=DICT[medcode==as.integer(strsplit(x, ' ')[[1]][3]), term],
			text=IN[thenames==x, text][1],
			fma=as.data.frame(OUT[thenames==x,
				list(enttype, medcode, data1, data2, data3, data4, readterm)]))
		})
	class(output) <- 'fmaTest'
	output
}


print.fmaTest <- function(x, ...){
	# S3 generic method
	printFMATest(x, ...)
}

printFMATest <- function(x, ...){
	cat(paste('\nfmaTest object with', length(x), 'records.\n'))
	for (thisn in 1:length(x)){
		cat(paste(c(rep('-', getOption('width')-1),
			'\n', thisn, '.'), collapse=''))
		print.fmaTestElement(x[[thisn]], ...)
	}
}

"[.fmaTest" <- function(x, y){
	# subset of fmaTest results
	if (length(y)==1){
		temp <- x[[y]]
		class(temp) <- 'fmaTestElement'
		temp
	} else if(length(y)> 1) {
		x <- unclass(x)
		temp <- x[y]
		class(temp) <- 'fmaTest'
		temp	
	}	
}


"[[.fmaTest" <- function(x, y){
	# subset of fmaTest results
	x <- unclass(x)
	temp <- x[[y]]
	class(temp) <- 'fmaTestElement'
	temp
}

print.fmaTestElement <- function(x, ...){
	cat('\n  pracid:', x$pracid, ', textid: ', x$textid)
	cat('\n  Original medcode:', x$medcode, x$readcode,
		x$readterm)
	cat('\n\n  Free text:', x$text)
	cat('\n\n  Structured output:\n')
	print(x$fma, ...)
}

outputTerms <- function(lookupfolder = NULL){
	# Loads output Read terms from lookup tables in FMA format
	if (is.null(lookupfolder)){
		lookupfolder <- getOption('CALIBERfma_dir_R')
	}
	NAT <- fread(paste(lookupfolder, 'nativeterms.txt', sep=''))[
		, list(medcode, readcode, term)]
	VIR <- fread(paste(lookupfolder, 'virtualterms.txt', sep=''))[
		, list(medcode, term)]
	VIR[, readcode:=NA_character_]
	rbind(NAT, VIR, use.names=TRUE)
}

