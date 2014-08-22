# R forest plots for display

########################################
# INSTRUCTIONS
# 
# 1. Prepare CSV file
# The first two letters of each column heading identifies the column, and the remaining part of the name says what information is in the column, which can be of the following types:
#   
# For text columns:
# text - the actual text
# fontface - either 'plain', 'italic', 'bold' or 'bold.italic' (must be in lower case)
# fontsize - size of font in points
# just - 'left', 'right' or 'centre'
# numcols - number of columns to span
# 
# For graph columns - (ALL COEFFICIENTS MUST BE UNEXPONENTIATED):
# mean - coefficient
# upper - upper 95% CI
# lower - lower 95% CI
# se - standard error, can be used to calculate 95% CI if upper and lower are not supplied
# boxsize - relative box sizes
# boxcolour - colour specified as a string, see http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf for a list of colours
# linecolour
# linewidth - default is 2
# summary - 1 if it is the summary row, 0 if not
# clipupper - clip if upper CI greater than this
# cliplower - clip if lower CI less than this
# 
# Options which apply to the whole graph, for which only the first row of the column is considered:
# xlog - 1 for log scale (default), 0 for ordinary scale (which will not exponentiate coefficients either)
# xlab - default is 'Hazard ratio'
# graphwidth - in centimetres

# 2. Set working directory to the location of your CSV file
# Use the command
# setwd('N:/Documents/')
# setwd('N:\\Documents\\')
# Use forward slashes or double backslashes in the file path

# 3. Use the function:
# multiforest(datafile, ...) - for output on screen
#
# multiforest_pdf(datafile, outfile, ...) - for PDF output
# (NB: fonts are not embedded so for publication you need to embed
# fonts e.g. by opening and saving it in Inkscape)
#
# multiforest_png(datafile, outfile, ...) - for PNG output (default 300dpi)
# PNG is a raster file format, and is also suitable for publication
# but file size will be larger than PDF
#
# All three functions can take additional options (default values given):
# 
# rowheights=1.1 (relative row height, can specify a vector of heights for individual rows)
# colgaps=unit(0.3, 'cm') (gaps between the columns, can specify a different width for each gap)
# colgaps_cm - alternative way of specifying gaps between
#     columns, as a vector of numbers for gap size in centimetres
# defaultfontface='plain' (alternatives: 'bold', 'italic', 'bold.italic')
# defaultjust='left' (alternatives: 'right', 'centre')
# defaultfontsize=10 
# defaultgraphwidth=unit(5, 'cm') (width of graph if not specified in data)
# defaultxlab='Hazard ratio' (label for x axis)
# xticks=NULL (calculate from data, or specify a vector of numbers)
# xtickfontsize=10 (size of axis numbers)
# xlabfontsize=12 (size of x axis label)
# xlaboffset=-2.5 (offset of x axis label from axis)
# zerocolour='gray' (colour of zero (HR=1) line)
# zerolinewidth=1 (width of HR=1 line)
#
# Note that if there is an error, the graphics device may be left open.
# Type dev.off() to close the device before re-running.

########################################
# Functions for producing PDF or PNG directly
# 
multiforestPDF <- function(data, outfile, border=0.1, ...){
  # Need to do PDF (with NULL file) in order to get correct size
  if (missing(data)) data <- file.choose()
  if (missing(outfile)) {outfile <- data %&%
    format(Sys.time(), '%d%B_%H.%M.%S')  %&% '.pdf'}
  pdf(file = NULL, width=30, height=15)
  size <- multiforest(data, ...)
  dev.off()
  pdf(outfile, width=size$width_in_inches + 2*border,
      height=size$height_in_inches + 2*border)
  multiforest(data, ...)
  dev.off()
}

multiforestPNG <- function(data, outfile, border=0.1, ...){
  # Need to do PNG in order to get correct size
  # Therefore generate PNG twice!
  if (missing(data)) data <- file.choose()
  if (missing(outfile)) {outfile <- data %&%
    format(Sys.time(), '%d%B_%H.%M.%S')  %&% '.png'}
  png(outfile, width=30, height=15, units='in', res=300)
  size <- multiforest(data, ...)
  dev.off()
  png(outfile, width=size$width_in_inches + 2*border,
		height=size$height_in_inches + 2*border,
		units='in', res=300)
  multiforest(data, ...)
  dev.off()
}

##########################################
# multiforest
# 
# Draws multiple forest plots
# can extract data from a text file or Stata file
multiforest <- function(data = NULL, rowheights = 1.1,
	colgaps = unit(0.3, 'cm'), colgaps_cm = NULL,
	defaultfontface='plain',
	defaultjust = 'left',
	defaultfontsize = 10,
	defaultgraphwidth = unit(5, 'cm'), 
	defaultboxsize = 0.8,
	defaultboxcolour = 'black',
	defaultlinecolour = 'black',
	defaultxlab = 'Hazard ratio', xticks = NULL, 
	xtickfontsize = 10, xlabfontsize = 12,
	xlaboffset = -2.5, zerocolour = 'gray',
	zerolinewidth = 1, zerolineposition = NULL, ...){
	defaultnumcols <- 1
  
	# for interactive mode
	if (is.null(data)){
		data <- file.choose()
	}
	
	# if colgaps are supplied in cm, convert to units
	if (!is.null(colgaps_cm)){
		colgaps <- unit(colgaps_cm, units='cm')
	}
  
	# If you supply a character vector, load it and convert to list
	if (length(data) == 1 & typeof(data) == 'character'){
		data <- multiforestSplitdata(importTemplate(data, ...))
	} else {
		data <- as.data.frame(data)
		data <- multiforestSplitdata(data)
	}

	# data is a list of data.frames (either for text or graphs)
	# Calculate maximum number of rows
	nrows <- max(sapply(data, nrow))
	# Generate istext, a vector
	istext <- sapply(data, function(x){'text' %in% names(x)})
	# Recycle rowheights if necessary, or tim to length of data
	rowheights <- c(rowheights, rep(rowheights[1], nrows))[1:nrows]
	# Recycle colgaps if necessary, make it the correct length
	colgaps <- unit.c(colgaps,
		rep(colgaps[1], length(data)))[1:(length(data)-1)]
  
	graphwidth <- function(data){
	# returns the width from dataframe
		if (is.null(data$graphwidth)){
			defaultgraphwidth
		} else {
			unit(data$graphwidth[1], units='cm')
		}
	}
  
	makeText <- function(data){
		# text and data parameters should be in a data.frame
		# returns a list of textGrobs
			
		# allow align to be used instead of just
		if (is.null(data$just) & !is.null(data$align)){
			data$just <- data$align
		}
		
		for (param in c('fontface', 'just', 'fontsize', 'numcols')){
			# Use defaults if any parameters not specified
			if (!param %in% names(data)){
				data[,param] <- get('default' %&% param)
			}
			# Replace missing with default
			data[,param][is.na(data[,param])] <- get('default' %&% param)
			data[,param][data[,param]==''] <- get('default' %&% param)
		}
		
		# Ensure that font faces and justification are in lower case
		# and remove excess spaces
		data$fontface <- tolower(gsub(' ', '', data$fontface))
		data$fontface[!(data$fontface %in% c('plain', 'italic', 'bold', 'bold.italic'))] <- NA
		data$just <- tolower(gsub(' ', '', data$just))
		data$just[!(data$just %in% c('left', 'right', 'center', 'centre'))] <- NA

		grobs <- vector("list", nrow(data))
		data$text <- as.character(data$text)
		data$text[is.na(data$text)] <- ''
		for (i in 1:nrow(data)){
			thislabel <- data$text[i]
			if (grepl('^expression\\(.*\\)$', thislabel)){
				thislabel <- eval(parse(text = thislabel))
			}
			grobs[[i]] <- textGrob(thislabel,
				x = switch(data$just[i], left=0, right=1, center=0.5, centre=0.5),
				just=data$just[i], 
				gp=gpar(fontface=data$fontface[i], fontsize=data$fontsize[i]))
		}
		# the last row contains the object widths
		grobs$numcols <- data$numcols
		usegrobs <- which(grobs$numcols==min(grobs$numcols))
		grobs$widths <- unit(rep(1,length(usegrobs)), 'grobwidth', grobs[usegrobs])
		grobs$maxwidth <- max(grobs$widths)
		grobs
	}
	
	output <- vector('list', (length(data) * 2 - 1))
	type <- as.character(rep('', length(data) * 2 - 1))
	
	# Calculate overall width and generate text
	# (don't plot anything yet)
	for (i in 1:(length(data) * 2 - 1)){
		# Calcuating overall width etc.
		if (i %% 2 == 0){
			# this is a column gap
			type[i] <- 'colgap'
			thiswidth <- colgaps[i/2]
		} else {
			if (istext[(i+1)/2]) {
				type[i] <- 'text'
				output[[i]] <- makeText(data[[(i+1)/2]])
				thiswidth <- output[[i]]$maxwidth
			} else {
				type[i] <- 'graph'
				thiswidth <- graphwidth(data[[(i+1)/2]])
			}
		}
		if (i==1){
			colwidths <- thiswidth
		} else {
			colwidths <- unit.c(colwidths, thiswidth)
		}
	}
	
	# Draw a text label
	drawText <- function(grob, colnumber, rownumber, numcols=1){
		pushViewport(viewport(layout.pos.row = rownumber,
			layout.pos.col = colnumber:(colnumber + numcols*2 - 2)))
		grid.draw(grob)
		popViewport()
	}
	
	# Draw the graph
	drawGraph <- function(data, colnumber){
		# Creates a viewport, draws the graph, and pops the viewport
		# mean, upper, lower, se, xlog, boxsize, linecolour,
		# boxcolour, linewidth, cliplower, clipupper
		# uses defaultdata, like textGrobs
		if (!('upper' %in% names(data))){
			# calculate from standard error
			if (is.null(data$se)) {data$se <- data$stderr}
			data$upper <- data$mean + 1.96*data$se
			data$lower <- data$mean - 1.96*data$se
		}
		# Should we use a log scale?
		if ('xlog' %in% names(data)){
			xlog <- as.logical(data$xlog[1])
		} else {xlog <- TRUE}
		if ('xlab' %in% names(data)){
			xlab <- (data$xlab[1])
		} else {xlab <- defaultxlab}
		# Which row(s) are the summary?
		if ('summary' %in% names(data)){
			data$summary <- as.logical(data$summary)
		} else {data$summary <- FALSE}
		
		if (!('boxcolour' %in% names(data))){
			data$boxcolour <- defaultboxcolour
		}
		if (!('linecolour' %in% names(data))){
			data$linecolour <- defaultlinecolour
		}
		if (!('cliplower' %in% names(data))){
			data$cliplower <- -Inf
		}
		if (!('clipupper' %in% names(data))){
			data$clipupper <- Inf
		}
		if (!('linewidth' %in% names(data))){
			data$linewidth <- 2
		}
		
		cwidth <- (data$upper - data$lower)
		start <- min(which(!is.na(data$mean)))
		# start is the first row with data

		# If ticks are supplied, don't squash the graph
		if (!is.null(xticks)){
			if (xlog){
				xrange <- c(min(log(xticks)), max(log(xticks)))
			} else {
				xrange <- c(min(xticks), max(xticks))
			}
		} else {
			xrange <- c(min(max(min(data$lower, na.rm = TRUE),
				data$cliplower[1]), 0),
				max(min(max(data$upper, na.rm = TRUE),
				data$clipupper[1]), 0))
		}
		# info is the size of the box, or inverse variance
		if (!is.null(data$boxsize)) {
			if (max(data$boxsize, na.rm=TRUE) > 1) {
				data$boxsize <- data$boxsize / max(data$boxsize, na.rm=TRUE)
			}
			data$info <- data$boxsize
		} else {
			# no boxsize data for this graph
			if (is.null(defaultboxsize)){
				data$info <- 1/(cwidth^2)
				data$info[cwidth == 0] <- NA
				data$info <- data$info/max(data$info[!data$summary], na.rm = TRUE)
				data$info[data$summary] <- 1
			} else {
				data$info <- min(defaultboxsize, 1)
			}
		}
		data$info = data$info / rowheights
		
		# Any missing info -- show as maximum size
		data$info[is.na(data$info)] <- max(data$info, na.rm=TRUE)
		
		# Function to draw 95% confidence intervals within the
		# current viewport
		drawNormalCI <- function(LL, OR, UL, size, boxcolour,
			linecolour, linewidth) {
			# Maximum size is 0.75 times the height of viewport.
			size <-	0.75 * size
			clipupper <-
				convertX(unit(UL, "native"), "npc", valueOnly = TRUE) >	 1
			cliplower <-
				convertX(unit(LL, "native"), "npc", valueOnly = TRUE) <	 0
			box <- convertX(unit(OR, "native"), "npc", valueOnly = TRUE)
			clipbox <- box < 0 || box > 1
			if (clipupper || cliplower) {
				ends <- "both"
				lims <- unit(c(0, 1), c("npc", "npc"))
				if (!clipupper) {
					ends <- "first"
					lims <- unit(c(0, UL), c("npc", "native"))
				}
				if (!cliplower) {
					ends <- "last"
					lims <- unit(c(LL, 1), c("native", "npc"))
				}
				grid.lines(x = lims, y = 0.5, arrow = arrow(ends = ends, 
					length = unit(0.05, "inches")),
					gp = gpar(col = linecolour, lwd = linewidth))
				if (!clipbox) 
					grid.rect(x = unit(OR, "native"), width = unit(size, 
					"snpc"), height = unit(size, "snpc"),
					gp = gpar(fill = boxcolour, col = boxcolour))
			} else {
				grid.lines(x = unit(c(LL, UL), "native"), y = 0.5, 
					gp = gpar(col = linecolour, lwd = linewidth))
				grid.rect(x = unit(OR, "native"), width = unit(size, 
					"snpc"), height = unit(size, "snpc"),
					gp = gpar(fill = boxcolour, col = boxcolour))
				if ((convertX(unit(OR, "native") + unit(0.5 * size, 
					"lines"), "native", valueOnly = TRUE) > UL) && 
					(convertX(unit(OR, "native") - unit(0.5 * size,
					"lines"), "native", valueOnly = TRUE) < LL)) 
					grid.lines(x = unit(c(LL, UL), "native"), y = 0.5, 
					gp = gpar(col = linecolour, lwd = linewidth))
			}
		}
		
		# Function to draw summary confidence intervals
		drawSummaryCI <- function(LL, OR, UL, size, boxcolour, linecolour) {
			grid.polygon(x = unit(c(LL, OR, UL, OR), "native"), 
				y = unit(0.5 + c(0, 0.5 * size, 0, -0.5 * size), "npc"),
				gp = gpar(fill = boxcolour, col = boxcolour))
		}
		
		pushViewport(viewport(layout.pos.col = colnumber,
			layout.pos.row=start:(nrows+1), xscale = xrange))

		# zero axis position (default zero if no log scale,
		# 1 if log scale)
		if (is.numeric(zerolineposition)){
			grid.lines(x = unit(zerolineposition, "native"), y = 0:1,
				gp = gpar(col = zerocolour, lwd=zerolinewidth))
		} else {
			grid.lines(x = unit(0, "native"), y = 0:1,
				gp = gpar(col = zerocolour, lwd=zerolinewidth))
		}

		if (xlog) {
			if (is.null(xticks)) {
				# always include 1 in the axis
				ticks <- pretty(c(1, exp(xrange)), n=4)
				ticks <- ticks[ticks > 0]
			} else {
				ticks <- xticks
			}
			if (length(ticks)) {
				if (min(data$lower, na.rm = TRUE) < data$cliplower[1]) 
					ticks <- c(exp(data$cliplower[1]), ticks)
				if (max(data$upper, na.rm = TRUE) > data$clipupper[1]) 
					ticks <- c(ticks, exp(data$clipupper[1]))
				xax <- xaxisGrob(gp = gpar(col = 'black',
					fontsize=xtickfontsize), at = log(ticks), name = "xax")
				xax1 <- editGrob(xax, gPath("labels"), label=sapply(ticks, format))
				grid.draw(xax1)
			}
		} else {
			if (is.null(xticks)) {
				grid.xaxis(gp = gpar(col = 'black',
					fontsize=xtickfontsize))
			} else if (length(xticks)) {
				grid.xaxis(at = xticks, gp = gpar(col = 'black',
					fontsize=xtickfontsize))
			}
		}
		grid.text(xlab, y = unit(xlaboffset, "lines"),
			gp = gpar(fontsize=xlabfontsize,	col = 'black'))
		popViewport()
		for (i in 1:nrows) {
			if (is.na(data$mean[i])) {
				next
			}
			pushViewport(viewport(layout.pos.row = i,
				layout.pos.col = colnumber, xscale = xrange))
			if (data$summary[i]) {
				drawSummaryCI(data$lower[i], data$mean[i], data$upper[i],
					data$info[i], data$boxcolour[i], data$linecolour[i])
			} else {
				drawNormalCI(data$lower[i], data$mean[i], data$upper[i],
					data$info[i], data$boxcolour[i], data$linecolour[i],
					data$linewidth[i])
			}
			popViewport()
		}
	}
	
	# At the end - pop the final viewport
	par(mar=rep(0,4))
	plot.new()
	pushViewport(viewport(
		layout = grid.layout(nrow = nrows + 2, ncol = length(colwidths),
			widths = colwidths, heights = unit(c(rowheights, 0.5, 3), "lines"))))
	for (j in 1:length(colwidths)) {
		if (type[j]=='text'){
			for (i in 1:nrows) {
				drawText(output[[j]][[i]], j, i, output[[j]]$numcols[i])
			}
		} else if (type[j]=='graph') {
			drawGraph(data[[(j+1)/2]], j)
		}
	}
	popViewport()
	
	# Calculate total height and width
	inchheight <- sum(as.numeric(convertUnit(unit(c(rowheights, 0.5, 3),
		"lines"), 'inches')))
	inchwidth <- sum(as.numeric(convertUnit(colwidths, 'inches')))
	cat('\nImage is ', inchheight, ' inches high and ', inchwidth,
		'inches wide.\n')
	list(height_in_inches = inchheight, width_in_inches = inchwidth)
}



#########################################
# importTemplate
# 
# imports from text file into a dataframe
# automatically detects type of delimiter or if it is Stata file
importTemplate <- function(filename, sep = NULL, ...){
	if (grepl('.dta$', filename)){
		read.dta(filename, convert.factors = FALSE, ...)
	}
	
	if (is.null(sep)){
		sep <- findDelimiter(filename)
	}
	read.delim(filename, sep = sep, row.names = NULL, as.is = TRUE, ...)
}


##########################################
# Convert a single data.frame into a list of data.frames
# for use in the model
multiforestSplitdata <- function(data, prefixlength=2, ...){
	# if data is a filename, import it
	if (length(data)==1 & typeof(data)=='character'){
		data <- importTemplate(data, ...)
	}
	
  # columns prefixed by first n characters
  datatype <- substr(names(data), 1, prefixlength)
  uniquedatatype <- datatype[!duplicated(datatype)]
  lapply(uniquedatatype, function(x){
    temp <- as.data.frame(
      data[, grepl('^' %&%  x, names(data))])
    if (ncol(temp)==1){
      names(temp) <- 'text'
    } else {
      names(temp) <- substr(names(temp), prefixlength+1, 100)
    }
		# ensure that all factor columns are converted to character
		for (i in names(temp)){
			if (is.factor(temp[[i]])){
				temp[[i]] <- as.character(temp[[i]])
			}
			if (grepl('mean$|se$|boxsize$|linewidth$|graphwidth$', i) |
				grepl('summary$|upper$|lower$|clipupper$|cliplower$', i)){
				temp[[i]] <- as.numeric(temp[[i]])
			}
		}
    temp
  })
}

###################################
# A function to concatenate text
"%&%" <- function(a,b){paste(a,b,sep='')}

