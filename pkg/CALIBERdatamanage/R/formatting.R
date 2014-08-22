##################################
# FUNCTIONS FOR FORMATTING NUMBERS
# CAN USE LATEX PHANTOMS TO LINE UP DECIMAL POINTS

# Use global options:
# options(xtable.type=='latex')
# options(CALIBERdatamanage_phantom=TRUE)

formatp <- function(pvalues, minp = 0.0001, 
	latex = (getOption('xtable.type') == 'latex'), ...){
	# Format p values
	minptext <- format(minp, scientific=FALSE, ...)
		
	if (is.null(latex)){ latex <- FALSE }
	if (length(latex) == 0){ latex <- FALSE }
	
	# P values smaller than minp are written as <0.0001
	pvalues[pvalues %in% c('<' %&% minptext,
		'$<' %&% minptext %&% '$')] <- '0'
	text <- format(pvalues, scientific=FALSE)

	if (latex){
		text[as.numeric(pvalues) < minp] <- '$<' %&% minptext %&% '$'
	} else {
		text[as.numeric(pvalues) < minp] <- '<' %&% minptext
	}
	formatnum(text, sigfig = 2, scientific = FALSE, latex = latex, ...)
}

formatci <- function(range, scientific = FALSE,
	template = '(\\1, \\2)',  ...){
	sub('([^_]+)__([^_]+)', template,
		formatnum(min(range), scientific = scientific, ...) %&%
		'__' %&% formatnum(max(range), scientific = scientific, ...))
}

formatnum <- function(numbers, dp = NA, sigfig = 3,
	scientific = TRUE, latex = (getOption('xtable.type') == 'latex'),
	usephantom = getOption('CALIBERdatamanage_phantom'), names = NULL,
	decimal.mark = '.'){
	# Only convert those that are actually numbers
	# Can specify either sigfig or decimal places

	if (is.null(latex)){ latex <- FALSE }
	if (length(latex) == 0){ latex <- FALSE }
	if (is.null(usephantom)){ usephantom <- FALSE }

	if (latex){
		# convert % to \%
		numbers <- sub('%', '\\\\%', sub('\\\\%', '%', numbers))
	}
	suppressWarnings(x <- as.numeric(numbers))
	convert <- !is.na(x)
	
	# if dp is not null, use a fixed number of decimal places
	if (!is.na(dp)){
		text <- sapply(x, formatC, format = 'f', digits = dp,
			decimal.mark = decimal.mark)
	} else {
    if (is.na(sigfig)){
      sigfig <- 3
    }
		# use significant figures
		if (scientific & latex){
			text <- sapply(x, format, digits = sigfig, scientific = -1,
				decimal.mark = decimal.mark)
		} else if (scientific) {
			text <- sapply(x, format, digits = sigfig, scientific = -1,
				decimal.mark = decimal.mark)
		} else {
			text <- sapply(x, format, digits = sigfig, scientific = FALSE,
				decimal.mark = decimal.mark)
		}
	}
	
	if (usephantom){
		# Add enough phantoms at the beginning and end
		# (include phantom decimal point)
		# so that all decimal points line up
		# decimal point assumed to be at end of text if not given
		hasnegative <- grepl('^-', text)
		beforeneg <- rep('', length(text))
		if (max(hasnegative)){
			beforeneg[!hasnegative] <- '\\phantom{-}'
		}
		# digits before (ignoring negative sign)
		before <- regexpr(decimal.mark, text %&% decimal.mark,
			fixed = TRUE) - 1 - hasnegative
		before[!convert] <- 0
    before[grepl('e', text)] <- 0
    after <- pmax(0, nchar(text) - (before + 1 + hasnegative))
		# whether to add a phantom decimal point after the text
		addpoint <- (nchar(text) == before)
		# add point if numeric and does not have a decimal point
		text[addpoint & convert] <- text[addpoint & convert] %&%
			'\\phantom{' %&% decimal.mark %&% '}'
		# create before and after phantoms
		before <- sapply(before, function(x){
			paste(rep('\\phantom{0}', max(before) - x), collapse = '')
		})
    # Don't mess about with exponentials
    before[grepl('e', text)] <- ''
    after[!convert] <- 0
    after[grepl('e', text)] <- 0
		after <- sapply(after, function(x){
			paste(rep('\\phantom{0}', max(after) - x), collapse = '')
		})
		after[grepl('e', text)] <- ''
		text <- beforeneg %&% before %&% text %&% after
	}
	if (latex){
		text <- sub('e-0([[:digit:]]*)$', '\\\\times 10^{-\\1}', text)
		text <- sub('e-([[:digit:]]*)$', '\\\\times 10^{-\\1}', text)
		text <- sub('e\\+0([[:digit:]]*)$', '\\\\times 10^{\\1}', text)
		text <- sub('e\\+([[:digit:]]*)$', '\\\\times 10^{\\1}', text)
		text <- '$' %&% text %&% '$'
		if (usephantom){
			# any phantom{0} at the beginning should be outside math mode
			while(any(grepl('\\$\\\\phantom\\{0\\}', text))){
				text <- sub('\\$\\\\phantom\\{0\\}', '\\\\phantom\\{0\\}\\$', text)
			}
		}
		text[text=='$Inf$'] <- '$\\infty$'
		text[text=='$Inf$'] <- '$\\infty$'
		text[text %in% c('NA', 'NaN', '$NA$', '$NaN$')] <- '$-$'
	}
	# Restore what was originally text
	text[!convert] <- numbers[!convert]
  text[sub('^[ ]*', '', sub('[ ]*$', '', text)) %in% c('NA', 'NaN')] <- '-'
	# Output the final formatted text
	
	if (!is.null(names)){
		names(text) <- names
	}
	text
}

formathr <- function(coef, se, df = Inf, dp = NA, pstar = TRUE,
	pvalueci = 0.05, template = '\\1 (\\2, \\3)', ...){
	# Formats hazard ratios and standard errors for display
	# Uses the t distribution if appropriate
	# Replicate df if not given for all observations
	if (length(df) < length(coef)){
		df <- c(df, rep(df[length(df)], length(coef)))[1:length(coef)]
	}
	if (is.na(dp)){
		dp <- rep(2, length(coef))
		dp[coef > 1] <- 1
	}
	pvalues <- 2 * pt(-abs(coef) / se, df)
	out <- character(length(coef))
	for (i in 1:length(coef)){
		out[i] <- 	sub('([^_]+)__([^_]+)__([^_]+)', template,
			formatnum(exp(coef[i]), dp = dp[i], ...) %&% '__' %&%
			paste(formatnum(exp(coef[i] + qt(c(pvalueci / 2,
			1 - (pvalueci / 2)), df[i]) * se[i]), dp = dp[i], ...),
			collapse = '__'))
	}
	if (pstar) {
		# 0.05
		out[istrue(pvalues < pvalueci)] <-
			out[istrue(pvalues < pvalueci)] %&% " *"
		# 0.01
		out[istrue(pvalues < pvalueci / 5)] <-
			out[istrue(pvalues < pvalueci / 5)] %&% "*"
		# 0.001
		out[istrue(pvalues < pvalueci / 50)] <-
			out[istrue(pvalues < pvalueci / 50)] %&% "*"
	}
	out
}

percentConf <- function(logicalvector, dp = 1, ...){
	# returns a string with percentage and confidence interval,
	# but no percentage sign
	result <- binom.test(sum(istrue(logicalvector)),
		sum(!is.na(logicalvector)))
	formatnum(result$estimate * 100, dp = dp, ...) %&% ' ' %&%
		formatci(100 * result$conf.int, dp = dp, ...)
}

meansd <- function(x, ...){
	# returns a string with mean and standard deviation
	# in brackets
	formatnum(mean(x, na.rm=TRUE), ...) %&% ' (' %&%
		formatnum(sd(x, na.rm=TRUE), ...) %&% ')'
}


medianiqr <- function(x, sep = '-', ...){
	# returns a string with median and interquartile range
	formatnum(median(x, na.rm = TRUE), ...) %&% ' (' %&%
		formatnum(quantile(x, 0.25, na.rm = TRUE), ...) %&% sep %&%
		formatnum(quantile(x, 0.75, na.rm = TRUE), ...) %&% ')'
}


"%&%" <- function(a, b) paste(a, b, sep='')

npercent <- function(logicalvector, ...){
	# x must be a logical vector
	# missing ignored	
	if (sum(istrue(logicalvector))==0){
		'0'
	} else {
		sum(istrue(logicalvector)) %&% ' (' %&%
			percent(logicalvector, ...) %&% ')'
	}
}

percent <- function(logicalvector, dp = 1,
	latex=(getOption('xtable.type')=='latex')){
	# x must be a logical vector
	# missing ignored
	# returns percentage as a text string

	if (is.null(latex)){ latex <- FALSE }
	if (length(latex) == 0){ latex <- FALSE }
	
	if (is.null(latex)){
		psign <- '%'
	} else if (latex==TRUE){
		psign <- '\\%'
	} else {
		psign <- '%'
	}
	
	formatC(100*sum(istrue(logicalvector)) /
		sum(!is.na(logicalvector)),
		format='f', digits=dp) %&% psign
}

nmissing <- function(x, ...){
	npercent(is.na(x), ...)
}
