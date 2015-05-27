GEN_SDCtoDate <- function(GEN_SDC){
	# returns the value in IDate format
	# Dates after 31/12/2014 or before 01/01/1800 are converted to missing
	# GEN_SDC: The date in dd/mm/yyyy format can be obtained as follows:
	# 0 = An invalid/ missing date
	# 2 = A date greater than 31/12/2014
	# 3 = A date earlier than 01/01/1800
	# All other values = number of days between the date and the 31/12/2014 offset by 10.
	# Example:  A value of 4027 decodes to the date 01/01/2004.
	# 4027 - 10 = 4017 days prior to the date 31/12/2014 = 01/01/2004
	out <- ifelse(is.na(GEN_SDC), yes = NA, no =
		ifelse(GEN_SDC > 3,
			yes = as.numeric(as.Date('2014-12-31')) - GEN_SDC + 10,
			no = NA)
		)
	return(as.IDate(out, origin=as.Date('1970-01-01')))
}

YYYYMMDDtoDate <- function(YYYYMMDD, default_day_month = 15, 
	default_month_year = 7, default_day_month_year = 1, 
	valid_lower_year = 1800, valid_upper_year = 2050){
	# Converts dates in YYYYMMDD format to IDate
	tmp <- format(YYYYMMDD, scientific = FALSE)
	tmp <- substr(tmp, 1, 4) %&% '-' %&%
		substr(tmp, 5, 6) %&% '-' %&% substr(tmp, 7, 8)
	tmp[YYYYMMDD < 10000 * valid_lower_year] <- NA
	tmp[YYYYMMDD > 10000 * valid_upper_year] <- NA
	tmp[is.na(YYYYMMDD)] <- NA
	tmp <- as.IDate(tmp)
	
	out <- ifelse(istrue(YYYYMMDD >= valid_lower_year &
		YYYYMMDD <= valid_upper_year),
		yes = as.IDate(ISOdate(YYYYMMDD, default_month_year,
			default_day_month_year)),
		no = ifelse(istrue(YYYYMMDD >= 100 * valid_lower_year &
			YYYYMMDD <= 100 * valid_upper_year),
			yes = as.IDate(ISOdate(YYYYMMDD %/% 100,
				YYYYMMDD %% 100, default_day_month)),
			no = tmp))

	as.IDate(out, origin = as.IDate('1970-01-01'))
}
