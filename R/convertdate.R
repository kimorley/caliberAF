# 
# Author: Anoop Shah
###############################################################################

###########################################
# FUNCTION convertdate
# 
# Generic parallelised date conversion function using multicore
convertdate <- function(datestring, multicore=FALSE){
	# Automatically detects date format and converts to 
	# Multicore version - needs library(multicore)
	# Also deals with null or missing dates, and converts them
	# to missing output
	
	datestring <- as.character(datestring)
	
	doconvertdate <- function(datestring){
		# Look at the first date and try potential formats
		formats <- c('%d/%m/%Y', '%d/%m/%y', '%d/%m/%Y %H:%M:%S',
				'%d-%m-%Y', '%d-%m-%y', '%d-%m-%Y %H:%M:%S',
				'%Y-%m-%d', '%Y/%m/%d', '%Y-%m-%d %H:%M:%S',
				'%d %b %Y', '%d %b %y', '%d-%b-%Y', '%m/%d/%Y')
		
		# maxnum = number of dates to try; up to 100 or the total number available
		if (length(datestring) < 100){
			maxnum <- length(datestring)
		} else {
			maxnum <- 100
		}
		
		found <- FALSE
		trynumber <- 0
		while(found==FALSE & trynumber <= length(formats) ){
			trynumber <- trynumber + 1
			# Try to convert the first maxnum dates
			trial <- as.Date(datestring[1:maxnum], formats[trynumber])
			if (sum(is.na(trial)) == 0){
				found <- TRUE # if no missing values, assume all converted
			}
		}
		
		if (found==FALSE){
			cat('ERROR: Unable to detect date format\n')
			datestring
		}	else { 
			if (multicore){
				pvec(datestring, as.Date, format=formats[trynumber])
			} else {
				as.Date(datestring, format=formats[trynumber])
			}
		}
	}
	
	missingformat <- c('0000-00-00', '0000-00-00 00:00:00', '', 'NA', "")
	foundmissingpattern <- FALSE
	i <- 0
	while((foundmissingpattern==FALSE) & (i < length(missingformat))){
		i <- i+1
		if (length(which(datestring==missingformat[i]))>0){
			foundmissingpattern=TRUE
		}
	}
	
	# If there is no missing data, convert the entire lot
	if (foundmissingpattern==FALSE & (sum(is.na(datestring)) == 0)){
		return(doconvertdate(datestring))
	} else {
		# select relevant indices for conversion
		# 'relevant' is a Boolean vector
		relevant <- (!is.na(datestring) & datestring!=missingformat[i])
		conversion <- doconvertdate(datestring[relevant])
		temp <- as.Date(rep(NA, length(datestring)))
		temp[relevant] <- conversion
		return(temp)
	}
}

