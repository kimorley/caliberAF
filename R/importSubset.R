# Modified version of Anoop's got for importing subsets of data, now for gzip'd files 
# Function imports a subset of data from CSV file 
# based on a set of criteria
# e.g. selected medcodes or patient IDs
# and keep a set of columns relating to these
# INPUT:
# [a] The name of the file from which data should be extracteed
# [b] The name of the column we want to filter the data on (default is NULL so no filtering)
# [c] The list of values in that column that mean we want to keep the row
# [d] The names of the columns that should be retained for selected rows
# [e] The name of the column containing the date variable (for the event)
# OUTPUT:
# [a] A data.frame containing the relevant rows and columns from the file.
#---------------------------------------------------------------------------------------------
importSubset <- function(fileName, filterColName=NULL, filterList=NULL,
		keepColNames=c('anonpatid', 'eventdate', 'medcode'),
		dateColNames='eventdate'){
	# Checks and preparation
	if (is.null(filterColName) | is.null(filterList)){
		warning( 'Filter column name and/or filter values are NULL.  All rows will be imported.' )
	}
	if (!file.exists(fileName)){
		stop( 'File specified does not exist.' )
	}
	# Read the header from the file
	header <- names(read.csv(fileName, nrows=1))
	# Identify columns we want to import
	importCols <- rep("NULL", length(header))
	for (i in which(header %in% keepColNames)){importCols[i] <- NA}
	if (sum(is.na(importCols)) != length(keepColNames)){
		stop( paste("Not all column names in keepColNames found in ",fileName,sep='') )
	}
	# Read the rows and columns we're interested in from the .gz file (without uncompressing it)
	temp <- read.csv(fileName, as.is=TRUE, colClasses=importCols)
	temp <- temp[temp[,filterColName] %in% filterList, ]
	# Format date column
	if (!is.null(dateColNames)) {
		if (length(dateColNames) == 1){
			temp[, dateColNames] <- convertdate(temp[, dateColNames])
		} else {
			temp[, dateColNames] <- lapply(temp[, dateColNames], convertdate)    
		}
	}
	# User report and return
	cat(' ... imported ', nrow(temp), ' rows.\n')
	return(temp)
}
