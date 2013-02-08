# Author: kimorley
###############################################################################

# Unzip a vector of files from RAWDATA to TEMPDIR
tempUnzip <- function(files, data_type, fromRAWPATH=TRUE){
	if (fromRAWPATH){
		a = apply(files, 1, function(x) if (x[3]==data_type){ 
						message(paste('Unzipping ',x[1],sep="")) 
						unzip(paste(RAWPATH,x[1],sep=""), exdir=TEMPDIR) })
	} else {
		a = apply(files, 1, function(x) if (x[3]==data_type){ 
						message(paste('Unzipping ',x[1],sep="")) 
						unzip(x[1], exdir=TEMPDIR) })
	}
}
