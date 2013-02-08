# Function for dealing with ICD-10 codes that are too short (i.e. 3 characters)
# Pads out the code to reflect coding possibilities (append "X","-","X-)
# Author: kimorley
###############################################################################

convertICD10 <- function(data, icdName='icd_code'){
	# Checks
	if (!(icdName %in% names(data))){
		stop("ICD-10 column name not in data names.")
	}
	# Function for converting ICD-10 codes - to be applied down rows
	recodeicd <- function(data){
		code <- gsub("[[:space:]]*$","", data[which(names(data)==icdName)])	# Remove trailing whitespace
		other <- data[which(names(data)!=icdName)] 
		if (nchar(code) < 4){
			new <- data.frame(
					rbind(
							cbind(V1=code,t(other)),
							cbind(V1=paste(code,'X',sep=''),t(other)),
							cbind(V1=paste(code,'-',sep=''),t(other)),
							cbind(V1=paste(code,'X-',sep=''),t(other))
					),
					row.names=NULL,
					stringsAsFactors=FALSE
			)
		}else if (nchar(code) < 5){
			new <- data.frame(
					rbind(
							cbind(V1=code,t(other)),
							cbind(V1=paste(code,'X',sep=''),t(other)),
							cbind(V1=paste(code,'-',sep=''),t(other))
					),
					row.names=NULL,
					stringsAsFactors=FALSE
			)
		}else{
			new <- data
		}
		names(new)[1] <- icdName
		return(new)
	}
	# Apply function
	tmp <- apply(data,1,recodeicd)
	# Convert from list to data.frame
	tmp <- do.call(rbind,tmp)
	return(tmp)
}
