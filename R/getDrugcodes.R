# Author: kimorley
# Function to extract drug information from product lookup table
# Modified from code by Anoop Shah
# INPUT:
# [a] Vector of drug names
# [b] CALIBER product table (loaded automatically)
# OUTPUT:
# [a] List object - each drug is a data.frame in the list that contains
#		the product code and product name
#----------------------------------------------------------------------
getDrugcodes <- function(drugList, list=FALSE){
	data('CALIBER-LOOKUPS')
	lookupProduct <- data.table(LOOKUP$product)
	drugCodes <- sapply(drugList, function(x) { lookupProduct[grepl(x, drugsubstance, ignore.case=TRUE), list(prodcode, productname)]} , simplify=FALSE)
	if (!list){
		drugs <- data.frame()
		for (i in 1:length(names(drugCodes))){
			temp <- drugCodes[[i]]
			if (dim(temp)[1]!=0){
				drugs <- rbind(drugs,cbind(drugCodes[[i]],drug=names(drugCodes)[i]))
			}
		}
		return(drugs)
	}else{
		return(drugCodes)
	}
}
