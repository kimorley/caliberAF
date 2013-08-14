# Wrapper for using xtable to write a summary table (means and SDs) in 
# LaTeX format to file for incorporation into a project log
# Author: kimorley
###############################################################################

writeLatexMeansTable <- function(data, vars, byVar=NULL, varNames=NULL, fileName){
	sumTab <- data.frame()	# Holds the results
	for (i in vars){	# First we make the calculations for the total sample
		temp <- data[, c(i), with=FALSE]	# Subset the data for the variable of interest (to avoid using var names)
		varSum <- data.frame(Variable=i,
				Mean=unlist(temp[,lapply(.SD, summary)][4,]),
				SD=unlist(temp[,lapply(.SD, sd, na.rm=T)]),
				N=nrow(temp)-unlist(temp[,lapply(.SD, summary)][7,]))
		if (!is.null(byVar)){	# If there are subgroups, calculate summaries for each
			cat <- table(data[, c(byVar), with=FALSE])	# This gives us N for each subgroup
			temp <- data[, c(i,byVar), with=FALSE]
			tempSum <- rbind(as.data.frame(temp[,lapply(.SD, summary), by=byVar]), as.data.frame(temp[,lapply(.SD, sd, na.rm=T), by=byVar]))
			catLab <- unique(tempSum[,1])	# Subgroup labels
			for (j in catLab){	# For each subgroup, generate a summary
				tempSum[tempSum[,1] %in% j,]
				grpSum <- data.frame(Mean=tempSum[tempSum[,1] %in% j,][4,2], 
						SD=tempSum[tempSum[,1] %in% j,][8,2], 
						N=cat[names(cat)==j]-tempSum[tempSum[,1] %in% j,][7,2])	
				varSum <- cbind(varSum, grpSum)								
			}
			catLab <- c("Total",catLab)
		}else{
			catLab <- c("Total")
		}
		sumTab <- rbind(sumTab, varSum)
	}
	# Transfer variable names to row names, then remove that column
	if (is.null(varNames)){
		row.names(sumTab) <- sumTab$Variable
	}else{
		row.names(sumTab) <- varNames
	}
	sumTab$Variable <- NULL
	# Function for creating multicolumns for LaTeX table
	multiColNames <- function(catLab, cat){
		vec <- c()
		for (k in 1:length(catLab)){
			if (k == length(catLab)){
				vec <- c(vec,paste('\\multicolumn{3}{c}{',catLab[k],' (',cat[names(cat)==catLab[k]],')} \\\\',sep=''))
			}else if(k == 1){
				vec <- c(vec,paste('& \\multicolumn{3}{c}{',catLab[k],' (',sum(cat),')} & ',sep=''))
			}else{
				vec <- c(vec,paste('\\multicolumn{3}{c}{',catLab[k],' (',cat[names(cat)==catLab[k]],')} & ',sep=''))
			}
		}
		vec <- paste(vec, collapse='')
		return(vec)
	}
	multiCol <- multiColNames(catLab, cat)
	colNames <- rep(c('Mean','SD','N'),length(catLab))
	colnames(sumTab) <- colNames
	# Format table
	table <- xtable(sumTab,digits=c(0,rep(c(2,2,0),length(catLab))), align=c('l',rep('r',ncol(sumTab))))
	print(table, 
			sanitize.text.function = function(x){x},
			floating=FALSE, 
			hline.after=NULL,
			size="\\footnotesize",
			include.rownames=TRUE,
			add.to.row=list(pos=list(-1,-1,0, nrow(table)),command=c('\\toprule ',multiCol,'\\midrule ','\\bottomrule ')),
			file=fileName
	)
}