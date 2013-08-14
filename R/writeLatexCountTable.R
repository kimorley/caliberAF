# Wrapper for using xtable to write a summary table (counts and percentages) in 
# LaTeX format to file for incorporation into a project log
# Author: kimorley
###############################################################################

writeLatexCountTable <- function(data, vars, byVar=NULL, varNames=NULL, fileName){
	sumTab <- data.frame()	# Holds the results
	if (is.null(varNames)){	# If user doesn't provide variable names, use those existing
		varNames <- vars
	}
	for (i in 1:length(vars)){	# First we make the calculations for the total sample
		temp <- data[, c(vars[i]), with=FALSE]	# Subset the data for the variable of interest (to avoid using var names)
		catLab <- names(table(temp))	# Category labels
		if (length(catLab) != nrow(temp[,lapply(.SD, summary)])){	# Need to generate missing data category
			catLab <- c(catLab,'Missing')
		}
		varSum <- data.frame(Variable=c(varNames[i],rep(NA,nrow(temp[,lapply(.SD, summary)])-1)),
				Categories=as.vector(catLab),
				count=temp[,lapply(.SD, summary)],
				perc=temp[,lapply(.SD, summary)]/nrow(temp)*100)
		names(varSum)[3:4] <- c("count","perc")			
		if (!is.null(byVar)){	# If there are subgroups, calculate summaries for each
			temp <- data[, c(vars[i],byVar), with=FALSE]
			tempSum <- as.data.frame(temp[,lapply(.SD, summary), by=byVar])
			grpLab <- unique(tempSum[,1])	# Subgroup labels
			for (j in grpLab){	# For each subgroup, generate a summary
				tempSum[tempSum[,1] %in% j,]
				grpSum <- data.frame(count=tempSum[tempSum[,1] %in% j,][2], 
						perc=tempSum[tempSum[,1] %in% j,][2]/sum(tempSum[tempSum[,1] %in% j,][2])*100)
				names(grpSum) <- c("count","perc")			
				varSum <- cbind(varSum, grpSum)								
			}
			grpLab <- c("Total",grpLab)
		}else{
			grpLab <- c("Total")
		}
		sumTab <- rbind(sumTab, varSum)
	}
	multiColNames <- function(count){
		vec <- c()
		for (i in 1:length(count)){
			if (i == length(count)){
				vec <- c(vec,paste('\\multicolumn{2}{c}{',count[i],'} \\\\',sep=''))
			}else if(i == 1){
				vec <- c(vec,paste('& & \\multicolumn{2}{c}{',count[i],'} & ',sep=''))
			}else{
				vec <- c(vec,paste('\\multicolumn{2}{c}{',count[i],'} & ',sep=''))
			}
		}
		vec <- paste(vec, collapse='')
		return(vec)
	}
	multiCol <- multiColNames(grpLab)
	colNames <- c('Characteristic','Categories',rep(c('(N)','(\\%)'),length(grpLab)))
	colnames(sumTab) <- colNames
	# Format table
	table <- xtable(sumTab,digits=c(0,0,0,rep(c(0,2),length(grpLab))), align=c('l','l','l',rep('r',ncol(sumTab)-2)))
	print(table, 
			sanitize.text.function = function(x){x},
			floating=FALSE, 
			hline.after=NULL,
			size="\\footnotesize",
			include.rownames=FALSE,
			add.to.row=list(pos=list(-1,-1,0, nrow(table)),command=c('\\toprule ',multiCol,'\\midrule ','\\bottomrule ')),
			file=fileName
	)
}

