# Wrapper for using xtable to write a summary table (counts and percentages) in 
# LaTeX format to file for incorporation into a project log
# Author: kimorley
###############################################################################

writeLatexCountTable <- function(x, y, fileName){
	count <- table(x,y)
	prop <- formatC(prop.table(table(x,y),margin=2)*100,digits=1,format='f')
	sumtab <- data.frame()
	for (i in 1:ncol(count)){
		sumtab <- rbind(sumtab, count[,i])
		sumtab <- rbind(sumtab, prop[,i])
	}
	sumtab <- t(sumtab)
	row.names(sumtab) <- row.names(prop)
	multiColNames <- function(count){
		vec <- c()
		for (i in 1:ncol(count)){
			if (i == ncol(count)){
				vec <- c(vec,paste('\\multicolumn{2}{c}{',colnames(count)[i],'} \\\\',sep=''))
			}else if(i == 1){
				vec <- c(vec,paste('& \\multicolumn{2}{c}{',colnames(count)[i],'} & ',sep=''))
			}else{
				vec <- c(vec,paste('\\multicolumn{2}{c}{',colnames(count)[i],'} & ',sep=''))
			}
		}
		vec <- paste(vec, collapse='')
		return(vec)
	}
	multiCol <- multiColNames(count)
	colNames <- rep(c('(N)','(\\%)'),ncol(count))
	colnames(sumtab) <- colNames
	# Format table
	table <- xtable(sumtab,digits=rep(0,ncol(sumtab)+1), align=c('l',rep('r',ncol(sumtab))))
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