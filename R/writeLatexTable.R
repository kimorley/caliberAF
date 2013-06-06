# Wrapper for using xtable to write table in LaTeX format to file for 
# incorporation into a project log
# Author: kimorley
###############################################################################

writeLatexTable <- function(table, fileName, colNames=NULL, colAlign=NULL, rowNames=TRUE, nDec=0){
	if (!is.null(colNames)){
		colnames(table) <- colNames
	}
	if (is.null(colAlign)){
		colAlign <- c('l',rep('r',ncol(table)))
	}
	if (rowNames){
		table <- xtable(table,digits=rep(nDec,ncol(table)+1), align=colAlign)
		print(table, 
				sanitize.text.function = function(x){x},
				floating=FALSE, 
				hline.after=NULL,
				size="\\footnotesize",
				add.to.row=list(pos=list(-1,0, nrow(table)),command=c('\\toprule ','\\midrule ','\\bottomrule ')),
				file=fileName
		)
	}else{
		table <- xtable(table,digits=rep(nDec,ncol(table)+1), align=colAlign)
		print(table, 
				sanitize.text.function = function(x){x},
				floating=FALSE, 
				hline.after=NULL,
				size="\\footnotesize",
				include.rownames=FALSE,
				add.to.row=list(pos=list(-1,0, nrow(table)),command=c('\\toprule ','\\midrule ','\\bottomrule ')),
				file=fileName
		)
	}
}
