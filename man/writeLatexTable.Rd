\name{writeLatexTable}
\alias{writeLatexTable}
\title{Wrapper for using xtable to write table in LaTeX format to file for incorporation into a project log}
\description{
	This is a simple wrapper for the xtable library which is used to write a table in latex format to a file
	for incorporation into a project log.  It does not do anything beyond the standard xtable commands and
	has been created simply for improving efficiency and simplicity of other code.
	The table produced is compatible with the LaTeX \code{booktabs} package.
}
\usage{
	writeLatexTable(table, fileName, colNames=NULL, colAlign=NULL, nDec=0)
}
\arguments{
	\item{table}{The object to be printed in LaTeX format, created e.g. using \code{table} or \code{xtabs}.}
	\item{fileName}{The name of the file to write to.}
	\item{colNames}{Optional column names for the table (by default the categories of the variable are used).}
	\item{colAlign}{Alignment of columns in table.  Can take standard LaTeX options of left-aligned (l), 
	right-aligned (r), and centre (c).  Can be provided as a single variable (e.g. 'rllc') or vector (e.g. c('r','l','c')).  
	Default  will generate table in which the first column is left-justified and all others are right-justified.}
	\item{nDec}{Number of decimal places to print (default is none).}
}
\seealso{
	\code{\link{xtable}}
}
\examples{
	\dontrun{
	## Not run
	data = data.frame(x=rep(c("a","b","c"),each=3), y=c(1,1,6,7,3,12,1,3,7), v=1:9)
	writeLatexTable(table=table(data$y,data$x), fileName='tmp', colNames=c('col1','col2','col3'), colAlign=c('r','l','c'))
	}
}