\name{writeLatexCountTable}
\alias{writeLatexCountTable}
\title{Wrapper for creating a LaTeX-ready table of counts and percentages (alternating columns) for incorporation into a project log}
\description{
	This is a wrapper that uses table() and prop.table() to create a table of counts and percentages.  
	The xtable library is then used to write a table in latex format to a file
	for incorporation into a project log.  There are no modifiable options - this has been designed to suit my specifications.  
	The table produced is compatible with the LaTeX \code{booktabs} package.
}
\usage{
	writeLatexTable(x, y, fileName)
}
\arguments{
	\item{x}{The row variable}
	\item{y}{The column variable}
	\item{fileName}{The filename}
}
\seealso{
	\code{\link{xtable}}
}

