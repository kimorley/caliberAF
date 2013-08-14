\name{writeLatexMeanTable}
\alias{writeLatexMeanTable}
\title{Function for creating summaries (mean, SD, N) of continuous variables and then writing a table in LaTeX format to file for incorporation into a project log}
\description{
	This function takes a data.table and a list of continuous variables and creates a summary table.
	Summaries can be produced for subgroups as well as the total sample.  A LaTeX format table is
	generated with all the results. The table produced is compatible with the LaTeX \code{booktabs} package.
}
\usage{
	writeLatexMeansTable(data, vars, byVar=NULL, varNames=NULL, fileName)
}
\arguments{
	\item{data}{A data.table object containing the variables to be summarised.}
	\item{vars}{A list of variable names (quoted) to be summarised.}
	\item{byVar}{A variable indicating subgroups (also quoted).}
	\item{varNames}{Human-readable versions of the variable names - row names for the table (optional - default is variable names).}
	\item{fileName}{The name of the file to write to.}
}
\seealso{
	\code{\link{xtable}, \link{data.table}}
}