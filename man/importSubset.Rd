\name{importSubset}
\alias{importSubset}
\title{Import subset of data from gzip file.}
\description{
	Imports subset of data from a gzip .csv file based on (a) column names and (b) filtering of particular column.
	Modification of Anoop's code that works on uncompressed files.
}
\usage{
	importSubset(fileName, filterColName=NULL, filterList=NULL, keepColNames=c('anonpatid', 'eventdate', 'medcode'), dateColNames='eventdate')
}
\arguments{
	\item{fileName}{The name of the file from which data is to be imported.}
	\item{filterColName}{The name (header) of the column which is to be filtered on.}
	\item{filterList}{The list of values that can occur in \code{filterColName} that will result in the row being selected for import.}
	\item{keepColNames}{Names of the columns which should be retained for rows identified for import.}
}
\seealso{
	\code{\link{unzip}}
}
\examples{
	###
	# drugcodes <- getDrugcodes(druglist=c('amiodarone', 'dronedarone', 'propafenone', 'flecainide', 'sotalol','quinidine', 'metoprolol', 'bisoprolol', 'esmolol', 'carvedilol'))
}