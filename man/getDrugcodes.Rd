\name{getDrugcodes}
\alias{getDrugcodes}
\title{Retrieve drug codes based on list of drug names}
\description{
	Takes a list of drug names and returns all the drug codes and product names relating to that drug.
	Object returned is a list of data.frames, one for each drug.
	It requires the CALIBER product lookup table (this is loaded automatically by the function).
	This function is based on code written by Anoop Shah.
}
\usage{
	getDrugcodes(drugList)
}
\arguments{
	\item{drugList}{A vector of drug names (case is ignored).}
}
\seealso{
	\code{\link{unzip}}
}
\examples{
	###
	# drugcodes <- getDrugcodes(drugList=c('amiodarone', 'dronedarone', 'propafenone', 'flecainide', 'sotalol','quinidine', 'metoprolol', 'bisoprolol', 'esmolol', 'carvedilol'))
}