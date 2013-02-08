\name{convertICD10}
\alias{convertICD10}
\title{Expands short (3/4 character) ICD-10 codes to format used in HES database.}
\description{
	Converts ICD-10 codes to the format entered into HES (appending 'X', '-', and 'X-').
	All other columns in the \code{data.frame} or \code{data.table} will be repeated.
}
\usage{
	convertICD10(data)
}
\arguments{
	\item{data}{A \code{data.frame} or \code{data.table} that contains the following column: \code{icd10code}, 
	which contains the ICD-10 codes of interest.}
}
\examples{
	data <- data.frame(icd10code='I48',icd10term='Atrial fibrillation/flutter',hesclinrank='1')
	newdata <- convertICD10(data)	
}