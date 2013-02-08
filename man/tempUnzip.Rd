\name{tempUnzip}
\alias{tempUnzip}
\title{Unzip series of compressed files, based on information in file list}
\description{
	Unzip a list of files of a particular type (specific by \code{data_type}) to the temporary directory.
	This is an alternative implementation of Anoop Shah's function \code{unzip_to_temp}.
}
\usage{
	temp_unzip(files, data_type, fromRAWPATH = TRUE)
}
\arguments{
	\item{files}{A \code{data.frame} containing the following information: zipfile name, filename, groupname (type of data contained in file).}
	\item{data_type}{The type of data contained in the zipfile, specified by the third column in the \code{files} object.}
	\item{fromRAWPATH}{Whether the file is in the RAWPATH directory, in which case only the filename is needed, otherwise the full path must be given in the data.frame.}
}
\seealso{
	\code{\link{unzip}}
}
\examples{
	###
	# tempUnzip(FILES, "clinical")
}