\name{gatherCsv}
\alias{gatherCsv}
\title{
Combine csv Files Containing Spectral Data into a Data Frame
}
\description{
This function processes csv files containing two columns, wavelength and absorbance, into a data frame, which is then written out as a csv file.  The files should have no header row.
}
\usage{
gatherCsv()
}
\details{
It is assumed that the csv files have already been cleaned up so that they contain only wavelength and absorbance data.  The wavelength data column must be the same in all the files (as they would be if they came from the same instrument with the same settings).
}
\value{
A data frame containing the wavelengths in the first column and the absorbances in the other columns, one per file, with the file name generating the column name.  The data frame is written out in a file called "All Spec Files.csv".
}
\author{
Bryan A. Hanson, DePauw University
}
\seealso{
\code{\link{gatherSpecFiles}} which is the function the user should call.
}
\keyword{ utilities }
