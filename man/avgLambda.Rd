\name{avgLambda}
\alias{avgLambda}
\title{
Convert Wavelengths to Integer Values and Average Corresponding Absorbances
}
\description{
This function reads a csv file containing columns of wavelength and absorbances.  It rounds the wavelengths to integers and replaces the absorbances corresponding to the rounded values with their averages.  NOTE THAT ALL THE csv FILES IN THE CURRENT DIRECTORY ARE PROCESSED AND REPLACED WITH THE MODIFIED FILEs.  You should use this function on a copy of the directory.
}
\usage{
avgLambda()
}

\value{
The original files are overwritten with the modified files.
}
\author{
Bryan A. Hanson, DePauw University
}
\seealso{
\code{\link{gatherSpecFiles}} which is the function the user should call.
}
\keyword{ utilities }
