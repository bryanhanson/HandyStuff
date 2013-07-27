\name{txt2csv}
\alias{txt2csv}
\alias{cmbl2csv}
\alias{sstab2csv}
\title{
Utility Functions to Clean and Convert Spectral Files to csv
}
\description{
These functions clean out extraneous information from exported spectral data files and then write them out in csv format.  The first two functions handle files exported by LoggerPro software.  The third handles files exported by Spectra Suite software.  Not directly called by the user.
}
\usage{
txt2csv(in.file = "", out.file = "")
cmbl2csv(in.file = "", out.file = "")
sstab2csv(in.file = "", out.file = "")
}
\arguments{
  \item{in.file}{
The name of the input file.
}
  \item{out.file}{
The name of the output file.
}
}
\details{
Extraneous text at the beginning of the file is removed.  In the case of cmbl files, lines containing "Z2" or ">" are removed.  Absorbances marked as "Z1" are replaced with zero.  The data are initially in one long column; the wavelength and absorbances are reunited into two columns.
}
\value{
A modifed file in csv format.
}
\author{
Bryan A. Hanson, DePauw University.
}

\seealso{
\code{\link{gatherSpecFiles}} which is the function the user should call. 
}
\keyword{ utilities }
