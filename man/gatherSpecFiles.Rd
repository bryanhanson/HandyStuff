\name{gatherSpecFiles}
\alias{gatherSpecFiles}
\title{
Process LoggerPro Spectral Files into a Data Frame
}
\description{
This function will go through all the files of a specified format in a directory and convert them into a data frame with one column containing the wavelength information and the other columns the absorbances of each sample (file).  The file names are used to create the column names in the data frame.  Optionally, non-integer wavelengths in the file can be combined to give integer wavelengths. Keep in mind that this function specifically modifies formats written by LoggerPro.  Each format, as it comes from LoggerPro, has various amounts of crap in it which has to be removed or modified.
}
\usage{
gatherSpecFiles(type = "txt", intLambda = FALSE, ...)
}
\arguments{
  \item{type}{
A character string giving the type of files to be processed.  Currently, either "txt", "csv" or "cmbl" extensions can be processed.
}
  \item{intLambda}{
Logical.  If TRUE, non-integer wavelengths that round to the same value will be combined and averaged and reported as integer values.
}
  \item{\dots}{
Other parameters to be passed downstream.  Currently none possible.
}
}
\details{
All files of a given extension in the directory will be processed, so make certain there are no extra files in the directory.  The files will be modified and written back out as .csv files so look for the number of files in the directory to double.  In the case of csv files, the original csv files will be overwritten.  These files have no header row.
}
\value{
A data frame containing the wavelengths in the first column and the absorbances in the other columns, one column per file, with the file name generating the column name.
}
\author{
Bryan A. Hanson, DePauw University
}
\keyword{ utilities }
