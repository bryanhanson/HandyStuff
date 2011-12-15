\name{plotNMRspec}
\alias{plotNMRspec}
\title{
Create and Plot an NMR Spectrum
}
\description{
This function simulates simple NMR spectra.  Only 1st order coupling can be handled, and there is currently no capacity for doublet of doublets and other such peaks.  The field strength of the "instrument" is taken into account.
}
\usage{
plotNMRspec(peaks, x.range = c(12, 0), MHz = 300,
ppHz = 1, labels = TRUE, lab.pos = NULL, ...)
}
\arguments{
  \item{peaks}{
A data frame with the following columns: delta, mult (multiplicity), J, area, pw.  Multiplicity should be given by a number, so use 2 for a doublet.  J is in Hz.  pw is the peak width at half-height in Hz.
}
  \item{x.range}{
A numeric vector of length 2 giving the ppm range desired.
}
  \item{MHz}{
The operating frequency of the instrument, in MHz.
}

  \item{ppHz}{
Points per Hz: The number of data points per Hz to use in calculating the spectrum (passed as argument \code{dd} to \code{makeSpec}).  The default (1) works well for 1H NMR spectra.  For 13C NMR spectra, where the peaks are very narrow, one may need to increase the data density so that enough points define the peaks (a value of 4 is a good starting point).  Note that this function uses Hz internally so that the \code{x.range}, which is in ppm, is multiplied by \code{Mhz} before being sent to \code{\link{makeSpec}}, and once there, \code{makeSpec} will multiply it by \code{ppHz}.  Thus the total data points used is \code{ppHz * Mhz * abs(diff(x.range))}.  This approach ensures that peaks are not distorted when changing \code{x.range} for the same \code{peak.list}.
}

  \item{labels}{
Set to \code{FALSE} to turn off labels, such as with 13C NMR spectra.
}

  \item{lab.pos}{
A vector of label positions as along as the number of rows in \code{peaks} (the number of peaks in the spectrum).  A numeric vector (for now) where 2 = left and 4 = right.  This adjusts the positions of the labels to be either left or right of the peak as a way to avoid overlaps.
}

  \item{\dots}{
Other parameters to be passed downstream.
}
}
\value{
Returns a data frame of the type produced by \code{\link{makeSpec}}.  See there for details.  x values are in Hz.
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\note{
Adding capacity for dd etc. may not be that hard, except that the input data frame will have to change a fair amount.
}

\seealso{
\code{\link{lorentzCurve}}, \code{\link{makeSpec}}
}
\examples{
# A simulated 1H NMR spectrum
peaks1 <- data.frame(
	delta = c(1.3, 3.75, 3.9, 10.2),
	mult = c(3, 4, 2, 1),
	J = c(14, 14, 14, 0),
	area = c(3, 2, 1, 1),
	pw = c(2, 2, 2, 10))
#	
res <- plotNMRspec(peaks1, x.range = c(12, 0), MHz = 500,
	main = "500 MHz Simulated 1H NMR Spectrum")
#
# Compare to the same data at 200 MHz and plot together:
par(mfrow = c(2,1))
res <- plotNMRspec(peaks1, x.range = c(12, 0), MHz = 500,
	main = "500 MHz Simulated 1H NMR Spectrum")
res <- plotNMRspec(peaks1, x.range = c(12, 0), MHz = 200,
	main = "200 MHz Simulated 1H NMR Spectrum")
par(mfrow = c(1,1))
#
# Zoom in to show off:
par(mfrow = c(2,1))
res <- plotNMRspec(peaks1, x.range = c(4.5, 1), MHz = 500,
	main = "500 MHz Simulated 1H NMR Spectrum")
res <- plotNMRspec(peaks1, x.range = c(4.5, 1), MHz = 200,
	main = "200 MHz Simulated 1H NMR Spectrum")
par(mfrow = c(1,1))
#
# A simulated 13C NMR spectrum
# These are substantially slower due to the large
# chemical shift range
#
peaks2 <- data.frame(
	delta = c(160, 155, 145, 143, 135, 60, 32),
	mult = rep(1, 7),
	J = rep(1, 7),
	area = c(0.1, 0.3, 0.3, 1, 1, 0.5, 0.5),
	pw = rep(1, 7))
#
res <- plotNMRspec(peaks2, x.range = c(180, 0), MHz = 200,
	main = "200 MHz Simulated 13C NMR Spectrum", ppHz = 4,
	labels = FALSE)
#
# Repeat the above with ppHz = 1; note the peaks heights are not quite right
#	

}
\keyword{ utilities }
