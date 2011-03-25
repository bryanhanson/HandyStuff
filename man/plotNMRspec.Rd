\name{plotNMRspec}
\alias{plotNMRspec}
\title{
Create and Plot an NMR Spectrum
}
\description{
This function simulates simple NMR spectra.  Only 1st order coupling can be handled, and there is currently no capacity for doublet of doublets and other such peaks.  The field strength of the "instrument" is taken into account.
}
\usage{
plotNMRspec(peaks, x.range = c(12, 0), MHz = 300, ...)
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
# A simulated spectrum
peaks1 <- data.frame(
	delta = c(1.3, 3.75, 3.9, 10.2),
	mult = c(3, 4, 2, 1),
	J = c(14, 14, 14, 0),
	area = c(3, 2, 1, 1),
	pw = c(2, 2, 2, 10))
#	
res <- plotNMRspec(peaks1, x.range = c(12, 0), MHz = 500,
	main = "500 MHz Simulated NMR Spectrum")
#
# Compare to the same data at 200 MHz and plot together:
par(mfrow = c(2,1))
res <- plotNMRspec(peaks1, x.range = c(12, 0), MHz = 500,
	main = "500 MHz Simulated NMR Spectrum")
res <- plotNMRspec(peaks1, x.range = c(12, 0), MHz = 200,
	main = "200 MHz Simulated NMR Spectrum")
par(mfrow = c(1,1))
#
# Zoom in to show off:
par(mfrow = c(2,1))
res <- plotNMRspec(peaks1, x.range = c(4.5, 1), MHz = 500,
	main = "500 MHz Simulated NMR Spectrum")
res <- plotNMRspec(peaks1, x.range = c(4.5, 1), MHz = 200,
	main = "200 MHz Simulated NMR Spectrum")
par(mfrow = c(1,1))
}
\keyword{ utilities }
