\name{makeSpec}
\alias{makeSpec}

\title{
Draw a Chromatogram or Spectrum
}
\description{
This function creates a chromatogram or spectrum from a list of appropriate parameters describing the peaks.  The individual curves are computed using the mathematical definition of either a Gaussian curve, possibly with tailing, or a Lorentzian curve.  Gaussian curves are appropriate for simulating chromatograms or UV-Vis spectra, while Lorentzians are generally used for simulating NMR peaks.  The function computes the individual curves as well as their sum (which is the whole chromatogram or spectrum).  A plot can be made, which may display the separate underlying curves.  If you want to draw NMR spectra, use \code{\link{plotNMRspec}} which is a much more natural interface to this function.
}

\usage{
makeSpec(peak.list, x.range, plot = TRUE, curves = FALSE,
	type = "gauss", noise = 0, dd = 1, ...)
}

\arguments{
  \item{peak.list}{
For a Gaussian curve, a data frame with the following columns: mu, sd, area, tail.  mu is the retention time (or center frequency).  sd is the standard deviation (or peak width).  area is the area under the peak.  tail is the tailing parameter - use NA when a pure Gaussian with no tailing is desired.  One row of the data frame contains data related to one peak.

For a Lorentzian curve, a data frame with the following columns: x0, area, gamma.  x0 is the center frequency or chemical shift.  gamma is the half the peak width at half-height.  area is the area under the peak.
}
  \item{x.range}{
A numeric vector of length 2 giving the retention time range (or frequency range) desired.  Must make sense in light of the peak list given (i.e. a wider range, possibly much wider depending up the values of \code{sd} and \code{tail}), as these broaden the peaks.
}
  \item{plot}{
Logical; if TRUE, a plot is produced.
}
  \item{curves}{
Logical; if TRUE, the individual curves are plotted (provided \code{plot = TRUE}.  Not very useful for NMR spectra, but great for showing, for instance, how shoulders arise on peaks in a chromatogram.
}
  \item{type}{
A character string.  Use "gauss" to generate Gaussian curves (for chromatograms, or UV-Vis spectra).  Use "lorentz" to generate Lorentzian curves as found in NMR spectra.
}
  \item{noise}{
A number giving the amount of noise to be added to the individual curves (the net spectrum has the noise from the individual spectra, it has no additional noise added to it).  Value corresponds to the argument \code{factor} in function \code{jitter}.
}

  \item{dd}{
The density of data points per unit of \code{x.range}.  The total number of data points used to create the spectrum or chromatogram is \code{dd*abs(diff(x.range))} and thus it also depends on the units of \code{x.range}.  This approach ensures that peaks are not distorted when changing \code{x.range} for the same \code{peak.list}.
}

\item{\dots}{
Additional arguments to be passed downstream.
}
}

\value{
A data frame containing the x values (retention times or frequencies) in the first row, and the complete chromatogram (spectrum) in the second row.  Additional rows contain chromatograms (spectra) of the individual components.  The row names of the data frame are character strings describing the chromatogram (spectrum) in that row.  The data frame contains 1,000 columns, as \code{xrange} is divided into 1,000 parts for computation of the corresponding y values.
}
\seealso{
\code{\link{gaussCurve}}, \code{\link{lorentzCurve}}, \code{\link{plotNMRspec}} the preferred interface for drawing NMR spectra.
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
# A fun little chromatogram:
chrom <- data.frame(mu = c(2, 5, 11), sd = c(0.5, 1, 2),
area = c(1, 0.5, 1), tail =  c(NA, NA, 0.1))
ex1 <- makeSpec(chrom, x.range = c(0, 20), plot = TRUE, curves = TRUE)
#
# Faux ethyl group NMR with J = 0.1 ppm.  Note that a much better
# NMR spectrum can be generated using plotNMRspec which also uses
# a more natural input format
spec <- data.frame(mu = c(3.5, 3.4, 3.3, 3.2, 1.4, 1.3, 1.2),
sd = rep(0.01, 7), tail =  rep(NA, 7),
area = c(1, 3, 3, 1, 1, 2, 1) * c(0.5, 0.5, 0.5, 0.5, 0.66, 0.66, 0.66))
ex2 <- makeSpec(spec, x.range = c(5, 0), plot = TRUE, curves = FALSE)
}

\keyword{ utility }

