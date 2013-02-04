\name{lorentzCurve}
\alias{lorentzCurve}
\title{
Compute a Lorentzian Curve
}
\description{
Computes the y values describing a Lorentzian curve such as seen in an NMR peak. Requires a range of x values and parameters for peak position, area, and gamma, half the peak width at half-height.  Not really intended for the end user, but no reason not to use it either.
}
\usage{
lorentzCurve(x, x0, area, gamma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector of x values which will be used to compute the corresponding y values.  Use enough to give good resolution.
}
  \item{x0}{
The position of the peak.  Must fall in the range of x, of course.
}
  \item{area}{
The area of the peak, in arbitrary units.
}
  \item{gamma}{
HWHM, half-width at half-maximum.  The peak "width" in units corresponding to x.
}
}
\value{
A vector of y values corresponding to the x values supplied.
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{gaussCurve}}, \code{\link{makeSpec}}, \code{\link{plotNMRspec}} which uses this function to draw simple NMR spectra.
}
\examples{
myx <- seq(0, 100, length.out = 1000) # use lots of point for resolution
myy <- lorentzCurve(x = myx, area = 1, x0 = 40, gamma = 5)
plot(myx, myy, type = "l")
y = 0.5*max(myy)
x = seq(40, 45, 0.5)
points(x = x, y = rep(y, length(x)), col = "blue", type = "l")
text(x = 42, y = y + 0.005, labels = c("gamma"), col = "blue", srt = 90)
}
\keyword{ utilities }
\keyword{ distributions }
