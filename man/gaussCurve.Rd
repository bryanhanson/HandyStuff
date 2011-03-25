\name{gaussCurve}
\alias{gaussCurve}
\title{
Compute a Gaussian Curve
}
\description{
Computes the y values describing a Gaussian distribution given a range of x values and parameters for mu, sigma, and area.  A tail may be introduced into the curve to simulate the behavior of some chromatography peaks.  Not really intended for the end user, but no reason not to use it either.
}
\usage{
gaussCurve(x, area, mu, sigma, tail)
}
\arguments{
  \item{x}{
A vector of x values which will be used to compute the corresponding y values.  Use enough to give good resolution.
}
  \item{area}{
The area of the peak, in arbitrary units.
}
  \item{mu}{
The position of the peak.  Must fall in the range of x, of course.
}
  \item{sigma}{
The standard deviation of the peak.
}
  \item{tail}{
A value describing any tailing desired.  If NA, no tailing is applied.
}
}
\value{
A vector of y values corresponding to the x values supplied.
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{lorentzCurve}}, \code{\link{makeSpec}} which uses this function to make either spectra or chromatograms.
}
\examples{
myx <- seq(0, 100, length.out = 1000) # use lots of point for resolution
myy <- gaussCurve(x = myx, area = 1, mu = 40, sigma = 1.5, tail = NA)
plot(myx, myy, type = "l")
# Now with tailing
myy2 <- gaussCurve(x = myx, area = 1, mu = 40, sigma = 1.5, tail = 0.1)
plot(myx, myy2, type = "l")
}
\keyword{ utilities }
\keyword{ distributions }
