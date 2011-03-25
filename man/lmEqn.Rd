\name{lmEqn}
\alias{lmEqn}
\title{
Plot Points, Fit a Line, Display Everything
}
\description{
This function plots the points given, then fits a line and displays the regression statistics using \code{ggplot2}.
}
\usage{
lmEqn(df = NULL, y = NULL, x = NULL, method = "lm",
	leg.loc = c(0, 0), xlab = NULL, ylab = NULL,
	title = NULL, ...)
}
\arguments{
  \item{df}{
A data frame containing two columns whose names correspond to the \code{x} and \code{y} given next.
}
  \item{y}{
A character string giving a valid column name from \code{df}, to be used as the y values.
}
  \item{x}{
A character string giving a valid column name from \code{df} to be used as the x values.
}
  \item{method}{
A character string.  Either "lm" for linear model or "rlm" for a robust fit.
}
  \item{leg.loc}{
A vector of two numbers (x, y) giving the location of the legend.  The values correspond to the units of the data.  See the example.
}
  \item{xlab}{
A character string giving the x axis label.
}

  \item{ylab}{
A character string giving the y axis label.
}
  \item{title}{
A character string giving the plot title.
}

  \item{\dots}{
Additional arguments to be passed downstream.
}
}
\value{
A plot is drawn and the \code{ggplot2} object is returned, possibly for further manipulation.
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
conc = seq(1, 12, length.out = 8)
abs = jitter(conc, factor = 2)*0.1
cc <- data.frame(conc, abs)
lmEqn(df = cc, x = "conc", y = "abs", leg.loc = c(3, 0.1),
	xlab = "concentration", ylab = "absorbance",
	title = "Calibration Curve")
}
\keyword{ utilities }
\keyword{ plot }
