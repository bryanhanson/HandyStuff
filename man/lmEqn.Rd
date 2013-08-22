\name{lmEqn}
\alias{lmEqn}
\alias{lmEqnL}
\title{
Plot Points, Fit a Line, Display Everything
}
\description{
These functions plot the points given, then fit a line and display the regression statistics using \code{ggplot2} or \code{lattice}.
}
\usage{
lmEqn(formula = NULL, data = NULL, method = "lm",
	leg.loc = c(0.5, 0.5), xlab = NULL, ylab = NULL,
	title = NULL, ...)
lmEqnL(formula = NULL, data = NULL, method = "lm",
	leg.loc = c(0.5, 0.5),poster = FALSE, ...)
}
\arguments{
  \item{formula}{
A formula with both LHS and RHS.  The formula should comply with the usual \code{R} standards, for instance \code{y ~ x}. 
}
  \item{data}{
A data frame containing two columns whose names correspond to the specified formula.
}
  \item{method}{
A character string.  Either "lm" for linear model or "rlm" for a robust fit.
}
  \item{leg.loc}{
A vector of two numbers (x, y) giving the location of the legend.  The values correspond to the units of the data. See the examples.
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
  \item{poster}{
Logical; if \code{TRUE}, use \code{posterThemeL} which has graphics settings suitable for viewing on a poster, otherwise, use \code{screenThemeL}.  Applies only to \code{lmEqnL} right now.
}

  \item{\dots}{
Additional arguments to be passed downstream.
}
}
\value{
A plot is drawn and the \code{ggplot2} or \code{lattice} object is returned, possibly for further manipulation.
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
conc = seq(1, 12, length.out = 8)
abs = jitter(conc, factor = 2)*0.1
cc <- data.frame(conc, abs)
lmEqn(formula = abs ~ conc, data = cc, leg.loc = c(3, 0.1),
	xlab = "concentration", ylab = "absorbance",
	title = "Calibration Curve")
lmEqnL(formula = abs ~ conc, data = cc, leg.loc = c(3, 0.1),
	xlab = "concentration", ylab = "absorbance",
	title = "Calibration Curve")
}
\keyword{ utilities }
\keyword{ plot }
