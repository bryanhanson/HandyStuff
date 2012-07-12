\name{qMS}
\alias{qMS}
\title{
Draw a Simple Mass Spectrum Showing the Parent Ion
}
\description{
Given a molecular formula, this function computes the mass of the parent ion, including any M + n peaks due to Br or Cl, and plots it.  Intended to draw the parent ion region for small organic molecules, especially those with Br or Cl.
}
\usage{
qMS(f = NULL, xlab = "m/z", ylab = "intensity",
	main = "Mass Spectrum", ...)
}
\arguments{
  \item{f}{A character string giving the molecular formula of the molecule of interest.
}
  \item{xlab}{
A character string giving the x axis label.
}
  \item{ylab}{
A character string giving the y axis label.
}
  \item{main}{
A character string giving the title of the plot.
}
  \item{\dots}{
Additional arguments to be passed downstream.
}
}
\details{
The function currently accepts formulas containing C, H, N, O, Br and Cl in any quantities.
}
\value{
Draws a plot.  Returns a data frame giving the peak masses and relative intensites.
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
ms <- qMS(f = "C5H8BrCl")
print(ms)
}
\keyword{utilities }
