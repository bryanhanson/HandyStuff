\name{compareCatsL}
\alias{compareCatsL}
\title{
Compare Data by Category
}
\description{
This function allows one to generate a nice plot comparing a response from samples falling into one or two categories with corresponding levels.  \code{lattice} is used to create plots, with options to include the original data and to use various measures of central tendency and spread.  A count of the number of samples in each level is included.  Gives a visual impression of the data to go along with hypothesis tests.
}
\usage{
compareCatsL(formula = NULL, data = NULL,
	cols = NULL, freckles = FALSE,
	method = c("sem", "sem95", "iqr", "mad", "box", "points"),
	poster = FALSE, ...)
}
\arguments{
  \item{formula}{
A formula with both LHS and RHS.  The formula should comply with the usual \code{lattice} conventions, for instance \code{y ~ factor1 | factor2}. 
}
  \item{data}{
A data frame containing the response and categorical/factor data.  A maximum two categories with any number of levels can be plotted at one time.
}
  \item{cols}{
A vector of colors to be used for factor 1; must be as along as the levels in factor 1.
}
  \item{freckles}{
Logical; if \code{TRUE}, the actual data points are plotted as small points, hence the name, jittered slightly in the horizontal direction only.
}
  \item{method}{
One of \code{c("sem", "sem95", "iqr", "mad", "box", "points")}.  Various methods for computing measures of spread and central tendency.  See the documentation for \code{\link[ChemoSpec]{seXy}}.
}
  \item{poster}{
Logical; if \code{TRUE}, use \code{posterThemeL} which has graphics settings suitable for viewing on a poster, otherwise, use \code{screenThemeL}.
}

  \item{\dots}{
Other parameters to be passed downstream.
}
}
\details{
The supplied data frame is stripped down to just the response and factor columns, then \code{NAs} are removed; the counts reflect this.
}
\value{
A \code{lattice} object.  These can be modified by the usual methods associated with these packages.
}
\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}\cr
\url{http://github.com/bryanhanson/HandyStuff}

}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
library("ChemoSpec")
mydf <- data.frame(
	resp = rnorm(40),
	cat1 = sample(LETTERS[1:3], 40, replace = TRUE),
	cat2 = sample(letters[1:2], 40, replace = TRUE))
#
# One factor:
#
p <- compareCatsL(formula = resp~cat1, data = mydf,
	method = "sem", freckles = TRUE, poster = FALSE, 
	cols = c("red", "orange", "blue"))
print(p)
#
# Two factors:
#	
p <- compareCatsL(formula = resp~cat1 | cat2, data = mydf,
	method = "sem", freckles = TRUE, poster = FALSE,
	cols = c("red", "orange", "blue"))
print(p)
#
# Interchange the roles of the factors (and hence colors):
#
p <- compareCatsL(formula = resp~cat2 | cat1, data = mydf,
	method = "sem", freckles = TRUE, poster = FALSE,
	cols = c("red", "blue"))
print(p)

}
\keyword{ plot }
\keyword{ univariate }

