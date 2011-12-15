\name{compareCats}
\alias{compareCats}
\title{
Compare Data by Category
}
\description{
This function allows one to generate a nice plot comparing a response from samples falling into one or two categories with corresponding levels.  \code{ggplot2} is used to create plots, with options to include the original data and to use various measures of central tendency and spread.  A count of the number of samples in each level is included.  Gives a visual impression of the data to go along with hypothesis tests.
}
\usage{
compareCats(data = NULL, res = NULL, fac1 = NULL, fac2 = NULL,
	fac1cols = NULL, freckles = FALSE,
	method = c("sem", "iqr", "mad", "box", "points", "sem95"),
	xlab = NULL, ylab = NULL, title = NULL, ...)
}
\arguments{
  \item{data}{
A data frame containing the response and categorical/factor data.  A maximum two categories with any number of levels can be plotted at one time.
}
  \item{res}{
A column name in the supplied data frame which gives the responses.  Should be in quotes.
}
  \item{fac1}{
A column name in the supplied data frame which gives the levels of factor/category 1. Should be in quotes.
}
  \item{fac2}{
A column name in the supplied data frame which gives the levels of factor/category 1. Should be in quotes. Optional.  If supplied, the plot will be faceted by the levels of \code{fac2}.
}
  \item{fac1cols}{
A vector of colors to be used for factor 1; must be as along as the levels in factor 1.
}
  \item{freckles}{
Logical; if \code{TRUE}, the actual data points are plotted as small points, hence the name, jittered slightly in the horizontal direction only.
}
  \item{method}{
One of \code{c("sem", "iqr", "mad", "box", "points", "sem95")}.  Various methods for computing measures of spread and central tendency.  See the documentation for \code{\link[ChemoSpec]{seXy}}.
}
  \item{xlab}{
Character; label for the x axis.
}
  \item{ylab}{
Character; label for the y axis.
}
  \item{title}{
Character; a title for the plot.
}
  \item{\dots}{
Other parameters to be passed downstream.
}
}
\details{
The supplied data frame is stripped down to just the response and factor columns, then \code{NAs} are removed; the counts reflect this.
}
\value{
A \code{ggplot2} object.
}
\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}\cr
\url{http://github.com/bryanhanson/HandyStuff}

}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
mydf <- data.frame(
	resp = rnorm(40),
	cat1 = sample(LETTERS[1:3], 40, replace = TRUE),
	cat2 = sample(letters[1:2], 40, replace = TRUE))
#	
p <- compareCats(data = mydf, res = "resp", fac1 = "cat1",
	fac2 = "cat2", method = "iqr", freckles = TRUE,
	fac1cols = c("red", "green", "blue"))
print(p)
#
# Interchange the roles of the factors (and hence colors):
#
p <- compareCats(data = mydf, res = "resp", fac1 = "cat2",
	fac2 = "cat1", method = "iqr", freckles = TRUE,
	fac1cols = c("red", "green"))
print(p)

}
\keyword{ graphics }
