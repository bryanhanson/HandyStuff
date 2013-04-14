\name{rxnNorm}
\alias{rxnNormL}

\title{
Create a Reaction Norm or Interaction Plot
}
\description{
Draws a reaction norm or interaction plot as typically used in ecology to demonstrate treatment x genotype interactions.  The x axis can be categorical or numeric.  There are options to plot the raw data points in addition to a choice of several summary methods.  The central values may be connected or the entire data set fitted to a line.  A summary table is optional, and gives either counts at each level, an ANOVA table or fit data.
}
\usage{
rxnNormL(data = NULL, res = NULL, fac1 = NULL, fac2 = NULL,
	fac2cols = NULL, freckles = FALSE, type = "connect",
	method = c("sem", "iqr", "mad", "box", "sem95"), table = NULL,
	xlab = NULL, ylab = NULL, title = NULL, ...)
}

\arguments{
  \item{data}{
A data frame containing columns containing \code{res}, \code{fac1} and \code{fac2}.
}
  \item{res}{
A numeric vector giving the response (y values).
}
  \item{fac1}{
A factor or numeric vector giving the x values, which may be categorical or numeric.
}
  \item{fac2}{
A factor.  In a typical use would correspond to genotype.  A line in the specified color will be produced for each level of \code{fac2}.
}
  \item{fac2cols}{
A vector of color names as long as the levels of \code{fac2}.  The data and lines will be colored accordingly.
}
  \item{freckles}{
Logical; if TRUE, add the raw data points to the plot (as tiny points or \emph{freckles}, thanks Dana!).
}
  \item{type}{
Either "connect", "fitLine" or "anova".  In the first case, a line is drawn which connects the chosen measure of central tendency (e.g. mean) for each value of \code{fac1}.  This would generally be used when the x value are categorical.  In the second case, a linear fit is performed and the line is drawn; used for continuous x values.  In the third case, a connecting line is drawn along with an ANOVA summary table.
}
  \item{method}{
One of c("sem", "iqr", "mad", "box", "sem95").  Each produces a dot at the measure of central tendency and error bars corresponding to the requested method  ("box" currently does not work quite right).  Full details are in \code{\link[ChemoSpec]{seXy}}.
}
  \item{table}{
If NULL, the default, no action.  If a vector of 3 numbers \code{c(x, y, cex)}, a summary table is placed at the given coordinates with font size cex.  The table contains either a summary of counts (if \code{type = "connect"}), a report of the linear fit (if \code{type = "fitLine"}) or an ANOVA table (if \code{type = "anova"}.  The coordinates are in the units of the measurements \code{res} and \code{fac1}; \code{fac1} is internally handled as a number so if \code{fac1} has three levels then the setting \code{x = 2} would center the table over the middle level.
}
  \item{xlab}{
Character; a label for the x axis.
}
  \item{ylab}{
Character; a label for the y axis.
}
  \item{title}{
Character; a title for the plot.
}
  \item{\dots}{
Additional arguments to be passed downstream.
}
}
\details{
The choices in \code{method} are based upon functions in the \code{\link[ChemoSpec]{ChemoSpec}} package which will need to be installed.  See \code{\link[ChemoSpec]{seXy}} for the details.
}
\value{
A \code{lattice} plot object is returned invisibly, and a plot is created.
}

\references{
\url{http://github.com/bryanhanson/HandyStuff}

}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
require("ChemoSpec")
require("lattice")
require("gridExtra")
# set up demo data
set.seed(79)
res = c(rnorm(10, 5, 1.5), rnorm(10, 8, 2),
rnorm(10, 14, 2.0), rnorm(10, 10, 1.5),
rnorm(10, 15, 2), rnorm(10, 12,2.5))
fac1 <- c(rep("L", 20), rep("M", 20), rep("H", 20))
fac1 <- factor(fac1, levels = c("L", "M", "H"))
# fac2 <- c(rep("blue", 10), rep("red", 10))
# fac2 <- rep(fac2, 3)
fac2 <- sample(c("A", "B"), 60, replace = TRUE)
fac2 <- as.factor(fac2)
num <- c(rep(5, 20), rep(10, 20), rep(12, 20))
td <- data.frame(resp = res, fac1 = fac1, fac2 = fac2, num = num)

#
# Numeric x with lm and summary table:
p <- rxnNormL(formula = resp~num, groups = fac2, data = td,
method = "iqr", freckles = TRUE, type = "fitLine",
cols = c("red", "blue"), table = c(0.4, 0.3, 0.75),
main = "rxnNormL - linear model w/summary table")
print(p)
#
# Categorical x, connected and summary table:
p <- rxnNormL(formula = resp~fac1, groups = fac2, data = td,
method = "iqr", freckles = TRUE, type = "connect",
cols = c("red", "blue"), table = c(0.6, 0.3, 0.75),
main = "rxnNormL - Categorical x w/summary table")
print(p)
#
# Categorical x, connected with ANOVA table:
p <- rxnNormL(formula = resp~fac1, groups = fac2, data = td,
method = "iqr", freckles = TRUE, type = "anova",
cols = c("red", "blue"), table = c(0.6, 0.3, 0.75),
main = "rxnNormL - Categorical x w/summary table")
print(p)

}

\keyword{ univariate }
\keyword{ plot }