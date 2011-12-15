\name{theme_poster}
\alias{theme_poster}
\alias{undo_theme_poster}
\title{
ggplot2 theme for scientific posters
}
\description{
This function sets up a theme and geom defaults suitable for figures on scientific posters.
}
\usage{
theme_poster(base_size = 20)
}
\arguments{
  \item{base_size}{
The base font size used in figures.  Corresponds to the size of axis labels.  Titles are slightly larger and tick labels slightly smaller.
}
}
\value{
Returns a function which sets/resets point size, line width and text label sizes.
}
\author{
Bryan A. Hanson (hanson@depauw.edu)
}

\examples{
df <- data.frame(x = rnorm(10), y = rnorm(10))
p1 <- qplot(x = x, y = y, data = df, geom = c("point", "line"), main = "Test Plot")
print(p1) # default values

p2 <- p1 + theme_poster()
print(p2) # more suited to a poster

p3 <- p2 + undo_theme_poster()
print(p3) # values restored
}
\keyword{ themes }
\keyword{ utilities }
