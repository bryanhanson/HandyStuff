\name{examplePage}
\alias{examplePage}
\title{
Create an html Page Showing All Examples in Rd Files
}
\description{
This function runs all the example code in a package's Rd files, using \code{knitr} to create a \code{markdown} file.  This file is then converted to html and opened in a browser.  This function was written by Sasha Epskamp and published on his blog (sachaepskamp.com/blog/HTMLexamples).  I modified it to accept more than one file for \code{exclude}.
}
\usage{
examplePage(pkg,
	openChunk = "```{r, message=FALSE, warning = FALSE, error = FALSE}",
	includeDontshow = FALSE, includeDontrun = TRUE, exclude)
}
\arguments{
  \item{pkg}{
Path to the package directory.
}
  \item{openChunk}{
Character.  The default opening markdown chunk.
}
  \item{includeDontshow}{
Logical indicating whether to include the 'Dontshow' sections of the Rd file.
}
  \item{includeDontrun}{
Logical indicating whether to include the 'Dontrun' sections of the Rd file.
}
  \item{exclude}{
Character vector listing the Rd files to skip.  For instance, \code{c("func1.Rd", "func2.Rd"}.
}
}
\value{
Creates and displays a web page.
}
\references{
sachaepskamp.com/blog/HTMLexamples
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}