\name{jSeq}
\alias{jSeq}
\title{
Special Sequences for Drawing NMR Peaks
}
\description{
This function creates sequences, centered on zero, which correspond to odd or even NMR multiplets.  Not intended for direct use. Called by \code{\link{plotNMRspec}}.
}
\usage{
jSeq(length.out)
}
\arguments{
  \item{length.out}{
An integer giving the number of peaks in the sequence.
}
}
\value{
A vector describing the spacing of the parts of an NMR multiplet in terms of multiples of the coupling constant, J.
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{plotNMRspec}} which calls this function.
}
\examples{
tmp <- jSeq(5) # a multiplet with an odd number of peaks
tmp
tmp <- jSeq(6) # an even number
tmp
}
\keyword{ utilities }
