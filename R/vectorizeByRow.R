##' Convert a Matrix into a Vector Row by Row
##'
##' This is one of several related convenience functions designed to convert
##' vectors into matrices or matrices into vectors, either by row or by column.
##' 
##' @param IN A matrix.
##'
##' @return A vector.
##' 
##' @name vectorizeByRow
##' @rdname vectorizeByRow
##' @export
##' @keywords utilities
##'
##' @seealso \code{\link{stackByColumn}}, \code{\link{stackByRow}},
##' \code{\link{vectorizeByColumn}}
##' 
##' @examples
##' vectorizeByRow(matrix(1:20, ncol = 4))

vectorizeByRow <- function(IN) {
	OUT <- rep(NA_real_, length(IN))
	nc <- ncol(IN)
	nr <- nrow(IN)
	a <- seq(1, length(IN), nc)
	b <- a + nc - 1
	for (n in 1:length(a)) {
		OUT[a[n]:b[n]] <- IN[n,]
		}
	OUT
	}
	