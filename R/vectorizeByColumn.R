##' Convert a Matrix into a Vector Column by Column
##'
##' This is one of several related convenience functions designed to convert
##' vectors into matrices or matrices into vectors, either by row or by column.
##' 
##' @param IN A matrix.
##'
##' @return A vector.
##' 
##' @name vectorizeByColumn
##' @rdname vectorizeByColumn
##' @export
##' @keywords utilities
##'
##' @seealso \code{\link{stackByColumn}}, \code{\link{stackByRow}},
##' \code{\link{vectorizeByRow}}
##' 
##' @examples
##' vectorizeByColumn(matrix(1:20, ncol = 4))

vectorizeByColumn <- function(IN) {
	OUT <- rep(NA_real_, length(IN))
	nc <- ncol(IN)
	nr <- nrow(IN)
	a <- seq(1, length(IN), nr)
	b <- a + nr - 1
	for (n in 1:length(a)) {
		OUT[a[n]:b[n]] <- IN[,n]
		}
	OUT
	}
