##' Convert a Vector into a Matrix Filling Across Rows
##'
##' This is one of several related convenience functions designed to convert
##' vectors into matrices or matrices into vectors, either by row or by column.
##' 
##' @param IN An integer or numeric vector.
##'
##' @param nrow The number of rows in the final matrix.
##'
##' @param ncol The number of columns in the final matrix.
##'
##' @return A ncol x nrow matrix with no attributes
##' 
##' @name stackByRow
##' @rdname stackByRow
##' @export
##' @keywords utilities
##'
##' @seealso \code{\link{stackByColumn}}, \code{\link{vectorizeByColumn}},
##' \code{\link{vectorizeByRow}}
##'
##' @details  This function is almost the same as
##' \code{\link{matrix}}, with \code{byrow = TRUE}, and
##' there are no dimnames or attributes in this function.
##' 
##' @examples
##' stackByRow(IN = 1:20, nrow = 5, ncol = 4)

stackByRow <- function(IN, nrow, ncol){
	if (length(IN)/nrow != ncol) stop("Dimensions don't make sense in stackByRow")
	OUT <- matrix(NA_real_, nrow = nrow, ncol = ncol)
	idx <- seq(ncol, length(IN), ncol)
	for (n in 1:nrow){
		OUT[n,] <- IN[(idx[n]-ncol+1):(idx[n])]
		}
	OUT
	}
