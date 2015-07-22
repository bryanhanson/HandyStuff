##' Lattice Panel Functions for Text-Decorated Levelplots
##'
##' These panel functions work in conjunction with levelplot
##' and label the cells of the plot with text.  \code{lpi}
##' labels with integers and \code{lpr} labels with real
##' numeric (real) values.
##'
##' @param x Numeric or integer vector, as in levelplot.
##'
##' @param y Numeric or integer vector, as in levelplot.
##'
##' @param z Numeric or integer vector, as in levelplot.
##'
##' @param ... Additional parameters to be passed to levelplot.
##'
##' @return None.  A level plot is displayed.
##' 
##' @name lpi
##' @aliases lpi, lpr
##' @rdname lpi
##' @export lpi
##' @export lpr
##' @keywords plot
##'
##' @examples
##' require('lattice')
##' #
##' ### Numeric test data
##' #
##' M1 <- abs(matrix(runif(16, 0.2, 1), nrow = 4))
##' rownames(M1) <- as.character(1:4)
##' colnames(M1) <- as.character(1:4)
##' # Negate the lower triangle values
##' M1[lower.tri(M1)] <- -1.0*M1[lower.tri(M1)]
##' diag(M1) <- 0.0
##' 
##' M2 <- t(M1[nrow(M1):1,])
##' levelplot(M2,
##' xlab.top = "column", ylab = "row", xlab = "",
##' scale = list(x = list(alternating = 2)),
##' col.regions = cm.colors(3),
##' at = c(-1.0, -0.01, 0.01, 1.0), panel = lpr)
##' #
##' ### Integer test data
##' #
##' M3 <- matrix(1:16, ncol = 4, byrow = TRUE)
##' rownames(M3) <- as.character(1:4)
##' colnames(M3) <- as.character(1:4)
##' 
##' M4 <- t(M3[nrow(M3):1,])
##' levelplot(M4,
##' xlab.top = "column", ylab = "row", xlab = "",
##' scale = list(x = list(alternating = 2)),
##' col.regions = heat.colors(26), panel = lpi)

lpi <- function(x, y, z, ...) {
	
	if (!requireNamespace("lattice", quietly = TRUE)) {
	stop("You need to install package lattice to use this function")
	}

	lattice::panel.levelplot(x, y, z, ...)
	lattice::panel.text(x, y, labels = sprintf("%i", z), cex = 1.75)
	}

lpr <- function(x, y, z, ...) {

	if (!requireNamespace("lattice", quietly = TRUE)) {
	stop("You need to install package lattice to use this function")
	}

	lattice::panel.levelplot(x, y, z, ...)
	lattice::panel.text(x, y, labels = sprintf("%.2f", z), cex = 1.75)
	}


