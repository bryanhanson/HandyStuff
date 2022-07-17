#' Lattice themes resembling ggplot2 themes
#' 
#' This is a set of theme details for use with \code{\link{compareCats}},
#' \code{\link{rxnNorm}} and \code{\link{lmEqn}}.  The themes are derived from package
#' \code{latticeExtra} with a few additional customizations.  The settings for
#' posterTheme are generally larger for decent viewing on a poster.
#' 
#' @return A list containing the plotting theme details.
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#'
#' @seealso \code{\link{compareCats}} and \code{\link{rxnNorm}} for examples
#' of this theme in use.
#' @references \url{https://github.com/bryanhanson/HandyStuff}
#' @keywords utilities
#' @export screenTheme
#' @export posterTheme
#' @aliases screenTheme, posterTheme
#'
screenTheme <- function() {
	
	# lattice settings resembling ggplot2 for use on screens
	# Values & concept derived from latticeExtra::ggplot2like()
	# Also see trellis.par.get()
	
	L <- list(
	
	# background, grids etc
	axis.line = list(col = "transparent"),
	axis.text = list(lineheight = 0.9, col = "grey50"), # fontsize below
	panel.background = list(col = "grey90"),
	reference.line = list(col = "white"),
	strip.background = list(col = c("grey70", "grey50", "grey40")),
	strip.border = list(col = "transparent"),
	
	# overall font and point size
	fontsize = list(text = 12, points = 10),
	
	# symbols and lines
	plot.symbol = list(pch = 20, cex = 1.5), # controls mean/median point size
	superpose.symbol = list(cex = 0.5, pch = 20), # controls freckles
#	superpose.line = list(lwd = 5), # does nothing?
	add.line = list(lwd = 1), # controls summary lines in rxnNorm
	# par.main.text = list(cex = 1.5), # individual control if desired
	# par.xlab.text = list(cex = 1.2),
	# par.ylab.text = list(cex = 1.2),
	# par.sub.text = list(cex = 1.2),
	# add.text = list(cex = 1.2), # lengend entries, not legend title
	
	# these next ones apply to boxplots (not an option in rxnNorm)
    box.dot = list(col = "black", pch = 18, cex = 1.5),
    box.rectangle = list(col = "transparent"),
    box.umbrella = list(lty = 1, lwd = 1.5)
    # The fill of the bwplot is controlled in drawing code
	)
	L
	}
