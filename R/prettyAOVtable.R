#' 
#' Create a nicely formated analysis of variance table as a grob
#' 
#' Creates a nicely formated tableGrob using \code{gridExtra} and \code{broom}.
#' 
#' @param aov.object An object of class \code{aov}.
#' 
#' @param cex Numeric.  The relative size of text in the table.  The size of the cells
#' and table automatically adjust for the size of the text.
#' 
#' @return A \code{gtable} / \code{grob} object which can be plotted.  See the examples.
#' 
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#' @references \url{https://github.com/bryanhanson/HandyStuff}
#' @keywords utilities
#'
#' @export prettyAOVtable
#' 
#' 
#' @examples
#' 
#' require("broom")
#' require("grid")
#' require("gridExtra")
#' require("gtable")
#'
#' npk1.aov <- aov(yield ~ block + N*P*K, npk) # from ?aov
#' grid.newpage()
#' grid.draw(prettyAOVtable(npk1.aov))
#' npk2.aov <- aov(yield ~ block*K, npk)
#' grid.newpage()
#' grid.draw(prettyAOVtable(npk2.aov))
#' 
prettyAOVtable <- function(aov.object = NULL, cex = 1.0) {

	if (!requireNamespace("broom", quietly = TRUE)) {
		stop("You need to install package broom to use this function")
		}
	if (!requireNamespace("grid", quietly = TRUE)) {
		stop("You need to install package grid to use this function")
		}
	if (!requireNamespace("gridExtra", quietly = TRUE)) {
		stop("You need to install package gridExtra to use this function")
		}
	if (!requireNamespace("gtable", quietly = TRUE)) {
		stop("You need to install package gtable to use this function")
		}
	
	if (is.null(aov.object)) stop("You must provide an object of class aov")
	
	# Clean up & format the table
	
	DF <- as.data.frame(broom::tidy(aov.object))
	DF[,3:5] <- round(DF[,3:5], 2)
	DF[,6] <- format(DF[,6], scientific = TRUE, digits = 4)
	DF[nrow(DF), c(5,6)] <- "" # remove NA, leave cell blank
	names(DF)[3:6] <- c("sum sqs", "mean sqs", "F statistic", "p value")
	nr <- nrow(DF)
	
	# Create a theme that will right justify so numbers line up
	
	PATtheme<- gridExtra::ttheme_default(
		core = list(fg_params = list(hjust = 1, x = 0.9, cex = cex)),
		rowhead = list(fg_params = list(hjust = 1, x = 0.95, cex = cex)),
		colhead = list(fg_params = list(cex = cex)))

	# Now assemble & pretty-up the grob
	
	g <- gridExtra::tableGrob(DF, rows = NULL, theme = PATtheme)
	g2 <- gtable::gtable_add_grob(g, 
			grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)), 
			t = 2, b = nrow(g), l = 1, r = ncol(g))
	g3 <- gtable::gtable_add_grob(g2, 
			grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)), 
			t = 1, l = 1, r = ncol(g2))
	g4 <- gtable::gtable_add_grob(g3, 
			grobs = grid::segmentsGrob(y1 = grid::unit(0,"npc"), gp = grid::gpar(lwd = 2.0)), 
			t = nr, b = nr-1, l = 1, r = ncol(g3))
	return(g4)
	}

