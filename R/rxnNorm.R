#' Create a Reaction Norm or Interaction Plot
#' 
#' Draws a reaction norm or interaction plot as typically used in ecology to
#' demonstrate treatment x genotype interactions.  The x axis can be
#' categorical or numeric.  There are options to plot the raw data points in
#' addition to a choice of several summary methods.  The central values may be
#' connected or the entire data set fitted to a line.  A summary table is
#' optional, and gives either counts at each level, an ANOVA table or fit data.
#' 
#' The choices in \code{method} are based upon functions in the
#' \code{\link[ChemoSpec]{ChemoSpec}} package which will need to be installed.
#' See \code{ChemoSpec::.seXy} for the details.
#' 
#' @param formula A formula with both LHS and RHS.  The formula should comply
#' with the usual \code{lattice} conventions, for instance \code{y ~ x_factor}.
#' @param data A data frame containing the response and categorical/factor
#' data, and possibly group data.
#'
#' @param groups The (unquoted) name of a variable in \code{data} to serve as
#' the grouping variable.
#'
#' @param cols A vector of colors to be used for factor 1; must be as along as
#' the levels in factor 1.
#'
#' @param freckles Logical; if \code{TRUE}, the actual data points are plotted
#' as small points, hence the name, jittered slightly in the horizontal
#' direction only.
#'
#' @param type Either "connect", "fitLine" , "anova" or "anovaNoLine".
#' In the first case, a
#' line is drawn which connects the chosen measure of central tendency (e.g.
#' mean) for each value of \code{fac1}.  This would generally be used when the
#' x value are categorical.  In the second case, a linear fit is performed and
#' the line is drawn; used for continuous x values.  In the third case, a
#' connecting line is drawn along with an ANOVA summary table.  Finally, in
#' the fourth case no line is drawn and the ANOVA summary table is provided.
#'
#' @param method One of \code{c("sem", "sem95", "iqr", "mad")}.  Each produces
#' a dot at the measure of central tendency and error bars corresponding to the
#' requested method ("box" currently does not work quite right).  Full details
#' are in \code{ChemoSpec::.seXy}.
#'
#' @param table If NULL, the default, no action.  If a vector of 3 numbers
#' \code{c(x, y, cex)}, a summary table is placed at the given coordinates with
#' font size cex.  The table contains either a summary of counts (if \code{type
#' = "connect"}), a report of the linear fit (if \code{type = "fitLine"}) or an
#' ANOVA table (if \code{type = "anova"}.  The coordinates are in device units
#' so 0.5, 0.5 puts the table dead center in the plotting region.
#'
#' @param legend If NULL, no action.  If a vector of 3 numbers
#' \code{c(x, y, cex)}, a legend is placed at the given coordinates with
#' font size cex.  The coordinates are in device units
#' so 0.5, 0.5 puts the legend dead center in the plotting region.
#'
#' @param theme Character; A suitable \code{lattice} theme.  
#' There are two built-in themes which you can use "as is" or modify to your heart's
#' content.  If  none is given, \code{\link{screenTheme}} will be used.  The other option
#' provided is \code{\link{posterTheme}}.
#'
#' @param \dots Additional arguments to be passed downstream.
#'
#' @return A \code{lattice} plot object is returned invisibly, and a plot is
#' created.
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#' @references \url{http://github.com/bryanhanson/HandyStuff}
#' @keywords univariate plot
#' @examples
#' #
#' ### set up demo data
#' #
#' require("ChemoSpec")
#' require("lattice")
#' require("gridExtra")
#'
#' set.seed(79)
#' res = c(rnorm(10, 5, 1.5), rnorm(10, 8, 2),
#' rnorm(10, 14, 2.0), rnorm(10, 10, 1.5),
#' rnorm(10, 15, 2), rnorm(10, 12,2.5))
#' fac1 <- c(rep("L", 20), rep("M", 20), rep("H", 20))
#' fac1 <- factor(fac1, levels = c("L", "M", "H"))
#' fac2 <- sample(c("A", "B"), 60, replace = TRUE)
#' fac2 <- as.factor(fac2)
#' num <- c(rep(5, 20), rep(10, 20), rep(12, 20))
#' td <- data.frame(resp = res, fac1 = fac1, fac2 = fac2, num = num)
#' #
#' ### Numeric x with lm and summary table:
#' #
#' p <- rxnNorm(formula = resp~num, groups = fac2, data = td,
#' method = "iqr", freckles = TRUE, type = "fitLine",
#' cols = c("red", "blue"), table = c(0.5, 0.3, 0.75),
#' legend = c(0.175, 0.84, 1.0),
#' main = "rxnNorm - linear model w/fit table")
#' print(p)
#' #
#' ### Categorical x, connected and summary table:
#' #
#' p <- rxnNorm(formula = resp~fac1, groups = fac2, data = td,
#' method = "iqr", freckles = TRUE, type = "connect",
#' cols = c("red", "blue"), table = c(0.5, 0.3, 0.75),
#' legend = c(0.175, 0.84, 1.0),
#' main = "rxnNorm - Categorical x w/summary table")
#' print(p)
#' #
#' ### Categorical x, connected with ANOVA table:
#' #
#' p <- rxnNorm(formula = resp~fac1, groups = fac2, data = td,
#' method = "iqr", freckles = TRUE, type = "anova",
#' cols = c("red", "blue"), table = c(0.57, 0.2, 0.75),
#' legend = c(0.175, 0.84, 1.0),
#' main = "rxnNorm - Categorical x w/ANOVA table")
#' print(p)
#' 
#' 
#' @export
#' 
rxnNorm <-
function(formula = NULL, data = NULL, groups = NULL,
	cols = NULL, freckles = FALSE, type = "connect",
	method = c("sem", "sem95", "iqr", "mad"),
	table = NULL, legend = c(0.5, 0.5, 1.0),
	theme = screenTheme(), ...) {

# Function to compare categorical data by
# plotting means etc in the same panel
# Options to connect the dots or do a fit
# and display some nice summary tables
# Bryan Hanson, DePauw Univ, July 2010

# Lattice version, February 2013
# We are using formula, data and groups close to how lattice handles them

	if (!requireNamespace("lattice", quietly = TRUE)) {
		stop("You need to install package lattice to use this function")
		}
	if (!requireNamespace("latticeExtra", quietly = TRUE)) {
		stop("You need to install package latticeExtra to use this function")
		}
	if (!requireNamespace("plyr", quietly = TRUE)) {
		stop("You need to install package plyr to use this function")
		}
	if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
		stop("You need to install package RColorBrewer to use this function")
		}
	if (!requireNamespace("ChemoSpec", quietly = TRUE)) {
		stop("You need to install package ChemoSpec to use this function")
		}
	if (!requireNamespace("gridExtra", quietly = TRUE)) {
		stop("You need to install package gridExtra to use this function")
		}
	if (!requireNamespace("gtable", quietly = TRUE)) {
		stop("You need to install package gtable to use this function")
		}
	if (!requireNamespace("grid", quietly = TRUE)) {
		stop("You need to install package grid to use this function")
		}

	# Check and process the formula & data types
	
	if (is.null(formula)) stop("Formula not given")
	if (!plyr::is.formula(formula)) stop("Invalid formula specification")

	yy <- as.character(formula[[2]])
	if (!length(formula[[3]]) == 1) stop("Formula must be of type y ~ x")
	if (length(formula[[3]]) == 1) {
		xx <- as.character(formula[[3]])
		}

	grn <- as.character(match.call()$groups) # the name
	if (!is.factor(data[,grn])) stop("Groups must be a factor")
	if (type == "fitLine") {
		if (!is.numeric(data[,xx])) {
			msg <- paste(xx, "must be numeric to fit a line")
			stop(msg)
			}
		}
	# Need to allow for groups = NULL above ^^^^^^^^
	# Also, cols depends upon groups so we might have to make groups = 1 if not given
	
	# Reduce the df and clean it of NAs
	# otherwise there is trouble with the counts
	
	keep <- c(yy, xx, grn)
	data <- data[, keep]
	data <- stats::na.omit(data)

	args <- as.list(match.call(expand.dots = FALSE)[-1]) # used a bit later
	
	# Set up the color scheme if not provided
	
	nc <- length(levels(data[,grn]))
	if (is.null(cols)) {
		cols <- RColorBrewer::brewer.pal(nc, "Set1")
		if (nc == 2) cols <- cols[1:2] # cannot keep the extra due to recycling problems
		}
	if (!is.null(cols)) {
		if (!length(cols) == nc) stop("cols must be the same length as the levels in groups")
		}

### This panel will compute & plot summary statistics
	
	panel.summary <- function(x, y, ...) {
		
		mean <- stats::aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = mean)
		med <- stats::aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = stats::median)
		sexy <- stats::aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = ChemoSpec::.seXy)
		sexy95 <- stats::aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = ChemoSpec::.seXy95)
		sexymad <- stats::aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = ChemoSpec::.seXyMad)
		sexyiqr <- stats::aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = ChemoSpec::.seXyIqr)
		sumDat <- cbind(mean, med[,3], sexy[[3]][,c(2,3)],
			sexy95[[3]][,c(2,3)], sexymad[[3]][,c(2,3)], sexyiqr[[3]][,c(2,3)])
		names(sumDat) <- c("xx", "gr", "mean", "median",
			"semL", "semU", "sem95L", "sem95U", "madL", "madU",
			"iqrL", "iqrU")
		gr <- NULL # Needed for next command to fake out check mechanism
		sumDat <- plyr::arrange(sumDat, xx, gr)
		sumDat$cols <- rep(cols, length(unique(sumDat$xx)))

		for (i in 1:nrow(sumDat)) {
		
			# Plot the mean or median
					
			if ((method == "sem") | (method == "sem95")) {
				lattice::panel.points(x = sumDat[i,1], y = sumDat[i,3], col = sumDat[i, 13], pch = 19, ...)
				}
				
			if ((method == "mad") | (method == "iqr")) {
				lattice::panel.points(x = sumDat[i,1], y = sumDat[i,4], col = sumDat[i, 13], pch = 19, ...)
				}
	
			# Plot the 'whiskers'
	
			if (method == "sem") {
				lattice::panel.segments(x0 = sumDat[i,1], y0 = sumDat[i,5],
				x1 = sumDat[i,1], y1 = sumDat[i,6], lwd = 3, col = sumDat[i,13], ...)		
				}
	
			if (method == "sem95") {
				lattice::panel.segments(x0 = sumDat[i,1], y0 = sumDat[i,7],
				x1 = sumDat[i,1], y1 = sumDat[i,8], lwd = 3, col = sumDat[i, 13], ...)		
				}
	
			if (method == "mad") {
				lattice::panel.segments(x0 = sumDat[i,1], y0 = sumDat[i,9],
				x1 = sumDat[i,1], y1 = sumDat[i,10], lwd = 3, col = sumDat[i, 13], ...)		
				}
	
			if (method == "iqr") {
				lattice::panel.segments(x0 = sumDat[i,1], y0 = sumDat[i,11],
				x1 = sumDat[i,1], y1 = sumDat[i,12], lwd = 3, col = sumDat[i, 13], ...)		
				}
			}
			
		} # end of panel.summary

### This panel will connect the means or medians
	
	panel.connect <- function(x, y, ...) {
		
		mean <- stats::aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = mean)
		med <- stats::aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = stats::median)
		sumDat <- cbind(mean, med[,3])
		names(sumDat) <- c("xx", "gr", "mean", "median")
		gr <- NULL # needed to fake out check mechanism for next command
		sumDat <- plyr::arrange(sumDat, gr, xx)
		sumDat$cols <- rep(cols, each = length(unique(sumDat$xx)))
	
		# The pattern here took a long time to come up with!
		# The premise is that for a nlvls x ngr plot, you have to
		# connect only certain pairs of points, in particular
		# every nlvls_th point pair has to be skipped to
		# create a universal pattern

		nlvls <- length(unique(sumDat$xx))
		ngr <- length(unique(sumDat$gr))
				
		i1 <- 1:(nlvls*ngr)
		rem <- seq(nlvls, (nlvls*ngr), nlvls)
		i1 <- i1[-rem]

		# Troubleshooting commented out (but keep!)
		
#		print(sumDat)
#		cat("no. levels =", nlvls, "\n")
#		cat("no. groups =", ngr, "\n")
#		cat("Rows from sumDat are connected in vertical pairs as follows\n"s)
#		cat("first point = ", i1, "\n")
#		i2 <- i1 + 1
#		cat("second point = ", i2, "\n")

		for (i in i1) {
			if ((method == "sem") | (method == "sem95")) {
				lattice::panel.segments(x0 = sumDat[i,1], y0 = sumDat[i,3],
					x1 = sumDat[i+1,1], y1 = sumDat[i+1,3], col = sumDat[i, 5], ...)
				}
			if ((method == "mad") | (method == "iqr")) {
				lattice::panel.segments(x0 = sumDat[i,1], y0 = sumDat[i,4],
					x1 = sumDat[i+1,1], y1 = sumDat[i+1,4], col = sumDat[i, 5], ...)
				}
			}
		} # end of panel.connect

### Now prepare summary tables using tableGrob from gridExtra

	if (!is.null(table)) {
		TT <- gridExtra::ttheme_default(
			core = list(fg_params=list(cex = table[3])),
			colhead = list(fg_params=list(cex = table[3])),
			rowhead = list(fg_params=list(cex = table[3])))

		if (type == "connect") {
			if (!is.factor(data[,xx])) {
				msg <- paste(xx, "must be a factor for a table of counts")
				stop(msg)
				}
			counts <- plyr::count(data, vars = c(grn, xx))
			colnames(counts) <- c(grn, xx, "count")

			myt <- gridExtra::tableGrob(counts, rows = NULL, theme = TT)
			myt <- gtable::gtable_add_grob(myt, 
				grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)), 
				t = 2, b = nrow(myt), l = 1, r = ncol(myt))
			myt <- gtable::gtable_add_grob(myt, 
				grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)), 
				t = 1, l = 1, r = ncol(myt))				
			}

		if (type == "anova") {
			if (!is.factor(data[,xx])) {
				msg <- paste(xx, "must be a factor for anova")
				stop(msg)
				}
			f <- paste(".~.*", deparse(substitute(groups)))
			nf <- stats::update.formula(formula, f)
			mod <- stats::aov(formula = nf, data = data)
			
			myt <- prettyAOVtable(aov.object = mod, cex = table[3])
						}

		if (type == "fitLine") {
			lvls <- levels(data[,grn])
			nl <- length(lvls)
			m <- c()
			b <- c()
			r2 <- c()
			for (i in 1:nl) {
				wh <- grepl(lvls[i], data[,grn])
				dat <- data[wh,]
				mod <- stats::lm(dat[,yy] ~ dat[,xx])
				m[i] <- round(mod$coef[2], 2)
				b [i]<- round(mod$coef[1], 2)
				r2[i] <- round(stats::cor(dat[,xx], dat[,yy])^2, 4)		
				}
				
			mod.yy <- data.frame(line = lvls, m = m, b = b, r2 = r2)
				
			myt <- gridExtra::tableGrob(mod.yy, rows = NULL, theme = TT)
			myt <- gtable::gtable_add_grob(myt, 
				grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)), 
				t = 2, b = nrow(myt), l = 1, r = ncol(myt))				
			myt <- gtable::gtable_add_grob(myt, 
				grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)), 
				t = 1, l = 1, r = ncol(myt))				
			}
		} # End of table preparations

##### Now the high level plot calls

	ps <- theme

	p <- lattice::xyplot(stats::as.formula(args$formula),
  		data = eval(args$data), 
        groups = eval(args$groups), ...,
		axis = latticeExtra::axis.grid,
		par.settings = ps,
		panel = function(x, y, ...) {
			panel.summary(x, y, ...)
			if (freckles) lattice::panel.xyplot(x, y, jitter.x = TRUE, col = cols, ...)
			if ((type == "connect") | (type == "anova")) panel.connect(x, y, ...)
			if (type == "fitLine") lattice::panel.xyplot(x, y, type = "r", col = cols, ...)
			if (!is.null(table)) {
				if (type == "connect") {
					grid::upViewport(0)
					tv <- grid::viewport(x = table[1], y = table[2])
					grid::pushViewport(tv)
					grid::grid.draw(myt)
					}
				if (type == "anova") {
					grid::upViewport(0)
					tv <- grid::viewport(x = table[1], y = table[2])
					grid::pushViewport(tv)
					grid::grid.draw(myt)
					}
				if (type == "fitLine") {
					grid::upViewport(0)
					tv <- grid::viewport(x = table[1], y = table[2])
					grid::pushViewport(tv)
					grid::grid.draw(myt)
					}
				} # end of table options

			if (!is.null(legend)) {
				TTT <- gridExtra::ttheme_minimal(core = list(fg_params = list(cex = legend[3], col = cols)))

				legtxt <- data.frame(lvs = levels(data[,grn]))
				myleg <- gridExtra::tableGrob(legtxt,
					cols = NULL, rows = NULL, theme = TTT)
				myleg <- gtable::gtable_add_grob(myleg, 
					grobs = grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2)), 
					t = 1, b = nrow(myleg), l = 1, r = ncol(myleg))				
				grid::upViewport(0)
				lv <- grid::viewport(x = legend[1], y = legend[2])
				grid::pushViewport(lv)
				grid::grid.draw(myleg)

				} # end of legend processing
			} # end of panel function
			) # end of xyplot
	
	invisible(p)
	}

