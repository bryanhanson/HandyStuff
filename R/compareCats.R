#' Compare Data by Category
#' 
#' This function allows one to generate a nice plot comparing a response from
#' samples falling into one or two categories with corresponding levels.
#' \code{lattice} is used to create plots, with options to include the original
#' data and to use various measures of central tendency and spread.  A count of
#' the number of samples in each level is included.  Gives a visual impression
#' of the data to go along with hypothesis tests.
#' 
#' The supplied data frame is stripped down to just the response and factor
#' columns, then \code{NAs} are removed; the counts reflect this.
#' 
#' @param formula A formula with both LHS and RHS.  The formula should comply
#' with the usual \code{lattice} conventions, for instance \code{y ~ factor1 |
#' factor2}.
#'
#' @param data A data frame containing the response and categorical/factor
#' data.  A maximum two categories with any number of levels can be plotted at
#' one time.
#'
#' @param cols A vector of colors to be used for factor 1; must be as along as
#' the levels in factor 1.
#'
#' @param freckles Logical; if \code{TRUE}, the actual data points are plotted
#' as small points, hence the name, jittered slightly in the horizontal
#' direction only.
#'
#' @param method One of \code{c("sem", "sem95", "iqr", "mad", "box",
#' "points")}.  Various methods for computing measures of spread and central
#' tendency.  See the documentation for \code{ChemoSpec::.seXy}.
#'
#' @param theme Character; A suitable \code{lattice} theme.  
#' There are two built-in themes which you can use "as is" or modify to your heart's
#' content.  If  none is given, \code{\link{screenTheme}} will be used.  The other option
#' provided is \code{\link{posterTheme}}.
#'
#' @param \dots Other parameters to be passed downstream.
#' @return A \code{lattice} object.  These can be modified by the usual methods
#' associated with these packages.
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#'
#' @references
#'
#' \url{https://github.com/bryanhanson/HandyStuff}
#' @keywords plot univariate
#' @examples
#' #
#' ### Set up test data
#' #
#' require("ChemoSpec")
#' require("lattice")
#' require("latticeExtra")
#' require("plyr")
#'
#' mydf <- data.frame(
#' 	resp = rnorm(40),
#' 	cat1 = sample(LETTERS[1:3], 40, replace = TRUE),
#' 	cat2 = sample(letters[1:2], 40, replace = TRUE),
#'  stringsAsFactors = TRUE)
#' #
#' ### One factor:
#' #
#' p <- compareCats(formula = resp~cat1, data = mydf,
#' 	method = "sem", freckles = TRUE, poster = FALSE, 
#' 	cols = c("red", "orange", "blue"))
#' print(p)
#' #
#' ### Two factors:
#' #	
#' p <- compareCats(formula = resp~cat1 | cat2, data = mydf,
#' 	method = "sem", freckles = TRUE,
#' 	cols = c("red", "orange", "blue"))
#' print(p)
#' #
#' ### Interchange the roles of the factors
#' #
#' p <- compareCats(formula = resp~cat2 | cat1, data = mydf,
#' 	method = "sem", freckles = TRUE,
#' 	cols = c("red", "blue"))
#' print(p)
#' 
#' 
#' @export
#'
compareCats <-
function(formula = NULL, data = NULL,
	cols = NULL, freckles = FALSE,
	method = c("sem", "sem95", "iqr", "mad", "box", "points"),
	theme = screenTheme(), ...)
	{

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

	# Function to compare categorical data by
	# plotting means and se's or something similar
	# Bryan Hanson, DePauw Univ, Jan 2010
	
	# This is the lattice version
	# Many features of the plot are designed to loosely emulate ggplot2

	# Check and process the formula
	
	if (is.null(formula)) stop("Formula not given")
	if (!plyr::is.formula(formula)) stop("Invalid formula specification")

	TwoFac <- FALSE
	res <- as.character(formula[[2]])
	if (length(formula[[3]]) == 1) {
		fac1 <- as.character(formula[[3]])
		}
	if (length(formula[[3]]) == 3) {
		fac2 <- as.character(formula[[3]][3])
		fac1 <- as.character(formula[[3]][2])
		TwoFac <- TRUE
		}

	args <- as.list(match.call(expand.dots = FALSE)[-1]) # used a bit later
		
	# Reduce the df and clean it of NAs
	# otherwise there is trouble with the counts
	
	if (TwoFac) keep <- c(res, fac1, fac2)
	if (!TwoFac) keep <- c(res, fac1)
	data <- data[, keep]
	data <- stats::na.omit(data)

	# Misc checks
	
	if (!is.factor(data[,fac1])) stop(paste(fac1, "was not of type factor"))
	if (TwoFac) if (!is.factor(data[,fac2])) stop(paste(fac2, "was not of type factor"))

	if ((freckles) & (method == "points")) {
		freckles <- FALSE
		message("Points & freckles don't make sense together, only plotting points")
		}

	if ((freckles) & (method == "box")) {
		freckles <- FALSE
		message("Box plot & freckles don't look good together, only plotting boxplot")
		}
		
	# Need to calc ylim to accommodate counts at bottom of plot
	yl <- c(min(data[,res]) - 0.15*diff(range(data[,res])), max(data[,res])*1.05)

	# Check the color scheme; set up if not given

	nc <- length(levels(data[,fac1]))
	
	if (is.null(cols)) {
		cols <- RColorBrewer::brewer.pal(nc, "Set1")
		if (nc == 2) cols <- cols[1:2]
		}
	if (!is.null(cols)) {
		if (nc != length(cols)) {
			stop(paste("You gave", length(cols), "colors, but you need", nc, "colors"))
			}
		}

	# Assign a color to each data point / also use as grouping variable

	ptcols <- rep(NA_character_, nrow(data))
	for (i in 1:nlevels(data[,fac1])) {
		tmp <- which(data[,fac1] == levels(data[,fac1])[i])
		ptcols[tmp] <- cols[i]
		}
	data$grps <- factor(ptcols, levels = cols)
		
	ps <- theme
			
########
		
	if (!TwoFac) { # Handle everything unique to one factor situation here

		# This panel sets up the count labels
		
		panel.counts <- function(x, y, ...) {
			
			facCounts <- plyr::count(data, vars = fac1)
			facCounts$label <- paste("n = ", facCounts$freq , sep = "")
			facCounts$y <- min(data[,res]) - 0.1*diff(range(data[,res]))
			facCounts <- facCounts[,-2]
			names(facCounts) <- c("x", "lab", "y")
			lattice::panel.text(x = facCounts[,1], y = facCounts[,3], labels = facCounts[,2], ...)
					
			} # end of panel.counts
		
		# This panel will compute & plot summary statistics
		
		panel.summary <- function(x, y, ...) {
			
			meany <- stats::aggregate(data[,res] ~ data[,fac1], data, FUN = mean)
			medy <- stats::aggregate(data[,res] ~ data[,fac1], data, FUN = stats::median)
			sexy <- stats::aggregate(data[,res] ~ data[,fac1], data, FUN = ChemoSpec::.seXy)
			sexy95 <- stats::aggregate(data[,res] ~ data[,fac1], data, FUN = ChemoSpec::.seXy95)
			sexymad <- stats::aggregate(data[,res] ~ data[,fac1], data, FUN = ChemoSpec::.seXyMad)
			sexyiqr <- stats::aggregate(data[,res] ~ data[,fac1], data, FUN = ChemoSpec::.seXyIqr)
			sumDat <- cbind(meany, medy[,2], sexy[[2]][,c(2,3)], sexy95[[2]][,c(2,3)],
				sexymad[[2]][,c(2,3)], sexyiqr[[2]][,c(2,3)])
			names(sumDat) <- c("factor1", "mean", "median",
				"semL", "semU", "sem95L", "sem95U", "madL", "madU",
				"iqrL", "iqrU")			

			# Plot the mean or median
			
			if ((method == "sem") | (method == "sem95")) {
				lattice::panel.points(x = sumDat[,1], y = sumDat[,2], pch = 20, ...)
				}
				
			if ((method == "mad") | (method == "iqr")) {
				lattice::panel.points(x = sumDat[,1], y = sumDat[,3], pch = 20, ...)
				}

			# Plot the 'whiskers'

			if (method == "sem") {
				lattice::panel.segments(x0 = sumDat[,1], y0 = sumDat[,4],
				x1 = sumDat[,1], y1 = sumDat[,5], lwd = 3, ...)		
				}

			if (method == "sem95") {
				lattice::panel.segments(x0 = sumDat[,1], y0 = sumDat[,6],
				x1 = sumDat[,1], y1 = sumDat[,7], lwd = 3, ...)		
				}

			if (method == "mad") {
				lattice::panel.segments(x0 = sumDat[,1], y0 = sumDat[,8],
				x1 = sumDat[,1], y1 = sumDat[,9], lwd = 3, ...)		
				}

			if (method == "iqr") {
				lattice::panel.segments(x0 = sumDat[,1], y0 = sumDat[,10],
				x1 = sumDat[,1], y1 = sumDat[,11], lwd = 3, ...)		
				}
	
			} # end of panel.summary
							
		} # End of !TwoFac
		
##########
		
	if (TwoFac) { # Handle everything unique to two factors here

		# This panel sets up the count labels
		
		panel.counts <- function(x, y, ...) {
			
			facCounts <- plyr::count(data, vars = c(fac1, fac2))
			facCounts$label <- paste("n = ", facCounts$freq , sep = "")
			facCounts$y <- min(data[,res]) - 0.1*diff(range(data[,res]))
			facCounts <- facCounts[,-3]
			names(facCounts) <- c("f1", "f2", "lab", "y")
			f1 <- f2 <- NULL # Needed to fake out check mechanism on next step
			facCounts <- plyr::arrange(facCounts, f2, f1)
			
			# Helper function to select the needed rows of facCounts

			lf1 <- length(levels(facCounts$f1)) # no. of draws w/i packet
			lf2 <- length(levels(facCounts$f2)) # no. of packets to be drawn
			
			xx <- function(pn) {
				nr <- nrow(facCounts) # this will not be elegant
				st <- 1 + (lattice::packet.number()-1)*lf1
				end <- st + lf1 - 1
				myx <- 1:nr
				myx <- myx[st:end]
				myx	
				}
			
			for (i in seq_along(lf2)) {
					xxx <- xx(lattice::packet.number())
					lattice::panel.text(x = 1:lf1, y = facCounts[xxx, 4],
						labels = facCounts[xxx, 3], ...)			
				}
			} # end of panel.counts
			
				
		panel.summary <- function(x, y, ...) {
			
			mean <- stats::aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = mean)
			med <- stats::aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = stats::median)
			sexy <- stats::aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = ChemoSpec::.seXy)
			sexy95 <- stats::aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = ChemoSpec::.seXy95)
			sexymad <- stats::aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = ChemoSpec::.seXyMad)
			sexyiqr <- stats::aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = ChemoSpec::.seXyIqr)
			sumDat <- cbind(mean, med[,3], sexy[[3]][,c(2,3)], sexy95[[3]][,c(2,3)],
				sexymad[[3]][,c(2,3)], sexyiqr[[3]][,c(2,3)])
	
			names(sumDat) <- c("factor1", "factor2", "mean", "median",
				"semL", "semU", "sem95L", "sem95U", "madL", "madU",
				"iqrL", "iqrU")

			# Helper function to select the needed rows of sumDat

			lf1 <- length(levels(sumDat$factor1)) # no. of draws w/i packet
			lf2 <- length(levels(sumDat$factor2)) # no. of packets to be drawn
			
			xx <- function(pn) {
				nr <- nrow(sumDat)
				st <- 1 + (lattice::packet.number()-1)*lf1
				end <- st + lf1 - 1
				myx <- 1:nr
				myx <- myx[st:end]
				myx	
				}
			
			# Plot the mean or median
			
			if ((method == "sem") | (method == "sem95")) {
				for (i in seq_along(lf2)) {
						xxx <- xx(lattice::packet.number())
						lattice::panel.points(x = 1:lf1, y = sumDat[xxx, 3], pch = 20, ...)
					}
				}
				
			if ((method == "mad") | (method == "iqr")) {
				for (i in seq_along(lf2)) {
						xxx <- xx(lattice::packet.number())
						lattice::panel.points(x = 1:lf1, y = sumDat[xxx, 4], pch = 20, ...)
					}
				}

			# Plot the 'whiskers'

			if (method == "sem") {
				for (i in seq_along(lf2)) {
						xxx <- xx(lattice::packet.number())
						lattice::panel.segments(x0 = sumDat[xxx,1], y0 = sumDat[xxx,5],
						x1 = sumDat[xxx,1], y1 = sumDat[xxx,6], lwd = 3, ...)		
					}
				}

			if (method == "sem95") {
				for (i in seq_along(lf2)) {
						xxx <- xx(lattice::packet.number())
						lattice::panel.segments(x0 = sumDat[xxx,1], y0 = sumDat[xxx,7],
						x1 = sumDat[xxx,1], y1 = sumDat[xxx,8], lwd = 3, ...)		
					}
				}

			if (method == "mad") {
				for (i in seq_along(lf2)) {
						xxx <- xx(lattice::packet.number())
						lattice::panel.segments(x0 = sumDat[xxx,1], y0 = sumDat[xxx,9],
						x1 = sumDat[xxx,1], y1 = sumDat[xxx,10], lwd = 3, ...)		
					}
				}

			if (method == "iqr") {
				for (i in seq_along(lf2)) {
						xxx <- xx(lattice::packet.number())
						lattice::panel.segments(x0 = sumDat[xxx,1], y0 = sumDat[xxx,11],
						x1 = sumDat[xxx,1], y1 = sumDat[xxx,12], lwd = 3, ...)		
					}
				}
	
			} # end of panel.summary
		
		} # End of TwoFac

##### Now the high level plot calls (common to all options)
	
	if (!method == "box") {
		p <- lattice::xyplot(
			stats::as.formula(args$formula),
  			data = eval(args$data),
  			ylim = yl,
			scales = list(alternating = FALSE),
			between = list(x = 0.25, y = 0.25),
			axis = latticeExtra::axis.grid,
			groups = data$grps,
			par.settings = ps,
			...,
			# This key works but seems unnecessary, as the plot is self-documenting
			# auto.key = list(text = levels(fac2), space = "right",
				# points = FALSE, title = fac2, cex.title = 1.0),
			panel = function(x, y, ...) {
				
				if (method == "points") lattice::panel.xyplot(x, y, col = cols, ...)
				if (freckles) lattice::panel.xyplot(x, y, jitter.x = TRUE, col = cols, ...)
					
				panel.summary(x, y, col = cols, ...)
				panel.counts(x, y, ...)
				}
				)
		}

	if (method == "box") { # This doesn't play well with the above, so do separately
		p <- lattice::bwplot(formula, data, ylim = yl, ...,
			scales = list(alternating = FALSE),
			between = list(x = 0.25, y = 0.25),
			axis = latticeExtra::axis.grid,
			par.settings = ps,
			# This key works but seems unnecessary, as the plot is self-documenting
			# auto.key = list(text = levels(fac2), space = "right",
				# points = FALSE, title = fac2, cex.title = 1.0),
			panel = function(x, y, ...) {
				lattice::trellis.par.set(box.umbrella = list(col = cols)) # can't change any other way
				lattice::panel.bwplot(x, y, box.width = 0.1, fill = cols,...)
				panel.counts(x, y, ...)
				}
				)
		}

		return(p) 
		
 	}

