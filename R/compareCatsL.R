compareCatsL <-
function(formula = NULL, data = NULL,
	cols = NULL, freckles = FALSE,
	method = c("sem", "iqr", "mad", "box", "points", "sem95"),
	xlab = NULL, ylab = NULL, title = NULL, ...)
	{

	# Function to compare categorical data by
	# plotting means and se's or something similar
	# Bryan Hanson, DePauw Univ, Jan 2010
	
	# This is the lattice version

	# Check and process the formula
	
	if (is.null(formula)) stop("Formula not given")
	if (!is.formula(formula)) stop("Invalid formula specification")

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
		
	# Reduce the df and clean it of NAs
	# otherwise there is trouble with the counts
	
	if (TwoFac) keep <- c(res, fac1, fac2)
	if (!TwoFac) keep <- c(res, fac1)
	data <- data[, keep]
	data <- na.omit(data)

	if (!is.factor(data[,fac1])) stop("factor1 was not actually of type factor")
	if (TwoFac) if (!is.factor(data[,fac2])) stop("factor2 was not actually of type factor")

	if ((freckles) & (method == "points")) {
		freckles <- FALSE
		message("Points & freckles don't make sense together, only plotting points")
		}
		
	# Need to calc ylim to accommodate counts at bottom of plot
	yl <- c(min(data$res) - 0.15*diff(range(data$res)), max(data$res)*1.05)

######## Some items useable by both one and two facs follow
	
	# This panel simply plots the data points
	
	panel.justpoints <- function(x, y, ...) {
		panel.xyplot(x, y, ...)
		}
		
########
		
	if (!TwoFac) { # Handle everything unique to one factor situation here

		# This panel sets up the count labels
		
		panel.counts <- function(x, y, ...) {
			
			facCounts <- count(data, vars = fac1)
			facCounts$label <- paste("n = ", facCounts$freq , sep = "")
			facCounts$y <- min(data$res) - 0.1*diff(range(data$res))
			facCounts <- facCounts[,-2]
			names(facCounts) <- c("x", "lab", "y")
			panel.text(x = facCounts[,1], y = facCounts[,3], labels = facCounts[,2], ...)
					
			} # end of panel.counts
		
		# This panel will compute & plot summary statistics
		
		panel.summary <- function(x, y, ...) {
			
			meany <- aggregate(data[,res] ~ data[,fac1], data, FUN = mean)
			medy <- aggregate(data[,res] ~ data[,fac1], data, FUN = median)
			sexy <- aggregate(data[,res] ~ data[,fac1], data, FUN = seXy)
			sexy95 <- aggregate(data[,res] ~ data[,fac1], data, FUN = seXy95)
			sexymad <- aggregate(data[,res] ~ data[,fac1], data, FUN = seXyMad)
			sexyiqr <- aggregate(data[,res] ~ data[,fac1], data, FUN = seXyIqr)
			sumDat <- cbind(meany, medy[,2], sexy[[2]][,c(2,3)], sexy95[[2]][,c(2,3)],
				sexymad[[2]][,c(2,3)], sexyiqr[[2]][,c(2,3)])
			names(sumDat) <- c("factor1", "mean", "median",
				"semL", "semU", "sem95L", "sem95U", "madL", "madU",
				"iqrL", "iqrU")			

			# Plot the mean or median
			
			if ((method == "sem") | (method == "sem95")) {
				panel.points(x = sumDat[,1], y = sumDat[,2], pch = 19, ...)
				}
				
			if ((method == "mad") | (method == "iqr")) {
				panel.points(x = sumDat[,1], y = sumDat[,3], pch = 19, ...)
				}

			# Plot the 'whiskers'

			if (method == "sem") {
				panel.segments(x0 = sumDat[,1], y0 = sumDat[,4],
				x1 = sumDat[,1], y1 = sumDat[,5], lwd = 3, ...)		
				}

			if (method == "sem95") {
				panel.segments(x0 = sumDat[,1], y0 = sumDat[,6],
				x1 = sumDat[,1], y1 = sumDat[,7], lwd = 3, ...)		
				}

			if (method == "mad") {
				panel.segments(x0 = sumDat[,1], y0 = sumDat[,8],
				x1 = sumDat[,1], y1 = sumDat[,9], lwd = 3, ...)		
				}

			if (method == "iqr") {
				panel.segments(x0 = sumDat[,1], y0 = sumDat[,10],
				x1 = sumDat[,1], y1 = sumDat[,11], lwd = 3, ...)		
				}
	
			} # end of panel.summary
		
		
		# # Now the high level plot calls
		
		# if (method == "box") {
			# p <- bwplot(formula, data, ylim = yl, ...,
				# panel = function(x, y, ...) {
					# panel.bwplot(x, y, box.width = 0.1, ...)
					# panel.counts(x, y, ...)
					# }
					# )
			# return(p)
			# }

		# if (method == "points") {
			# p <- xyplot(formula, data, ylim = yl, ...,
				# panel = function(x, y, ...) {
					# panel.justpoints(x, y, pch = 20, ...)
					# panel.counts(x, y, ...)
					# }
					# )
			# return(p) # nothing further will be drawn in this case
			# }

		# if (freckles) {
			# p <- stripplot(formula, data, ylim = yl, ..., 
				# panel = function(x, y, ...) {
					# panel.counts(x, y, ...)
					# panel.summary(x, y, col = cols, ...)
					# panel.stripplot(x, y, jitter.data = TRUE, pch = 20, col = "black", ...)
					# }
					# )
			# }

		# if (!freckles) {
			# p <- stripplot(formula, data, ylim = yl, ...,
				# panel = function(x, y, ...) {
					# panel.counts(x, y, ...)
					# panel.summary(x, y, col = cols, ...)
					# }
					# )
			# }
					
		} # End of !TwoFac
		
##########
		
	if (TwoFac) { # Handle everything unique to two factors here

		# This panel sets up the count labels
		
		panel.counts <- function(x, y, ...) {
			
			facCounts <- count(data, vars = c(fac1, fac2))
			facCounts$label <- paste("n = ", facCounts$freq , sep = "")
			facCounts$y <- min(data$res) - 0.1*diff(range(data$res))
			facCounts <- facCounts[,-3]
			names(facCounts) <- c("f1", "f2", "lab", "y")
			facCounts <- arrange(facCounts, f2, f1)
			
			# Helper function to select the needed rows of facCounts

			lf1 <- length(levels(facCounts$f1)) # no. of draws w/i packet
			lf2 <- length(levels(facCounts$f2)) # no. of packets to be drawn
			
			xx <- function(pn) {
				nr <- nrow(facCounts) # this will not be elegant
				st <- 1 + (packet.number()-1)*lf1
				end <- st + lf1 - 1
				myx <- 1:nr
				myx <- myx[st:end]
				myx	
				}
			
			for (i in seq_along(lf2)) {
					xxx <- xx(packet.number())
					panel.text(x = 1:lf1, y = facCounts[xxx, 4],
						labels = facCounts[xxx, 3], ...)			
				}
			} # end of panel.counts
			
				
		panel.summary <- function(x, y, ...) {
			
			mean <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = mean)
			med <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = median)
			sexy <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = seXy)
			sexy95 <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = seXy95)
			sexymad <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = seXyMad)
			sexyiqr <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = seXyIqr)
			sumDat <- cbind(mean, med[,3], sexy[[3]][,c(2,3)], sexy95[[3]][,c(2,3)],
				sexymad[[3]][,c(2,3)], sexyiqr[[3]][,c(2,3)])
#			sumDat <- arrange(sumDat, fac2, fac1)
	
			names(sumDat) <- c("factor1", "factor2", "mean", "median",
				"semL", "semU", "sem95L", "sem95U", "madL", "madU",
				"iqrL", "iqrU")

			# Helper function to select the needed rows of sumDat

			lf1 <- length(levels(sumDat$factor1)) # no. of draws w/i packet
			lf2 <- length(levels(sumDat$factor2)) # no. of packets to be drawn
			
			xx <- function(pn) {
				nr <- nrow(sumDat)
				st <- 1 + (packet.number()-1)*lf1
				end <- st + lf1 - 1
				myx <- 1:nr
				myx <- myx[st:end]
				myx	
				}
			
			# Plot the mean or median
			
			if ((method == "sem") | (method == "sem95")) {
				for (i in seq_along(lf2)) {
						xxx <- xx(packet.number())
						panel.points(x = 1:lf1, y = sumDat[xxx, 3], pch = 19, ...)
					}
				}
				
			if ((method == "mad") | (method == "iqr")) {
				for (i in seq_along(lf2)) {
						xxx <- xx(packet.number())
						panel.points(x = 1:lf1, y = sumDat[xxx, 4], pch = 19, ...)
					}
				}

			# Plot the 'whiskers'

			if (method == "sem") {
				for (i in seq_along(lf2)) {
						xxx <- xx(packet.number())
						panel.segments(x0 = sumDat[xxx,1], y0 = sumDat[xxx,5],
						x1 = sumDat[xxx,1], y1 = sumDat[xxx,6], lwd = 3, ...)		
					}
				}

			if (method == "sem95") {
				for (i in seq_along(lf2)) {
						xxx <- xx(packet.number())
						panel.segments(x0 = sumDat[xxx,1], y0 = sumDat[xxx,7],
						x1 = sumDat[xxx,1], y1 = sumDat[xxx,8], lwd = 3, ...)		
					}
				}

			if (method == "mad") {
				for (i in seq_along(lf2)) {
						xxx <- xx(packet.number())
						panel.segments(x0 = sumDat[xxx,1], y0 = sumDat[xxx,9],
						x1 = sumDat[xxx,1], y1 = sumDat[xxx,10], lwd = 3, ...)		
					}
				}

			if (method == "iqr") {
				for (i in seq_along(lf2)) {
						xxx <- xx(packet.number())
						panel.segments(x0 = sumDat[xxx,1], y0 = sumDat[xxx,11],
						x1 = sumDat[xxx,1], y1 = sumDat[xxx,12], lwd = 3, ...)		
					}
				}
	
			} # end of panel.summary
		
		
		# # Now the high level plot calls
		
		# if (method == "box") {
			# p <- bwplot(formula, data, ...)
			# }

		# if (method == "points") {
			# p <- xyplot(formula, data,
				# panel = panel.justpoints, col = "black", ...)
				# }

		# if (freckles) {
			# p <- stripplot(formula, data, ylim = yl, ..., 
				# panel = function(x, y, subscripts, ...) {
					# panel.counts(x, y, subscripts, ...)
					# panel.summary(x, y, col = cols, ...)
					# panel.stripplot(x, y, jitter.data = TRUE, pch = 20, col = "black", ...)
					# }
					# )
			# }

		# if (!freckles) {
			# p <- stripplot(formula, data, ylim = yl, ..., 
				# panel = function(x, y, ...) {
					# panel.counts(x, y, subscripts, ...)
					# panel.summary(x, y, col = cols, ...)
					# }
					# )
			# }
		
		} # End of TwoFac

##### Now the high level plot calls (common to all options)
		
		if (method == "box") {
			p <- bwplot(formula, data, ylim = yl, ...,
				panel = function(x, y, ...) {
					panel.bwplot(x, y, box.width = 0.1, ...)
					panel.counts(x, y, ...)
					}
					)
			return(p)
			}

		if (method == "points") {
			p <- xyplot(formula, data, ylim = yl, ...,
				panel = function(x, y, ...) {
					panel.justpoints(x, y, pch = 20, ...)
					panel.counts(x, y, ...)
					}
					)
			return(p) # nothing further will be drawn in this case
			}

		if (freckles) {
			p <- stripplot(formula, data, ylim = yl, ..., 
				panel = function(x, y, ...) {
					panel.counts(x, y, ...)
					panel.summary(x, y, col = cols, ...)
					panel.stripplot(x, y, jitter.data = TRUE, pch = 20, col = "black", ...)
					}
					)
			}

		if (!freckles) {
			p <- stripplot(formula, data, ylim = yl, ...,
				panel = function(x, y, ...) {
					panel.counts(x, y, ...)
					panel.summary(x, y, col = cols, ...)
					}
					)
			}
	
	p
 	}

