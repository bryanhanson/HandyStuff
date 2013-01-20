compareCats <-
function(formula = NULL, data = NULL,
	cols = NULL, freckles = FALSE,
	method = c("sem", "iqr", "mad", "box", "points", "sem95"),
	xlab = NULL, ylab = NULL, title = NULL, ...)
	{

# Function to compare categorical data by
# plotting means and se's or something similar
# Bryan Hanson, DePauw Univ, Jan 2010

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
				
	# Initialize + add the points, jittered or not
	# This is aesthetic #1
	
	p <- ggplot()

	if (!freckles) {
		if (TwoFac) {
			p <- p + geom_point(data = data, 
			aes_string(x = fac1, y = res, color = fac2, group = fac2))
			# p <- ggplot(data = data, geom = "point",
			# aes_string(x = fac1, y = res, color = fac2, group = fac2))
			}
		if (!TwoFac) {
			p <- p + geom_point(data = data, 
			aes_string(x = fac1, y = res, color = fac1))
			}
		}
		
	if (freckles) {
		jit <- position_jitter(width = 0.05, height = 0.0)
		if (TwoFac) {
			p <- p + geom_jitter(data = data,
			aes_string(x = fac1, y = res, color = fac2, group = fac2),
			position = jit, size = 1.0)
			}
		if (!TwoFac) {
			p <- p + geom_jitter(data = data,
			aes_string(x = fac1, y = res, color = fac1),
			position = jit, size = 1.0)
			}
		}	

	# Boxplot does its own summary
	
	if (TwoFac) {
		if (method == "box") { # Does not work correctly for TwoFac
			p <- p + geom_boxplot(data = data, width = 0.2,
			aes_string(x = fac1, y = res, color = fac2, group = fac2))
			}
		}

	if (!TwoFac) {
		if (method == "box") {
			p <- p + geom_boxplot(data = data, width = 0.2,
			aes_string(x = fac1, y = res, color = fac1))
			}
		}

	# All plotting below here uses this summary data
	# Prepare the summary data (required for all other options)
	# Due to a bug in ggplot2_0.9.3, we must calc some quantities
	# and put them in a separate data frame for a new aesthetic
	# Slightly wasteful to compute all when you may not use all, but...

	if (TwoFac) {
		meany <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = mean)
		medy <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = median)
		sexy <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = seXy)
		sexy95 <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = seXy95)
		sexymad <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = seXyMad)
		sexyiqr <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = seXyIqr)
		sumDat <- cbind(meany, medy[,3], sexy[[3]][,c(2,3)], sexy95[[3]][,c(2,3)],
			sexymad[[3]][,c(2,3)], sexyiqr[[3]][,c(2,3)])
		names(sumDat) <- c("factor1", "factor2", "mean", "median",
			"semL", "semU", "sem95L", "sem95U", "madL", "madU",
			"iqrL", "iqrU")
#		print(sumDat)

		# Now add summary layers as requested
		# These are aesthetic #2, using a new data set
	
		# sumDat2 <- melt(sumDat, id.vars = c("factor1", "factor2"))
		# print(sumDat2)
		
		if (method == "sem") {
			sumDat2 <- sumDat[,c(1,2,3,5,6)]
			sumDat2 <- melt(sumDat2, id.vars = c("factor1", "factor2"), measure.vars = c("mean", "semL", "semU"))
			sumDat2 <- dcast(sumDat2, factor1 + factor2 ~ variable)
			print(sumDat2)
			p <- p + geom_pointrange(data = sumDat2,
				aes(x = factor1, y = mean, ymin = semL, ymax = semU,
				color = factor2, group = factor2))
			}
	
		if (method == "sem95") {
			p <- p + geom_pointrange(data = sumDat,
				aes(x = factor1, y = mean, ymin = sem95L, ymax = sem95U,
				color = factor2, group = factor2))
			}
	
		if (method == "mad") {
			p <- p + geom_pointrange(data = sumDat,
				aes(x = factor1, y = median, ymin = madL, ymax = madU,
				color = factor2, group = factor2))
			}
	
		if (method == "iqr") {
			p <- p + geom_pointrange(data = sumDat,
				aes(x = factor1, y = median, ymin = iqrL, ymax = iqrU,
				color = factor2, group = factor2))
			}
	} # end of TwoFac = TRUE

	if (!TwoFac) { # These all work
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
	
		# Now add summary layers as requested
		# These are aesthetic #2, using a new data set
	
		if (method == "sem") {
			p <- p + geom_pointrange(data = sumDat,
				aes(x = factor1, y = mean, ymin = semL, ymax = semU,
				color = factor1))
			}
	
		if (method == "sem95") {
			p <- p + geom_pointrange(data = sumDat,
				aes(x = factor1, y = mean, ymin = sem95L, ymax = sem95U,
				color = factor1))
			}
	
		if (method == "mad") {
			p <- p + geom_pointrange(data = sumDat,
				aes(x = factor1, y = median, ymin = madL, ymax = madU,
				color = factor1))
			}
	
		if (method == "iqr") {
			p <- p + geom_pointrange(data = sumDat,
				aes(x = factor1, y = median, ymin = iqrL, ymax = iqrU,
				color = factor1))
			}
	} # End of TwoFac = FALSE
	
	# Set up the color scheme & legend
	
	if (TwoFac) if (is.null(cols)) cols <- brewer.pal(length(levels(data[,fac2])), "Set1")
	if (!TwoFac) if (is.null(cols)) cols <- brewer.pal(length(levels(data[,fac1])), "Set1")
	
	p <- p  + scale_colour_manual(name = "", values = cols) +
		theme(axis.text.x = element_text(colour = "black"),
		axis.text.y = element_text(colour = "black"),
		axis.ticks = element_blank())		
		
	# Now add labels and fix limits (modified from qplot)
	
    if (!is.null(title)) p <- p + labs(title = title)
    if (!is.null(xlab)) p <- p + labs(xlab = xlab)
    if (!is.null(ylab)) p <- p + labs(ylab = ylab)
#    if (exists("xlim")) p <- p + xlim(xlim)
#    if (exists("ylim")) p <- p + ylim(ylim)

	if (TwoFac) facCounts <- count(data, vars = c(fac2, fac1))
	if (!TwoFac) facCounts <- count(data, vars = fac1)
	
	# facCounts$label <- paste("n = ", facCounts$freq , sep = "")
	# facCounts$x <- seq(1, length(levels(data[,fac1])), by = 1) # probably not right
	# facCounts$y <- min(data$res) - 0.1*diff(range(data$res))

	# p <- p + geom_text(aes(x, y, label = label), 
		# color = "black", size = 4.0, data = facCounts)

	invisible(p)
	}

