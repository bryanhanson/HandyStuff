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

	if ((freckles) & (method == "points")) message("You probably shouldn't use both points and freckles")

	########
		
	if (!TwoFac) { # Handle everything for one fac here
		
		p <- ggplot(aes_string(x = fac1, y = res, color = fac1), data = data)

		if (freckles) {
			jit <- position_jitter(width = 0.05, height = 0.0)
				p <- p + geom_jitter(color = "black",
				position = jit, size = 1.0)
			}	
		
		if (method == "box") {
			p <- p + geom_boxplot(width = 0.2)
			}

		if (method == "points") {
			p <- p + geom_point()
			}

		# Now add summary layers as requested
		# These are aesthetic #2, using a new data set

	# All plotting below here uses this summary data
	# Prepare the summary data (required for all other options)
	# Due to a bug in ggplot2_0.9.3, we must calc some quantities
	# and put them in a separate data frame for a new aesthetic
	# Slightly wasteful to compute all when you may not use all, but...

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

		# Aesthetic #3
	
		facCounts <- count(data, vars = fac1)
		facCounts$label <- paste("n = ", facCounts$freq , sep = "")
		facCounts$y <- min(data$res) - 0.1*diff(range(data$res))
		facCounts <- facCounts[,-2]
		names(facCounts) <- c("f1", "label", "y")
		p <- p + geom_text(aes(x = f1, y = y, label = label),
			color = "black", size = 4.0, data = facCounts)		

		} # End of !TwoFac
		
	##########
		
	if (TwoFac) { # Handle everything for two fac here
		
		p <- ggplot(aes_string(x = fac1, y = res, color = fac1), data = data) +
			facet_grid(paste(".~", fac2))

		if (freckles) {
			jit <- position_jitter(width = 0.05, height = 0.0)
			p <- p + geom_jitter(position = jit, size = 1.0, color = "black")
			}
		
		if (method == "box") {
			p <- p + geom_boxplot(width = 0.2)
			}

		if (method == "points") {
			p <- p + geom_point()
			}

	# All plotting below here uses this summary data
	# Prepare the summary data (required for all other options)
	# Due to a bug in ggplot2_0.9.3, we must calc some quantities
	# and put them in a separate data frame for a new aesthetic
	# Slightly wasteful to compute all when you may not use all, but...

		mean <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = mean)
		med <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = median)
		sexy <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = seXy)
		sexy95 <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = seXy95)
		sexymad <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = seXyMad)
		sexyiqr <- aggregate(data[,res] ~ data[,fac1]*data[, fac2], data, FUN = seXyIqr)
		sumDat <- cbind(mean, med[,3], sexy[[3]][,c(2,3)], sexy95[[3]][,c(2,3)],
			sexymad[[3]][,c(2,3)], sexyiqr[[3]][,c(2,3)])

		print(names(sumDat))

		names(sumDat) <- c("factor1", "factor2", "mean", "median",
			"semL", "semU", "sem95L", "sem95U", "madL", "madU",
			"iqrL", "iqrU")

		# Now add summary layers as requested
		# These are aesthetic #2, using a new data set
		
		if (method == "sem") {
			p <- p + geom_point(aes(x = factor1, y = mean, color = factor1),
				data = sumDat)
			# p <- p + geom_linerange(data = sumDat,
				# aes(x = fac2, ymin = semL, ymax = semU,
				# color = fac2))
			}
	
		if (method == "sem95") {
			p <- p + geom_pointrange(data = sumDat,
				aes(x = factor1, y = mean, ymin = sem95L, ymax = sem95U,
				color = factor1)) +
				facet_grid(".~factor2")
			}
	
		if (method == "mad") {
			p <- p + geom_pointrange(data = sumDat,
				aes(x = factor1, y = median, ymin = madL, ymax = madU,
				color = factor1)) +
				facet_grid(".~factor2")
			}
	
		if (method == "iqr") {
			p <- p + geom_pointrange(data = sumDat,
				aes(x = factor1, y = median, ymin = iqrL, ymax = iqrU,
				color = factor1)) +
				facet_grid(".~factor2")
			}		

		# Aesthetic #3
	
		facCounts <- count(data, vars = c(fac2, fac1))
		facCounts$label <- paste("n = ", facCounts$freq , sep = "")
		facCounts$y <- min(data$res) - 0.1*diff(range(data$res))
		facCounts <- facCounts[,-3]
		names(facCounts) <- c("cat2", "f1", "label", "y")
		p <- p + geom_text(aes(x = f1, y = y, label = label),
			color = "black", size = 4.0, data = facCounts)	

		} # End of TwoFac

	##########
	
	# Set up the color scheme & legend
	
	if (is.null(cols)) cols <- brewer.pal(length(levels(data[,fac1])), "Set1")
	
	p <- p  + scale_colour_manual(name = "", values = cols) +
		theme(axis.text.x = element_text(colour = "black"),
		axis.text.y = element_text(colour = "black"),
		axis.ticks = element_blank())		

	# Labeling
	
    if (!is.null(title)) p <- p + labs(title = title)
    if (!is.null(xlab)) p <- p + labs(xlab = xlab)
    if (!is.null(ylab)) p <- p + labs(ylab = ylab)

	invisible(p)
	}

