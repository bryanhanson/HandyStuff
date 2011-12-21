compareCats <-
function(data = NULL, res = NULL,
	fac1 = NULL, fac2 = NULL,
	fac1cols = NULL, freckles = FALSE,
	method = c("sem", "iqr", "mad", "box", "points", "sem95"),
	xlab = NULL, ylab = NULL, title = NULL, ...)
	{

# Function to compare categorical data by
# plotting means and se's or something similar
# Bryan Hanson, DePauw Univ, Jan 2010
	
	require(ChemoSpec)
	if (is.null(fac1cols)) stop("You need to supply fac1cols")
	
	# 1st, reduce the df and clean it of NAs
	# otherwise there is trouble with the counts
	
	if (!identical(fac2, NULL)) {
		keep <- c(res, fac1, fac2)
		} else {
			keep <- c(res, fac1)
			}
	data <- data[, keep]
	data <- na.omit(data)

	# Specify the data & aesthetics
		
	if (identical(fac2, NULL)) { 
		p <- ggplot(data, aes_string(x = fac1, y = res, color = fac1))
		
		} else {
			p <- ggplot(data, aes_string(x = fac1, y = res, color = fac1)) +
			facet_grid(paste(". ~", fac2, sep = ""))
			}
				
	# now add layers as requested
	
	if (freckles) {
		if (method == "points") warning("It doesn't make sense to combine points with freckles!")
		jit <- position_jitter(width = 0.1, height = 0.0)
		p <- p + layer(geom = "jitter", position = jit, color = "black", size = 1) 
		}

	if (method == "points") {
		jit <- position_jitter(width = 0.05, height = 0.0)
		p <- p + layer(geom = "jitter", position = jit)		}
		
	if (method == "box") {
		p <- p + geom_boxplot(width = 0.2, outlier.shape = 2)
		}
	
	if (method == "sem") {
		p <- p + stat_summary(fun.data = "seXy")		}
	
	if (method == "sem95") {
		p <- p + stat_summary(fun.data = "seXy95")
		}

	if (method == "mad") {
		p <- p + stat_summary(fun.data = "seXyMad")
		}
		
	if (method == "iqr") {
		p <- p + stat_summary(fun.data = "seXyIqr")
		}

	# now add the common decorations & modifications
	
	p <- p  + scale_colour_manual(values = fac1cols) +
		opts(axis.text.x = theme_text(colour = "black"),
		axis.text.x = theme_text(colour = "black"),
		axis.ticks = theme_blank(), legend.position = "none", ...)
		
	# now add labels and fix limits (modified from qplot)
	
    if (!is.null(title)) p <- p + opts(title = title)
    if (!is.null(xlab)) p <- p + xlab(xlab)
    if (!is.null(ylab)) p <- p + ylab(ylab)
#    if (exists("xlim")) p <- p + xlim(xlim)
#    if (exists("ylim")) p <- p + ylim(ylim)
		
	
	p <- p + geom_text(aes_string(x = fac1,
		y = paste("min(",res,") - 0.1 * diff(range(",res,"))", sep=""),
		label = 'paste("n = ", ..count.. , sep = "")'),
		color = "black", size = 4.0, stat = "bin", data = data)

	invisible(p)
	}

