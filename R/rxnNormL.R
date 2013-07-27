rxnNormL <-
function(formula = NULL, data = NULL, groups = NULL,
	cols = NULL, freckles = FALSE, type = "connect",
	method = c("sem", "sem95", "iqr", "mad"),
	table = NULL, poster = FALSE, ...) {

# Function to compare categorical data by
# plotting means etc in the same panel
# Options to connect the dots or do a fit
# and display some nice summary tables
# Bryan Hanson, DePauw Univ, July 2010

# This is the lattice version, February 2013
# We are using formula, data and groups close to how lattice handles them

	# Check and process the formula & data types
	
	if (is.null(formula)) stop("Formula not given")
	if (!is.formula(formula)) stop("Invalid formula specification")

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
	data <- na.omit(data)

	args <- as.list(match.call(expand.dots = FALSE)[-1]) # used a bit later
	
	# Set up the color scheme if not provided
	
	nc <- length(levels(data[,grn]))
	if (is.null(cols)) {
		cols <- brewer.pal(nc, "Set1")
		if (nc == 2) cols <- cols[1:2] # cannot keep the extra due to recycling problems
		}
	if (!is.null(cols)) {
		if (!length(cols) == nc) stop("cols must be the same length as the levels in groups")
		}

### This panel will compute & plot summary statistics
	
	panel.summary <- function(x, y, ...) {
		
		mean <- aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = mean)
		med <- aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = median)
		sexy <- aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = seXy)
		sexy95 <- aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = seXy95)
		sexymad <- aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = seXyMad)
		sexyiqr <- aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = seXyIqr)
		sumDat <- cbind(mean, med[,3], sexy[[3]][,c(2,3)],
			sexy95[[3]][,c(2,3)], sexymad[[3]][,c(2,3)], sexyiqr[[3]][,c(2,3)])
		names(sumDat) <- c("xx", "gr", "mean", "median",
			"semL", "semU", "sem95L", "sem95U", "madL", "madU",
			"iqrL", "iqrU")
		gr <- NULL # Needed for next command to fake out check mechanism
		sumDat <- arrange(sumDat, xx, gr)
		sumDat$cols <- rep(cols, length(unique(sumDat$xx)))

		for (i in 1:nrow(sumDat)) {
		
			# Plot the mean or median
					
			if ((method == "sem") | (method == "sem95")) {
				panel.points(x = sumDat[i,1], y = sumDat[i,3], col = sumDat[i, 13], pch = 19, ...)
				}
				
			if ((method == "mad") | (method == "iqr")) {
				panel.points(x = sumDat[i,1], y = sumDat[i,4], col = sumDat[i, 13], pch = 19, ...)
				}
	
			# Plot the 'whiskers'
	
			if (method == "sem") {
				panel.segments(x0 = sumDat[i,1], y0 = sumDat[i,5],
				x1 = sumDat[i,1], y1 = sumDat[i,6], lwd = 3, col = sumDat[i,13], ...)		
				}
	
			if (method == "sem95") {
				panel.segments(x0 = sumDat[i,1], y0 = sumDat[i,7],
				x1 = sumDat[i,1], y1 = sumDat[i,8], lwd = 3, col = sumDat[i, 13], ...)		
				}
	
			if (method == "mad") {
				panel.segments(x0 = sumDat[i,1], y0 = sumDat[i,9],
				x1 = sumDat[i,1], y1 = sumDat[i,10], lwd = 3, col = sumDat[i, 13], ...)		
				}
	
			if (method == "iqr") {
				panel.segments(x0 = sumDat[i,1], y0 = sumDat[i,11],
				x1 = sumDat[i,1], y1 = sumDat[i,12], lwd = 3, col = sumDat[i, 13], ...)		
				}
			}
			
		} # end of panel.summary

### This panel will connect the means or medians
	
	panel.connect <- function(x, y, ...) {
		
		mean <- aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = mean)
		med <- aggregate(data[,yy] ~ data[,xx]*data[, grn], data, FUN = median)
		sumDat <- cbind(mean, med[,3])
		names(sumDat) <- c("xx", "gr", "mean", "median")
		gr <- NULL # needed to fake out check mechanism for next command
		sumDat <- arrange(sumDat, gr, xx)
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
				panel.segments(x0 = sumDat[i,1], y0 = sumDat[i,3],
					x1 = sumDat[i+1,1], y1 = sumDat[i+1,3], col = sumDat[i, 5], ...)
				}
			if ((method == "mad") | (method == "iqr")) {
				panel.segments(x0 = sumDat[i,1], y0 = sumDat[i,4],
					x1 = sumDat[i+1,1], y1 = sumDat[i+1,4], col = sumDat[i, 5], ...)
				}
			}
		} # end of panel.connect

### Now prepare summary tables using tableGrob from gridExtra

	if (!is.null(table)) {
		
		if (type == "connect") {
			if (!is.factor(data[,xx])) {
				msg <- paste(xx, "must be a factor for a table of counts")
				stop(msg)
				}
			counts <- count(data, vars = c(grn, xx))
			colnames(counts) <- c(grn, xx, "count")

			myt <- tableGrob(counts, show.box = TRUE,
				show.rownames = FALSE, show.colnames = TRUE,
				show.csep = TRUE, show.rsep = TRUE,
				separator = "black", gp = gpar(cex = table[3]))
			}

		if (type == "anova") {
			if (!is.factor(data[,xx])) {
				msg <- paste(xx, "must be a factor for anova")
				stop(msg)
				}
			f <- paste(".~.*", deparse(substitute(groups)))
			nf <- update.formula(formula, f)
			mod <- aov(formula = nf, data = data)
			mod <- summary(mod)[[1]]
			mod[,2:4] <- round(mod[,2:4], 2)
			mod[,5] <- format(mod[,5], scientific = TRUE, digits = 4)

			myt <- tableGrob(mod, show.box = TRUE,
				show.rownames = TRUE, show.colnames = TRUE,
				show.csep = FALSE, show.rsep = FALSE, core.just = "right", row.just = "right", 
				separator = "black", gp = gpar(cex = table[3]),
				gpar.rowtext = gpar(col = "black", cex = 1, fontface = "bold"))

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
				mod <- lm(dat[,yy] ~ dat[,xx])
				m[i] <- round(mod$coef[2], 2)
				b [i]<- round(mod$coef[1], 2)
				r2[i] <- round(cor(dat[,xx], dat[,yy])^2, 4)		
				}
				
			mod.yy <- data.frame(line = lvls, m = m, b = b, r2 = r2)
				
			myt <- tableGrob(mod.yy, show.box = TRUE,
				show.rownames = FALSE, show.colnames = TRUE,
				show.csep = TRUE, show.rsep = TRUE,
				separator = "black", gp = gpar(cex = table[3]))
				
			}
		} # End of table preparations

##### Now the high level plot calls

	if (poster) {
		ps <- posterThemeL()
		kts <- posterThemeL()$fontsize$text*0.08 # personal taste
		}
	if (!poster) {
		ps <- screenThemeL()
		kts <- screenThemeL()$fontsize$text*0.08 # personal taste
		}

	p <- xyplot(as.formula(args$formula),
  		data = eval(args$data), 
        groups = eval(args$groups), ...,
		axis = axis.grid,
		par.settings = ps,
		panel = function(x, y, ...) {
			panel.summary(x, y, ...)
			if (freckles) panel.xyplot(x, y, jitter.x = TRUE, col = cols, ...)
			if ((type == "connect") | (type == "anova")) panel.connect(x, y, ...)
			if (type == "fitLine") panel.xyplot(x, y, type = "r", col = cols, ...)
			if (!is.null(table)) {
				if (type == "connect") {
					upViewport(0)
					tv <- viewport(x = table[1], y = table[2])
					pushViewport(tv)
					grid.draw(myt)
					}
				if (type == "anova") {
					upViewport(0)
					tv <- viewport(x = table[1], y = table[2])
					pushViewport(tv)
					grid.draw(myt)
					}
				if (type == "fitLine") {
					upViewport(0)
					tv <- viewport(x = table[1], y = table[2])
					pushViewport(tv)
					grid.draw(myt)
					}
				} # end of table options
			}, # end of panel function
		auto.key = list(space = "right", col = cols, points = FALSE, title = grn, cex.title = kts)
			)
	
	invisible(p)
	}

