rxnNorm <-
function(formula = NULL, data = NULL, cols = NULL,
	freckles = FALSE, type = "connect",
	method = c("sem", "iqr", "mad", "box", "sem95"),
	table = NULL, xlab = NULL, ylab = NULL, title = NULL, ...) {

# Function to compare categorical data by
# plotting means etc in the same panel
# Option to connect the dots or do a fit
# Bryan Hanson, DePauw Univ, July 2010

# Both fac1 and fac2 must be given: this is G x E!
# fac2 must be a factor, but fac1 can be either factor or numeric
# Everything related to a particular level of fac2 should be colored the same
	
	# Check and process the formula
	
	if (is.null(formula)) stop("Formula not given")
	if (!is.formula(formula)) stop("Invalid formula specification")
	if (!length(formula) == 3) stop("Formula must include both LHS and RHS terms, e.g.: response ~ factor1*factor2")
	if (!length(all.vars(formula[[3]])) == 2) stop("Formula must include interaction terms, e.g.: response ~ factor1*factor2")
	if (!formula[[3]][[1]] == "*") stop("Formula must include interaction terms, e.g.: response ~ factor1*factor2")

	res <- as.character(formula[[2]])
	fac1 <- as.character(formula[[3]][2])
	fac2 <- as.character(formula[[3]][3])
	
	# Reduce the df and clean it of NAs
	# otherwise there is trouble with the counts
	# plus this keeps the ggplot2 object size down
	
	keep <- c(res, fac1, fac2)
	data <- data[, keep]
	data <- na.omit(data)

	if (!is.factor(data[,fac2])) stop("factor2 was not actually of type factor")

	# Initialize + add the points, jittered or not
	# This is aesthetic #1
	
	p <- ggplot()
	
	if (!freckles) {
		p <- p + geom_point(data = data, 
		aes_string(x = fac1, y = res, color = fac2, group = fac2))
		}

	if (freckles) {
		jit <- position_jitter(width = 0.05, height = 0.0)
		p <- p + geom_jitter(data = data,
		aes_string(x = fac1, y = res, color = fac2, group = fac2),
		position = jit, size = 1.0)
		}

	# Set up the color scheme & legend
	
	if (is.null(cols)) cols <- brewer.pal(length(levels(data[,fac2])), "Set1")
	
	p <- p  + scale_colour_manual(name = "", values = cols) +
		theme(axis.text.x = element_text(colour = "black"),
		axis.text.y = element_text(colour = "black"),
		axis.ticks = element_blank())		

	# Do fitLine next as it uses the same aesthetics
	
	if (type == "fitLine") {
		p <- p + geom_smooth(data = data, se = FALSE, method = "lm",
		aes_string(x = fac1, y = res, color = fac2, group = fac2))
		}

	# Boxplot does its own summary
	
	if (method == "box") { # Does not work correctly
		p <- p + geom_boxplot(data = data, width = 0.2,
		aes_string(x = fac1, y = res, color = fac2, group = fac2))
		}

	# All plotting below here uses this summary data
	# Prepare the summary data (required for all other options)
	# Due to a bug in ggplot2_0.9.3, we must calc some quantities
	# and put them in a separate data frame for a new aesthetic
	# Slightly wasteful to compute all when you may not use all, but...

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

	# Now add summary layers as requested
	# These are aesthetic #2, using a new data set

	if (method == "sem") {
		p <- p + geom_pointrange(data = sumDat,
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
	
	# Now add the appropriate lines
	# New layers, but using aes #2 explicitly
		
	if ((type == "connect") | (type == "anova")) {
		
		if ((method == "sem") | (method == "sem95")) {
			p <- p + geom_line(data = sumDat,
			aes(x = factor1, y = mean,
			color = factor2, group = factor2))
			}
			
		if ((method == "mad") | (method == "iqr")) {
			p <- p + geom_line(data = sumDat,
			aes(x = factor1, y = median,
			color = factor2, group = factor2))
			}
		}

	# now add labels and fix limits (modified from qplot)
	
    if (!is.null(title)) p <- p + labs(title = title)
    if (!is.null(xlab)) p <- p + labs(xlab = xlab)
    if (!is.null(ylab)) p <- p + labs(ylab = ylab)
#    if (exists("xlim")) p <- p + xlim(xlim)
#    if (exists("ylim")) p <- p + ylim(ylim)

	# now add summary tables
	# using tableGrob from gridExtra

	if (!is.null(table)) {
		
		if (type == "connect") {
			counts <- count(data, vars = c(fac2, fac1))
			colnames(counts) <- c(fac2, fac1, "count")

			# myt <- tableGrob(counts, show.box = TRUE,
				# show.rownames = FALSE, show.colnames = TRUE,
				# show.csep = TRUE, show.rsep = TRUE,
				# separator = "black", gp = gpar(cex = table[3]))
			
			# p <- p + annotation_custom(grob=circleGrob(r=unit(1,"npc")), xmin = table[1]-0.5, xmax = table[1]+0.5,
				# ymin = table[2]-0.5, ymax = table[2]+0.5)
			#p <- p + annotation_custom(circleGrob())
			pp <- qplot(1, 1, geom = "blank") + annotation_custom(circleGrob())
			p <- p + annotation_custom(ggplotGrob(pp))
			}

		if (type == "anova") {
			if (!is.null(fac2)) form <- as.formula(paste(res, "~", paste(fac1, fac2, sep = "*")))
			if (is.null(fac2)) form <- as.formula(paste(res, "~", fac1, sep = ""))
			mod <- aov(formula = form, data = data)
			mod <- summary(mod)[[1]]
			mod[,2:4] <- round(mod[,2:4], 2)
			mod[,5] <- signif(mod[,5], 4)

			myt <- tableGrob(mod, show.box = TRUE,
				show.rownames = FALSE, show.colnames = TRUE,
				show.csep = TRUE, show.rsep = TRUE,
				separator = "black", gp = gpar(cex = table[3]))

			p <- p + annotation_custom(myt, xmin = table[1]-0.5, xmax = table[1]+0.5,
				ymin = table[2]-0.5, ymax = table[2]+0.5)
			
			}

		if (type == "fitLine") {
			lvls <- levels(data[,fac2])
			nl <- length(lvls)
			m <- c()
			b <- c()
			r2 <- c()
			for (i in 1:nl) {
				dat <- subset(data, data[,fac2] == lvls[i])
				mod <- lm(dat[,res] ~ dat[,fac1])
				m[i] <- round(mod$coef[2], 2)
				b [i]<- round(mod$coef[1], 2)
				r2[i] <- round(cor(dat[,fac1], dat[,res])^2, 4)
				
				}
				
			mod.res <- data.frame(line = lvls, m = m, b = b, r2 = r2)
				
			myt <- tableGrob(mod.res, show.box = TRUE,
				show.rownames = FALSE, show.colnames = TRUE,
				show.csep = TRUE, show.rsep = TRUE,
				separator = "black", gp = gpar(cex = table[3]))

			p <- p + annotation_custom(myt, xmin = table[1]-0.5, xmax = table[1]+0.5,
				ymin = table[2]-0.5, ymax = table[2]+0.5)
				
			}
		}	
	invisible(p)
	}

