rxnNorm <-
function(data = NULL, res = NULL,
	fac1 = NULL, fac2 = NULL, fac2cols = NULL,
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
	
	if (is.null(fac2cols)) stop("You need to supply fac2cols")
	
	# 1st, reduce the df and clean it of NAs
	# otherwise there is trouble with the counts
	# plus this keeps the ggplot2 object size down
	
	keep <- c(res, fac1, fac2)
	data <- data[, keep]
	data <- na.omit(data)

	# Specify the data & aesthetics
		
	p <- ggplot(data, aes_string(x = fac1, y = res, color = fac2,
		group = fac2))

	# now add layers as requested

	if (freckles) {
		jit <- position_jitter(width = 0.05, height = 0.0)
		p <- p + geom_jitter(position = jit, size = 1.0) 
		}
	
	if (method == "box") { # this doesn't work right
		p <- p + geom_boxplot(width = 0.2)
		if (type == "connect") {
			p <- p + stat_summary(fun.y = "median", geom = "line")
				}
		}
		
	if (method == "sem") {
		p <- p + stat_summary(fun.data = "seXy")
		if ((type == "connect") | (type == "anova")) {
			p <- p + stat_summary(fun.y = "mean", geom = "line")
				}
		}
		
	if (method == "sem95") {
		p <- p + stat_summary(fun.data = "seXy95")
		if ((type == "connect") | (type == "anova")) {
			p <- p + stat_summary(fun.y = "mean", geom = "line")
				}
		}
		
	if (method == "mad") {
		p <- p + stat_summary(fun.data = "seXyMad")
		if ((type == "connect") | (type == "anova")) {
			p <- p + stat_summary(fun.y = "median", geom = "line")
				}
		}
		
	if (method == "iqr") {
		p <- p + stat_summary(fun.data = "seXyIqr")
		if ((type == "connect") | (type == "anova")) {
			p <- p + stat_summary(fun.y = "median", geom = "line")
				}
		}
	
	if (type == "fitLine") {
		p <- p + geom_smooth(method = "lm", se = FALSE)
		
		}
		
	# now add the common decorations & modifications
	
	p <- p  + scale_colour_manual(name = "", values = fac2cols) +
		opts(axis.text.x = theme_text(colour = "black"),
		axis.text.y = theme_text(colour = "black"),
		axis.ticks = theme_blank()) + opts(...)		

	# now add labels and fix limits (modified from qplot)
	
    if (!is.null(title)) p <- p + opts(title = title)
    if (!is.null(xlab)) p <- p + xlab(xlab)
    if (!is.null(ylab)) p <- p + ylab(ylab)
#    if (exists("xlim")) p <- p + xlim(xlim)
#    if (exists("ylim")) p <- p + ylim(ylim)

	# now add summary tables
	# using tableGrob from gridExtra

	if (!is.null(table)) {
		
		if (type == "connect") {
			counts <- count(data, vars = c(fac2, fac1))
			colnames(counts) <- c(fac2, fac1, "count")

			myt <- tableGrob(counts, show.box = TRUE,
				show.rownames = FALSE, show.colnames = TRUE,
				show.csep = TRUE, show.rsep = TRUE,
				separator = "black", gp = gpar(cex = table[3]))

			p <- p + annotation_custom(myt, xmin = table[1]-0.5, xmax = table[1]+0.5,
				ymin = table[2]-0.5, ymax = table[2]+0.5)
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

