lmEqn <-
function(formula = NULL, data = NULL,
	method = "lm", leg.loc = c(0.5, 0.5),
	xlab = NULL, ylab = NULL, title = NULL, ...) {
	
	# Process & check formula
	
	if (is.null(formula)) stop("Formula not given")
	if (!is.formula(formula)) stop("Invalid formula specification")
	if (!length(formula) == 3) stop("Formula must include both LHS and RHS terms, e.g.: response ~ variable")
	if (length(all.vars(formula[[3]])) > 1) stop("Formula can only take the form response ~ variable")

	res <- as.character(formula[[2]])
	v <- as.character(formula[[3]])

	# Compute the linear model

	if (method == "lm") mod <- lm(formula, data, ...)
	if (method == "rlm") mod <- rlm(formula, data, ...)
	m <- mod$coef[2]
	b <- mod$coef[1]
	r2 <- round(cor(data[,v], data[,res])^2, 4)
	Lab <- paste("m =", round(m, 3), "b =", round(b, 3), "r^2 =", r2, sep = "  ")
	if (method == "lm") Lab <- paste("linear model: ", Lab)
	if (method == "rlm") Lab <- paste("robust linear model: ", Lab)

	# Make the plot
	
	p <- ggplot(data = data, aes_string(x = v, y = res))
	p <- p + geom_point()
	if (method == "lm") p <- p + geom_smooth(method = "lm")
	if (method == "rlm") p <- p + geom_smooth(method = "rlm")

	p <- p + annotate("text", label = Lab, x = leg.loc[1], y = leg.loc[2],
		size = 5, hjust = 0, vjust = 0)
		
	if (!is.null(title)) p <- p + labs(title = title)
    if (!is.null(xlab)) p <- p + labs(xlab = xlab)
    if (!is.null(ylab)) p <- p + labs(ylab = ylab)

	p
	
	}

