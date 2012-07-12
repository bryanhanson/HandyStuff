lmEqn <-
function(df = NULL, y = NULL, x = NULL,
	method = "lm", leg.loc = c(0, 0),
	xlab = NULL, ylab = NULL, title = NULL, ...) {
	
# work out a linear model first

	y1 <- df[,y]
	x1 <- df[,x]
	if (method == "lm") mod <- lm(y1 ~ x1)
	if (method == "rlm") mod <- rlm(y1 ~ x1)
	m <- mod$coef[2]
	b <- mod$coef[1]
	r2 <- round(cor(x1, y1)^2, 4)
	Lab <- paste("m =", round(m, 3), "b =", round(b, 3), "r^2 =", r2, sep = "  ")
	if (method == "lm") Lab <- paste("linear model: ", Lab)
	if (method == "rlm") Lab <- paste("robust linear model: ", Lab)

# now plot the data

	p <- ggplot(df, aes_string(x = x, y = y))
	p <- p + geom_point()
	if (method == "lm") p <- p + geom_smooth(method = "lm")
	if (method == "rlm") p <- p + geom_smooth(method = "rlm")

	p <- p + annotate("text", label = Lab, x = leg.loc[1], y = leg.loc[2],
		size = 5, hjust = 0, vjust = 0)
		
	if (!is.null(title)) p <- p + opts(title = title)
    if (!is.null(xlab)) p <- p + xlab(xlab)
    if (!is.null(ylab)) p <- p + ylab(ylab)

	p
	
	}

