lmEqnL <-
function(formula = NULL, data = NULL,
	method = "lm", leg.loc = c(0.5, 0.5),
	poster = FALSE, ...) {
	
	# This is the version that uses Lattice graphics
	# compare to lmEqn which uses ggplot2 graphics
	
	# Process & check formula
	
	if (is.null(formula)) stop("Formula not given")
	if (!is.formula(formula)) stop("Invalid formula specification")
	if (!length(formula) == 3) stop("Formula must include both LHS and RHS terms, e.g.: response ~ variable")
	if (length(all.vars(formula[[3]])) > 1) stop("Formula can only take the form response ~ variable")

	res <- as.character(formula[[2]])
	v <- as.character(formula[[3]])

	# Compute the linear model

	if (method == "lm") mod <- lm(formula, data)
	if (method == "rlm") mod <- rlm(formula, data)
	m <- mod$coef[2]
	b <- mod$coef[1]
	r2 <- round(cor(data[,v], data[,res])^2, 4)
	Lab <- paste("m =", round(m, 3), "b =", round(b, 3), "r^2 =", r2, sep = "  ")
	if (method == "lm") Lab <- paste("linear model: ", Lab)
	if (method == "rlm") Lab <- paste("robust linear model: ", Lab)

	# Make the plot
	if (poster) ps = posterThemeL()
	if (!poster) ps = screenThemeL()

	mypanel <- function(x, y, ...) {
		panel.xyplot(x, y, type = "p", ...)
		panel.text(leg.loc[1], leg.loc[2], labels = Lab, adj = 0, ...)
		panel.lmline(x, y, a = mod, ...)

		}
	
	xyplot(formula, data, panel = mypanel, par.settings = ps, axis = axis.grid, ...)
	
	}

