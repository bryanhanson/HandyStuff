#' Plot Points, Fit a Line, Display Equation
#' 
#' This function plots the points given, then fits a line and displays the
#' regression statistics using \code{lattice}.
#' 
#' @param formula A formula with both LHS and RHS.  The formula should comply
#' with the usual \code{R} standards, for instance \code{y ~ x}.
#'
#' @param data A data frame containing two columns whose names correspond to
#' the specified formula.
#'
#' @param method A character string.  Either "lm" for linear model or "rlm" for
#' a robust fit.
#'
#' @param leg.loc A vector of two numbers (x, y) giving the location of the
#' legend.  The values correspond to the units of the data. See the examples.
#'
#' @param theme Character; A suitble \code{lattice} theme.  
#' There are two built-in themes which you can use "as is" or modify to your heart's
#' content.  If  none is given, \code{\link{screenTheme}} will be used.  The other option
#' provided is \code{\link{posterTheme}}.
#'
#' @param \dots Additional arguments to be passed downstream.
#'
#' @return A plot is drawn and the \code{ggplot2} or \code{lattice} object is
#' returned, possibly for further manipulation.
#'
#' @export
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#' @keywords utilities plot
#' @examples
#' 
#' require("lattice")
#' require("latticeExtra")
#' require("plyr")
#' require("MASS")
#'
#' conc = seq(1, 12, length.out = 8)
#' abs = jitter(conc, factor = 2)*0.1
#' cc <- data.frame(conc, abs)
#' lmEqn(formula = abs ~ conc, data = cc, method = "rlm",
#' leg.loc = c(3, 0.1),
#' xlab = "concentration", ylab = "absorbance",
#' main = "Calibration Curve")
#' 
lmEqn <-
function(formula = NULL, data = NULL,
	method = "lm", leg.loc = c(0.5, 0.5),
	theme = screenTheme(), ...) {
	
	if (!requireNamespace("lattice", quietly = TRUE)) {
	stop("You need to install package lattice to use this function")
	}
	if (!requireNamespace("latticeExtra", quietly = TRUE)) {
	stop("You need to install package latticeExtra to use this function")
	}
	if (!requireNamespace("plyr", quietly = TRUE)) {
	stop("You need to install package plyr to use this function")
	}
	if (!requireNamespace("MASS", quietly = TRUE)) {
	stop("You need to install package MASS to use this function")
	}
	
	# Process & check formula
	
	if (is.null(formula)) stop("Formula not given")
	if (!plyr::is.formula(formula)) stop("Invalid formula specification")
	if (!length(formula) == 3) stop("Formula must include both LHS and RHS terms, e.g.: response ~ variable")
	if (length(all.vars(formula[[3]])) > 1) stop("Formula can only take the form response ~ variable")

	res <- as.character(formula[[2]])
	v <- as.character(formula[[3]])

	# Compute the linear model

	if (method == "lm") mod <- stats::lm(formula, data)
	if (method == "rlm") mod <- MASS::rlm(formula, data)
	m <- mod$coef[2]
	b <- mod$coef[1]
	r2 <- round(stats::cor(data[,v], data[,res])^2, 4)
	Lab <- paste("m =", round(m, 3), "b =", round(b, 3), "r^2 =", r2, sep = "  ")
#	Lab <- paste("m =", round(m, 3), "b =", round(b, 3), bquote(r^2 == .(r2)), sep = "  ")
	if (method == "lm") Lab <- paste("linear model: ", Lab)
	if (method == "rlm") Lab <- paste("robust linear model: ", Lab)

	# Make the plot
	ps = theme

	mypanel <- function(x, y, ...) {
		lattice::panel.xyplot(x, y, type = "p", ...)
		lattice::panel.text(leg.loc[1], leg.loc[2], labels = Lab, adj = 0, ...)
		lattice::panel.lmline(x, y, a = mod, ...)

		}
	
	lattice::xyplot(formula, data, panel = mypanel, par.settings = ps, axis = latticeExtra::axis.grid, ...)
	
	}

