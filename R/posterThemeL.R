posterThemeL <- function() {
	
	# lattice settings resembling ggplot2 for use on posters
	# Values & concept derived from latticeExtra::ggplot2like()
	
	L <- list(
	axis.line = list(col = "transparent"),
	axis.text = list(cex = 0.8, lineheight = 0.9, col = "grey50"),
	panel.background = list(col = "grey90"),
	reference.line = list(col = "white"),
	plot.symbol = list(col = "black", pch = 19, cex = 0.3),
	strip.background = list(col = c("grey70", "grey50", "grey40")),
	strip.border = list(col = "transparent"),
	add.text = list(cex = 0.8),
    box.dot = list(col = "black", pch = 18, cex = 1.5),
    box.rectangle = list(col = "transparent"),
    box.umbrella = list(lty = 1, lwd = 1.5)
    # The fill of the bwplot is controlled in drawing code
	)
	L
	}
