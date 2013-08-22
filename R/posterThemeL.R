posterThemeL <- function() {
	
	# lattice settings resembling ggplot2 for use on posters
	# Values & concept derived from latticeExtra::ggplot2like()
	# Also see trellis.par.get()
	
	L <- list(
	
	# background, grids etc
	axis.line = list(col = "transparent"),
	axis.text = list(lineheight = 0.9, col = "grey50"), # fontsize below
	panel.background = list(col = "grey90"),
	reference.line = list(col = "white"),
	strip.background = list(col = c("grey70", "grey50", "grey40")),
	strip.border = list(col = "transparent"),
	
	# overall font and point size
	fontsize = list(text = 14, points = 15),
	
	# symbols and lines
	plot.symbol = list(pch = 20, cex = 10), # seems to be ignored by everything
	superpose.symbol = list(cex = 0.5, pch = 20), # controls freckles in rxnNormL not compareCatsL
#	superpose.line = list(lwd = 5), # does nothing?
	add.line = list(lwd = 3), # controls summary lines
	# par.main.text = list(cex = 1.5), # individual control if desired
	# par.xlab.text = list(cex = 1.2),
	# par.ylab.text = list(cex = 1.2),
	# par.sub.text = list(cex = 1.2),
	# add.text = list(cex = 1.2), # lengend entries, not legend title
	
	# these next ones apply to boxplots (not an option in rxnNormL)
    box.dot = list(col = "black", pch = 18, cex = 1.5),
    box.rectangle = list(col = "transparent"),
    box.umbrella = list(lty = 1, lwd = 1.5)
    # The fill of the bwplot is controlled in drawing code
	)
	L
	}
