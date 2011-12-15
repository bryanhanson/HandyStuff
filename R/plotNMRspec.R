

plotNMRspec <-
function(peaks, x.range = c(12, 0), MHz = 300, ppHz = 1,
	labels = TRUE, lab.pos = NULL, ...) {
	
	# Function to draw a theoretical 1H NMR spectrum
	# Simple, 1st order coupling only
	# Bryan Hanson, DePauw Univ, Feb 2011

	# peaks must have columns named delta, mult, J, area, pw
	# J and pw should be expressed in Hz
	# pw is peak width at half height
	# Uses makeSpec with Lorentz curves to draw the spectrum	
	p <- peaks
	area <- ppm <- pw <- gr <- c()
	
	for (n in 1:nrow(p)) {
		# Get binomical coefficients for each part of multiplet
		z <- p$mult[n] - 1
		coef <- choose(z, 0:z) # a strange name for binomial!
		
		# Calc areas
		a <- coef * p$area[n]/sum(coef)
		
		# Calc chemical shifts
		no.pks <- length(coef)
		m <- jSeq(no.pks) 
		m <- p$J[n]*m
		m <- m + p$delta[n]*MHz # positions in Hz
		gr <- c(gr, rep(n, no.pks))
		area <- c(area, a)
		ppm <- c(ppm, m)
		pw <- c(pw, rep(p$pw[n], length(m)))
		}

	ans <- data.frame(area = area, x0 = ppm, gamma = pw/2)

	# p.ppm = points per ppm This way the data density
	# is independent of the requested x.range
	# This is esp. important for 13C since the peaks are narrow
	
	spec <- makeSpec(peak.list = ans, plot = FALSE,
		x.range = x.range*MHz, type = "lorentz", dd = ppHz, ...)

	if (!labels) y.lim <- range(spec[-c(1,2),])

	# Labels are actually the hardest part!  And slowest!
	
	if (labels) { # Figure out where the integral label will go...
		spec2 <- data.frame(group = as.factor(gr), spec[-c(1,2),])
		spec2 <- aggregate(.~group, data = spec2, FUN = "max")
		y.pos <- apply(spec2[,-1], MARGIN = 1, FUN = max)
#		cat("y.pos original =", y.pos, "\n")
		
		# Next part adds an extra offset if labels are close together
		# A better approach than this would be to calc exactly where
		# the label is and move high enough
		extra <- 0.5*min(y.pos)
		y.off <- rep(0, (length(y.pos)-1))
#		cat("y.off initially =", y.off, "\n")
		gap <- abs(diff(peaks$delta))
#		cat("gap =", gap, "\n")
		if (any(gap <= 0.3)) {
			for (n in 1:length(gap)) {
				if (gap[n] <= 0.3) y.off[n] <- 1
				}	
			}
#		cat("y.off final =", y.off, "\n")
		y.off <- c(0, extra*y.off)
		y.pos <- y.pos + y.off
#		cat("y.pos final =", y.pos, "\n")
		# End of extra offset calculation
		
		y.lim <- c(0, max(y.pos)*1.1) # Increase ylim a bit so integral labels are not cut off
		labs <- paste(p$area, "H", sep = " ") # add labels after the data is plotted (below)
		}
		
	plot(x = spec[1,]/MHz, y = spec[2,],
		type = "l", xlim = x.range, xlab = "chemical shift, ppm",
		ylab = "", axes = FALSE, ylim = y.lim, ...)
	axis(side = 1)
	
	if (labels) { # Now that the plotting window is open add the labels
		lp = 2
		if (!is.null(lab.pos)) lp <- lab.pos # need to add translation of L, R to 2, 4
		text(x = p$delta, y = y.pos, labels = labs, col = "red", pos = lp, offset = 0.5)
		}
		
	return(spec)
	}