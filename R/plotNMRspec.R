

plotNMRspec <-
function(peaks, x.range = c(12, 0), MHz = 300, ...) {
	
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

	spec <- makeSpec(peak.list = ans, plot = FALSE,
		x.range = x.range*MHz, type = "lorentz")

	spec2 <- data.frame(group = as.factor(gr), spec[-c(1,2),])
	spec2 <- aggregate(.~group, data = spec2, FUN = "max")
	y.pos <- apply(spec2[,-1], MARGIN = 1, FUN = max)

	plot(x = spec[1,]/MHz, y = spec[2,],
		type = "l", xlim = x.range, xlab = "chemical shift, ppm",
		ylab = "", axes = FALSE, ...)
	axis(side = 1)
	labs <- paste(p$area, "H", sep = " ")
	text(x = p$delta, y = y.pos, labels = labs, col = "red", pos = 2, offset = 0.5)
	return(spec)
	}