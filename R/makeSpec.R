makeSpec <-
function(peak.list, x.range, plot = TRUE, curves = FALSE,
	type = "gauss", noise = 0, ...) {

# Function to generate sample spectra or chromatograms
# Bryan Hanson, DePauw Univ, July 2010

	# For Gaussian curves:
	# peak.list must contain area, mu, sd, tail
	# tailing is handled by making sd a function of x (time)
	# tailing can be disabled entirely with tail = NA

	# For Lorentzian curves:
	# peak.list must contain area, x0, gamma
	# tailing is not relevant
	# Conceptually, x0 ~ mu, gamma ~ sd

	if (type == "gauss") {		
		pl <- peak.list
		ns <- length(pl$mu) # ns = no. of spec
		if (is.null(pl$tail)) pl$tail <- rep(NA, ns)
	
		# create x-data, initialize y-data
		# y.mat will hold each spectrum separately
	
		x <- seq(from = x.range[1], to = x.range[2], length.out = 1000)
		y.mat <- matrix(data = NA, nrow = ns, ncol = 1000)
	
		for (n in 1:ns) {
			y.mat[n,] <- gaussCurve(x = x, area = pl$area[n], mu = pl$mu[n],
				sigma = pl$sd[n], tail = pl$tail[n])
			}

		rn <- list()
		for (n in 1:ns) {
			rn[n] <- paste("area", pl$area[n], "mu", pl$mu[n], "sigma", pl$sd[n], "tail", pl$tail[n], sep = " ")
			}
		}

	if (type == "lorentz") {		
		pl <- peak.list
		ns <- length(pl$x0) # ns = no. of spec
		x <- seq(from = x.range[1], to = x.range[2], length.out = 1000)
		y.mat <- matrix(data = NA, nrow = ns, ncol = 1000)
	
		for (n in 1:ns) {
			y.mat[n,] <- lorentzCurve(x = x, area = pl$area[n],
				x0 = pl$x0[n], gamma = pl$gamma[n])
			}

		rn <- list()
		for (n in 1:ns) {
			rn[n] <- paste("area", pl$area[n], "x0", pl$x0[n], "gamma", pl$gamma[n], sep = " ")
			}
		}

	dimnames(y.mat)[[1]] <- rn
	
	if (!noise == 0) { y.mat <- jitter(y.mat, factor = noise)}

	y.sum <- colSums(y.mat)
	all <- rbind(x, y.sum, y.mat)

	if (plot) {
		plot(x, y.sum, type = "l", lwd = 2, col = "black", xlim = x.range, ...)
		if (curves) for (n in 1:ns) lines(x, y.mat[n,], lwd = 1.0, col = "blue")
		}
	
	return(all)
	}

