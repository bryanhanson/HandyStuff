gaussCurve <-
function(x, area, mu, sigma, tail) {
	
	# Function to generate Gaussian curves
	# Bryan Hanson, DePauw Univ, July 2010
	# This version handles tailing

	m <- mu
	if (is.na(tail)) s <- sigma
	if (!is.na(tail)) s <- sigma*tail*x
	numerator <- exp(-1.0 * ((x - m)^2)/(2*s^2))
	denominator <- s*sqrt(2*pi)
	y <- area*numerator/denominator
	}

