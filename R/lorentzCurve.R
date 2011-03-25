lorentzCurve <-
function(x, x0, area, gamma) {
	
	# Function to generate Lorentzian curves
	# Bryan Hanson, DePauw Univ, Feb 2011
	# Derived from gaussCurve
	# gamma is peak width @ half height
	# x0 is the center of the peak
	# x is a vector of values @ which the pdf should be computed
	# area is the peak area

	y <- gamma/((x - x0)^2 + gamma^2)
	y <- y*area/pi
	}

