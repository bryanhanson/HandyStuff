

qMS <- function(f = NULL, xlab = "m/z",
	ylab = "intensity", main = "Mass Spectrum", ...) {

	# Quick MS Spectrum
	
	# Function to draw the mass spectrum of the molecular
	# ion for a given formula
	# Bryan Hanson, DePauw Univ. Dec. 2010
	
	if (is.null(f)) stop("No formula provided")
		
	# Process formula
	# Thanks to Gabor Grothendieck for this approach
	
	DF <- strapply(f,
		"([A-Z][a-z]*)(\\d*)",
		~ c(..1, if (nchar(..2)) ..2 else 1),
		simplify = ~ as.data.frame(t(matrix(..1, 2)), stringsAsFactors = FALSE),
		engine = "R")
		
	DF[[2]] <- as.numeric(DF[[2]])
	colnames(DF) <- c("element", "count")
	
	DF$mass <- rep(NA, nrow(DF))
	for (n in 1:nrow(DF)) {
		if (DF$element[n] == "C") DF$mass[n] <- 12L
		if (DF$element[n] == "H") DF$mass[n] <- 1L
		if (DF$element[n] == "N") DF$mass[n] <- 14L
		if (DF$element[n] == "O") DF$mass[n] <- 16L
		if (DF$element[n] == "Br") DF$mass[n] <- 79L
		if (DF$element[n] == "Cl") DF$mass[n] <- 35L
		}

	M <- 0L	# compute MW
	for (n in 1:nrow(DF)) M <- M + (DF$mass[n] * DF$count[n])
	Mi <- 100 # trivial peak intensity for one peak
	rn <- c("M")
	# For any isotopes of Br, Cl, we need to figure M + 2 etc

	if (any(DF$element == "Br" | DF$element == "Cl")) {
		Br <- Cl <- nBr <- nCl <- 0
		sam.size <- 1e4
		nBr <- DF$count[grep("Br", DF$element)]
		nCl <- DF$count[grep("Cl", DF$element)]
		if (!length(nCl) == 0) Cl <- rbinom(sam.size, size = nCl, prob = 0.25)
		if (!length(nBr) == 0) Br <- rbinom(sam.size, size = nBr, prob = 0.48)
		BrCl <- Br + Cl
		hal.tab <- table(BrCl)
		no.pks <- length(hal.tab)
		M <- seq(from = M, to = (M + ((no.pks-1)*2)), by = 2L)
		Mi <- as.data.frame(hal.tab)
		Mi <- Mi$Freq
		l <- length(Mi)
		i <- seq(2, (l*2)-2, by = 2)
		rn <- c(rn, paste("M", i, sep = "+"))
		}

	Mi <- round(Mi*100/max(Mi))
	MS <- data.frame(mass = M, rel.int = Mi, row.names = rn)
		
	p <- qplot(x = M, y = Mi, data = MS, geom = "segment",
		yend = 0, xend = M, size = I(1), ylab = ylab, xlab = xlab,
		main = main, ...)
		
	print(p)
	return(MS)
	}