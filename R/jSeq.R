
	
# Function to create sequences of a given type centered on zero
# These correspond to the spacing of odd and even NMR multiplets
# Answer is in terms of multiples of J

jSeq <- function(length.out) {
	if (length.out%%2 == 1) {
		zs <- 0L
		for (n in 1:10) {
			if (length(zs) == length.out) break
			zs <- c(-n, zs, n)
			}
		}

	if (length.out%%2 == 0) {
		zs <- c()
		for (n in seq(0.5, 10.5, by = 1)) {
			if (length(zs) == length.out) break
			zs <- c(-n, zs, n)
			}
		}
		
	ans <- zs
	}	

