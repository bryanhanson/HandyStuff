compareCats <-
function(data = NULL, res = NULL,
	fac1 = NULL, fac2 = NULL,
	fac1cols = NULL,
	method = c("box", "points"), ...)
	{

	if (is.null(fac1cols)) stop("You need to supply fac1cols")
	
	# 1st, reduce the df and clean it of NAs
	# otherwise there is trouble with the counts
	
	if (!identical(fac2, NULL)) {
		keep <- c(res, fac1, fac2)
		} else {
			keep <- c(res, fac1)
			}
	data <- data[, keep]
	data <- na.omit(data)

	# Specify the data & aesthetics
		
	if (identical(fac2, NULL)) { 
		p <- ggplot(data, aes_string(x = fac1, y = res, color = fac1))
		
		} else {
			p <- ggplot(data, aes_string(x = fac1, y = res, color = fac1)) +
			facet_grid(paste(". ~", fac2, sep = ""))
			}
				
	# now add layers as requested
	
	if (method == "points") {
		jit <- position_jitter(width = 0.05, height = 0.0)
		p <- p + layer(geom = "jitter", position = jit)		}
		
	if (method == "box") {
		p <- p + geom_boxplot(width = 0.2, outlier.shape = 2)
		}
	
	invisible(p)
	}
