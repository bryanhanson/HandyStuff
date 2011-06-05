avgLambda <-
function() {

	files <- list.files(pattern = ".csv|.CSV")
	for (i in 1:length(files)) {
		temp <- read.csv(files[i], header = FALSE)
		#cat("Ready to round wavelengths ", files[i], "\n")
		temp[,1] <- round(temp[,1])
		#cat("Ready to average ", files[i], "\n")
		temp <- aggregate(data = temp, V2~V1, mean, na.action = na.pass) # what a time saver!
		write.table(temp, file = files[i], row.names = FALSE, col.names = FALSE, quote = FALSE, sep = ",")
		}
	}

