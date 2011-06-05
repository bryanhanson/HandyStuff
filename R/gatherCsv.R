gatherCsv <-
function() {

	files <- list.files(pattern = ".csv|.CSV")
	names <- substr(basename(files), 1, nchar(basename(files)) - 4)
	wave <- read.csv(files[1], header = FALSE)
	df <- wave

	for (i in 2:length(files)) {
		temp <- read.csv(files[i], header = FALSE)
		temp <- temp[,2]
		df <- data.frame(df, temp)
		}

	colnames(df) <- c("Wavelength", names)
	write.csv(df, "All Specs.csv", row.names = FALSE)
	}

