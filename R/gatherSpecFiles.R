gatherSpecFiles <-
function(type = "txt", intLambda = FALSE, ...) {

	message("Are you working on a copy?")
	answer <- substr(readline("Continue (y/n)?  "), 1L, 1L)
		if (answer == "n" | answer == "N") {
			cat("cancelled by user\n")
			return(invisible())
			}
	if (answer == "y" | answer == "Y") cat("Onward then!\n")

	if (type == "csv") message("Files are being overwritten...")
	
	if (type == "cmbl") {
		files <- list.files(pattern = "\\.(cmbl|CMBL)")
		files.noext <- substr(basename(files), 1, nchar(basename(files)) - 4)
		out.files <- paste(files.noext, "csv", sep = "")
	
		for (i in 1:length(files)) {
			cmbl2csv(in.file = files[i], out.file = out.files[i])
			}
		}
		
	if (type == "txt") {
		files <- list.files(pattern = "\\.(txt|TXT)")
		files.noext <- substr(basename(files), 1, nchar(basename(files)) - 3)
		out.files <- paste(files.noext, "csv", sep = "")
	
		for (i in 1:length(files)) {
			txt2csv(in.file = files[i], out.file = out.files[i])
			}
		}

	if (type == "csv") { # read 'em and remove header
		files <- list.files(pattern = "\\.(csv|CSV)")
	
		for (i in 1:length(files)) {
			df <- read.csv(files[i])
			write.table(df, file = files[i], row.names = FALSE,
				col.names = FALSE, quote = FALSE, sep = ",")
			}
		}

	if (type == "sstab") {
		files <- list.files(pattern = "\\.(txt|TXT)")
		files.noext <- substr(basename(files), 1, nchar(basename(files)) - 3)
		out.files <- paste(files.noext, "csv", sep = "")
	
		for (i in 1:length(files)) {
			sstab2csv(in.file = files[i], out.file = out.files[i])
			}
		}
	
	if (intLambda) avgLambda() # aggregate wavelengths into whole numbers and mean abs
	
	df <- gatherCsv(...) # combine the many into one
	
	}

