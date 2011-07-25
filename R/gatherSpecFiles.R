gatherSpecFiles <-
function(type = "txt", intLambda = FALSE, ...) {

	cat("Are you working on a copy?\n")
	cat("Files are being overwritten...\n")
	
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
	
	if (intLambda) avgLambda() # aggregate wavelengths into whole numbers and mean abs
	
	df <- gatherCsv(...) # combine the many into one
	
	}

