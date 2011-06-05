gatherSpecFiles <-
function(type = "txt", intLambda = FALSE, ...) {

# get the files, convert to clean csv

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
	
	if (intLambda) avgLambda() # aggregate wavelengths into whole numbers and mean abs
	
	gatherCsv(...) # combine the many into one
	
	}

