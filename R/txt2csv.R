txt2csv <-
function(in.file = "", out.file = "") {
	
	df <- read.table(in.file, skip = 7, sep = "\t")
	write.table(df, file = out.file, row.names = FALSE,
		col.names = FALSE, quote = FALSE, sep = ",")

	}

