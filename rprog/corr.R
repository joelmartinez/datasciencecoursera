corr <- function(directory, threshold = 0) {
	set <- complete(directory)
	ids <- set[set$nobs >= threshold,c("id")]

	

	files <- dir(directory)[ids]
	rows <- lapply(files, function(fname){

		file <- paste(directory, "/", fname, sep="")
		frame <- read.csv(file)
		allcomplete <- frame[complete.cases(frame),]
		cor(allcomplete$nitrate, allcomplete$sulfate)
		
	})
	
	unlist(rows[!is.na(rows)], use.names=F)
}