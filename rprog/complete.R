
col <- function(rows, colindex) {
	unlist(lapply(rows, function(r) {
		r[colindex]
	}), use.names=F)
}

complete <- function(directory, id = 1:332) {
	files <- dir(directory)
	rows <- lapply(id, function(i){
		fname <- files[i]
		file <- paste(directory, "/", fname, sep="")
		frame <- read.csv(file)
		allcomplete <- frame[complete.cases(frame),]
		c(i, nrow(allcomplete))
	})
	id <- col(rows,1)
	nobs <- col(rows,2)
	data.frame(id, nobs)
}