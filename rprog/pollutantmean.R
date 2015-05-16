
getfield <- function(data, field)
{
	data[field][!is.na(data[field])]
}

meanforfile <- function(directory, id, pollutant) {
	file <- paste(directory, "/", id, sep="")
	data <- read.csv(file)
	vec <- getfield(data, pollutant)
	vec
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
	files <- dir(directory)[id]
	means <- unlist(lapply(files, function(f) {meanforfile(directory, f, pollutant)}), use.names=F)
	mean(c(means))
}