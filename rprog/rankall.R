toColumnName <- function(x, prefix="Hospital.30.Day.Death..Mortality..Rates.from.") {
    s <- strsplit(x, " ")[[1]]
    val <- gsub(" ", ".", paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" "))
    paste(prefix, val, sep="")
}
outcomesForState <- function(data, state, colName) {
	dataForState <- data[data[["State"]] == state, c("Hospital.Name", colName)]
	dataForState[2] <- as.numeric(unlist(dataForState[2]))
	u <- complete.cases(dataForState)
	dataForState[u,]
}

rankall <- function(outcome, num = "best") { 
	## Read outcome data 
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	colName <- toColumnName(outcome)
	
	if (! colName %in% names(data)) stop("invalid outcome")
	
	## Check that state and outcome are valid 
	states <- factor(data[["State"]])
	allstates <- sapply(levels(states), function(state) {
	
		if (! colName %in% names(data)) stop("invalid outcome")
		
		## Return hospital name in that state with the given rank 
		statedata <- outcomesForState(data, state, colName)		
		statedata <- statedata[order(statedata[,2], statedata[,1]),] ## sorted
		
		if (!is.numeric(num)) {
			if (num == "best") num <- 1
			else if (num == "worst") num <- nrow(statedata)
		}
		else {
			num <- as.numeric(num)
		}
		if (num > nrow(statedata)) {
			NA
		}
		else {
			statedata[num,1]
		}
	})

	state <- names(allstates)
	hospital <- allstates
	data.frame(hospital, state)
}