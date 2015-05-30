
## Helper Functions

toColumnName <- function(x, prefix="") {
    s <- strsplit(x, " ")[[1]]
    val <- gsub(" ", ".", paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" "))
    paste(prefix, val, sep="")
}
outcomesForState <- function(data, state, colName) {
	dataForState <- data[data[["State"]] == state, c("Hospital.Name", colName)]
	dataForState[2] <- as.numeric(unlist(dataForState[2]))
	u <- complete.cases(dataForState)
	dataForState[u,]
}

best <- function(state, outcome) { 
	## Read outcome data 
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	## Check that state and outcome are valid 
	colName <- toColumnName(outcome, "Hospital.30.Day.Death..Mortality..Rates.from.")
	states <- factor(data[["State"]])
	stateValid <- state %in% levels(states)
	outcomeValid <- colName %in% names(data)
	if (stateValid && outcomeValid) {
		## Return hospital name in that state with lowest 30-day death 
		## rate 
		statedata <- outcomesForState(data, state, colName)		
		statemin <- min(statedata[2])
		bestCandidates <- statedata[statedata[2] <= statemin, 1]
		## now sort the candidates by alpha, and grab the first one
		head(bestCandidates[order(bestCandidates)],1)
	}
	else {
		if (!stateValid) stop("invalid state")
		if (!outcomeValid) stop("invalid outcome")
	}
}