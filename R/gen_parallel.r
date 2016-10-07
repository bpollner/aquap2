checkHaveParallel <- function() {
	if (is.null(getDoParName()) ) { return(FALSE) }
	if (getDoParName() == "doSEQ") { return(FALSE) }
	return(TRUE)
} # EOF

checkNrOfWorkers <- function(allowNA=TRUE, logic=FALSE) {
	nr <- parallel::detectCores(all.tests=FALSE, logical=logic)
	if (is.na(nr)) {
		if (allowNA) {
			return(nr)
		} else {
			return(1)
		}
	} else {
		return(nr)
	}
} # EOF

getDesiredNrCPUs <- function(allowNA=TRUE) {
	nrWorkers <- .ap2$stn$gen_numberOfCPUs
	if (is.na(nrWorkers)) {
		nrWorkers <- checkNrOfWorkers(allowNA)
	}
	return(nrWorkers)
} # EOF

registerParallelBackend <- function() {
	nrWorkers <- getDesiredNrCPUs(allowNA=TRUE)
	haveParallel <- checkHaveParallel()
	if (!haveParallel) {
		if (is.na(nrWorkers)) { # could still be NA if automatic detection could not find anything
			registerDoParallel()
		} else {
			if (is.numeric(nrWorkers) & (length(nrWorkers) == 1)) {
				registerDoParallel(nrWorkers)		
			} else {
				stop("Please provide a length one numeric as the number of worker processes in the settings file. Thank you very much.", call.=FALSE)
			}
		}
	}
} # EOF

