checkHaveParallel <- function() {
	if (is.null(foreach::getDoParName()) ) { return(FALSE) }
	if (foreach::getDoParName() == "doSEQ") { return(FALSE) }
	return(TRUE)
} # EOF

checkNrOfWorkers <- function(allowNA=TRUE, logic=getstn()$gen_useVirtualCores) {
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
	nrWorkers <- getstn()$gen_numberOfCPUs
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
			doParallel::registerDoParallel()
		} else {
			if (is.numeric(nrWorkers) & (length(nrWorkers) == 1)) {
				doParallel::registerDoParallel(nrWorkers)		
			} else {
				stop("Please provide a length one numeric as the number of worker processes in the settings file. Thank you very much.", call.=FALSE)
			}
		}
	}
} # EOF
