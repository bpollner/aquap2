checkHaveParallel <- function() {
	if (is.null(getDoParName()) ) { return(FALSE) }
	if (getDoParName() == "doSEQ") { return(FALSE) }
	return(TRUE)
} # EOF

registerParallelBackend <- function() {
	nrWorkers <- .ap2$stn$gen_numberOfCPUs
	haveParallel <- checkHaveParallel()
	if (!haveParallel) {
		if (is.na(nrWorkers)) {
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
