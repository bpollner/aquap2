show_aquap_data <- function(object) {
	cns <- colnames(object@NIR)
	nr <- nrow(object@header)
	nNIRcols <- nnc <- ncol(object@NIR)
	cat("Formal class 'aquap_data' \n")
	cat(paste(nr, " observations in ", nNIRcols, " wavelengths, from ", cns[1], " to ", cns[length(cns)], ".\n(Showing only max. 10 rows.)\n", sep=""))
	if (nr < 10) {
		rs <- 1:nr
	} else {
		rs <- 1:10
	}
	if (nnc < 8) {
		cs <- 1:nnc
	} else {
		cs <- 1:8
	}	
	print(object@header[rs,])
	print(object@NIR[rs,cs])
} # EOF
