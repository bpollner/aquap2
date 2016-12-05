resampleNIR_inner <- function(NIR, targetWls=NULL, method="linear") {
    x <- getWavelengths(NIR)
	if (is.null(targetWls)) {
    	xNew <- seq(ceiling(x[1]/2) * 2, floor(x[length(x)]/2) * 2, 0.5)
	} else {
		xNew <- targetWls
	}
	NIRnew <- t(apply(NIR, 1, pracma::interp1, x = x, xi = xNew, method = method))
    colnames(NIRnew) <- paste0("X", xNew)
	return(NIRnew)
} # EOF

resampleNIR_singleRow <- function(NIRdataRow, oldWls, targetWls, method="linear") {
	NIRnew <- pracma::interp1(x=oldWls, y=NIRdataRow, xi=targetWls, method=method)
	return(NIRnew)
} # EOF

interpWls <- function(trg, src) {

} #EOF
