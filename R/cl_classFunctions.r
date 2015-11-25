show_aquap_data <- function(object) {
	cns <- colnames(object$NIR)
	nr <- nrow(object$header)
	nNIRcols <- nnc <- ncol(object$NIR)
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
	print(object$header[rs,])
	if (.ap2$stn$gen_showData_NIR) {
		print(object$NIR[rs,cs])
	}
} # EOF

showCube <- function(object) {
	stats <- checkForStats(object@anproc)  ## list(cnt=cnt, char=char)
	if (stats$cnt == 0) {
		add <- "and no models."
	} else {
		ModNames <- paste(stats$char, collapse=", ")
		if (is.logical(object@anproc$aquagr$spectra)) { # so it is FALSE
			specTxt <- ""
		} else {
			specTxt <- "+spectra"
		}
		txt <- sub("Aquagram", paste("Aquagram", specTxt," [", object@anproc$aquagr$mod, "]", sep=""), ModNames)
		add <- paste("and ", stats$cnt, " models (", txt, ") in each set.", sep="")
	}
	cat(paste("Formal class 'aquap_cube', containing ", object@cpt@len, " datasets in total ", add, "\n", sep=""))
	cat("\n")
	print(object@cp)
} # EOF

plot_cube_M <- function(x, ...) {
  plot_cube(x, ...)
} # EOF

plot_spectra_M <- function(x, ...) {
	plot_spectra(x, ...)
} # EOF

