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

plot_spectra_Data_M <- function(x, colorBy=NULL, ...) {
	plot_spectra_Data(x, colorBy, ...)
} # EOF

plot_spectra_Cube_M <- function(x, colorBy=NULL, ...) {
	plot_spectra_Cube(x, colorBy, ...)
} # EOF

getWavelengths_dataset <- function(object) { # object is a dataset
	a <- colnames(object$NIR)
	ncpwl <- getNcpwl(object)
	wls <- as.numeric(substr(a, 1+ncpwl, nchar(a)))
	return(wls)	
} # EOF

getWavelengths_set <- function(object) { # object is a set
	return(getWavelengths_dataset(getDataset(object)))
} # EOF

getNIR_df_dataset <- function(object) { # object is a dataset
	NIR <- as.data.frame(matrix(object$NIR, ncol=ncol(object$NIR)))
	colnames(NIR) <- colnames(object$NIR)
	rownames(NIR) <- rownames(object$NIR)
	return(NIR)
} # EOF

getNIR_df_set <- function(object) { # object is a set
	return(getNIR_df_dataset(getDataset(object)))
} # EOF

getHeader_dataset <- function(object) { # object is a dataset
	hd <- object$header
	class(hd) <- "data.frame" # to get rid of the "AsIs" component
	return(hd)
} # EOF

getHeader_set <- function(object) { # object is a set
	return(getHeader(getDataset(object)))
} # EOF

getColRep_data <- function(object) { # object is a dataset
	cr <- object$colRep
	class(cr) <- "data.frame" # to get rid of the "AsIs" component
	return(cr)
}# EOF

getColRep_set <- function(object) { # object is a set
	return(getColRep(getDataset(object)))
}# EOF
