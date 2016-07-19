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
	stats <- checkCubeForRealStats(object)  ## list(cnt=cnt, char=char)
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
		add <- paste("and ", stats$cnt, " models (", txt, ") in each [or some] sets.", sep="")
	}
	cat(paste("Formal class 'aquap_cube', containing ", object@cpt@len, " datasets in total ", add, "\n", sep=""))
	cat("\n")
	cp <- getCP(object) # is a method
	out <- cp
	if (.ap2$stn$gen_showExtendedCube) {
		a <- getCubeNrs(object)
		nrRows <- a$nrRows
		nrWls <- a$nrWls
		extend <- data.frame(nrRows, nrWls)
		colnames(extend) <- c("  #spectra", " #wavelengths")
		out <- cbind(cp, extend)
	} # end if extend
	print(out)
#	return(invisible(out)
} # EOF

plot_cube_M <- function(x, ...) {
  plot_cube(x, ...)
} # EOF

plot_spectra_Data_M <- function(x, ...) {
	plot_spectra_Data(x, ...)
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

subtract_two_aquap_data_M <- function(e1, e2) { # e1 and e1 being each an object of class aquap_data
	if (nrow(e1) != 1 & nrow(e2) != 1) {
		if (nrow(e1) != nrow(e2)) {
			stop("The provided datasets do not have the same number of rows.\nFor successful subtraction via '-', both datasets have to have the same number of rows, or one dataset has to have exactly one (1) row.", call.=FALSE)
		}
		if (ncol(e1$NIR) != ncol(e2$NIR)) {
			stop("The provided datasets do not have the same number of wavelengths.\nFor successful subtraction via '-', both datasets have to have the same number of wavelengths, i.e. the same number of columns in their NIR data.", call.=FALSE)
		}
		if (!identical(colnames(e1$NIR), colnames(e2$NIR))) {
			stop("The provided datasets do not have the same wavelengths.\nFor successful subtraction via '-', in both datasets there have to be the same wavelengths present.", call.=FALSE)
		}
		if (!.ap2$stn$gen_calc_allowSubtrDiffHead) {	 # if the subtraction of datasets having a different header structure should be allowed.
			if (!identical(e1$header, e2$header)) {
 				stop("The provided datasets have a different structure. \nFor successful subraction via '-', both datasets must have the same structure, i.e. the same header.\nYou can change this behaviour in the setting.r file at the parameter 'gen_calc_allowSubtrDiffHead'.", call.=FALSE)
			}
		}
		e1$NIR <- e1$NIR - e2$NIR #### CORE ######
		return(e1)
	} # end if both more than one row
	if (nrow(e1) == 1 | nrow(e2) == 1) {
		if (nrow(e1) == 1 & nrow(e2) == 1) {
			stop("One of the provided datasets must have more than one row for subtraction of a single spectrum from a full dataset.", call.=FALSE)
		} # stop if both are nrow==1
		if (nrow(e1) == 1) {
			nirSingle <- e1$NIR
			nirFull <- e2$NIR
			allFull <- e2
		} # end if nrow(e1)==1
		if (nrow(e2) == 1) {
			nirSingle <- e2$NIR
			nirFull <- e1$NIR
			allFull <- e1
		} # end if nrow(e2)==1
		NIR <- sweep(nirFull, 2, nirSingle) 		#### CORE ##### subtraction is the default in sweep !
		colnames(NIR) <- colnames(nirFull)
		rownames(NIR) <- rownames(nirFull)
		allFull$NIR <- NIR
		return(allFull)
	} # end one has only one (1) row
	stop("An error has occured at the subtraction of datasets, sorry.", call.=FALSE)
} # EOF

plot_pca_cube_M <- function(object, ...) {
	plot_pca_cube(cube=object, ...)
} # EOF

plot_pca_data_M <- function(object, ...) {
	plot_pca_data(dataset=object, ...)
} # EOF
