show_aquap_data <- function(object) {
	autoUpS(cfs=FALSE)
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
	len <-  object@cpt@len
	if (len > 1) {addPlural <- "s"} else {addPlural <- ""}	
	if (stats$cnt > 1) {addModelsPlural <- "s"} else {addModelsPlural <- ""}
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
		add <- paste("and ", stats$cnt, " model", addModelsPlural ," (", txt, ") in each [or some] sets.", sep="")
	}
	cat(paste("Formal class 'aquap_cube', containing ", len, " dataset", addPlural, " in total ", add, "\n", sep=""))
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
	print(out) ## here the printing !!!! ####
	ap <- getAnproc(object)
	a <- ap$dpt$dptModules
	preMsg <- preChar <- postMsg <- postChar <- lineBreak <- NULL
	if (!is.null(a$dptPre) | !is.null(a$dptPost)) {
		if (!is.null(a$dptPre)) {
			preMsg <- "Data pre-treatment (dpt.pre): "
			preChar <- paste(a$dptPre, collapse=", ", sep="")
			lineBreak <- "\n"
		}
		if (!is.null(a$dptPost)) {
			postMsg <- "Data post-treatment (dpt.post): "
			postChar <- paste(a$dptPost, collapse=", ", sep="")
			lineBreak <- "\n"		
		}
		cat(paste(preMsg, preChar, lineBreak, sep=""))
		cat(paste(postMsg, postChar, lineBreak, sep=""))
	}
#	return(invisible(out)
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

subtract_two_aquap_data_M <- function(e1, e2) { # e1 and e2 being each an object of class aquap_data
	if (nrow(e1) != 1 & nrow(e2) != 1) {
		if (nrow(e1) != nrow(e2)) {
			stop("The provided datasets do not have the same number of rows.\nFor successful subtraction via '-', both datasets have to have the same number of rows, or one dataset has to have exactly one (1) row.", call.=FALSE)
		}
		if (ncol(e1$NIR) != ncol(e2$NIR)) {
			if (!.ap2$stn$gen_calc_allowSubtrDiffWavels) {
				stop("The provided datasets do not have the same number of wavelengths.\nFor successful subtraction via '-', both datasets have to have the same number of wavelengths, i.e. the same number of columns in their NIR data.", call.=FALSE)
			} else { # so we do want to allow the subtractions between datasets that have possibly been touched by do_gapDer
				cns1 <- colnames(e1$NIR)
				cns2 <- colnames(e2$NIR)
				if (length(cns1) > length(cns2)) {
					longer <- cns1
					shorter <- cns2
					oneIsLonger <- TRUE
				} else {
					longer <- cns2
					shorter <- cns1
					oneIsLonger <- FALSE
				}
				ind <- which(longer %in% shorter) # retrieve the indices of those wavelengths that are in both the datasets
				if (oneIsLonger) {
					e1$NIR <- e1$NIR[, ind]
				} else {
					e2$NIR <- e2$NIR[, ind]
				}
			} # end else allow for subtraction of different number of wavelengths
		} # end check the same nr of columns in the NIR
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
		allFull$NIR <- I(NIR)
		return(allFull)
	} # end one has only one (1) row
	stop("An error has occured at the subtraction of datasets, sorry.", call.=FALSE)
} # EOF

divide_two_aquap_data_M <- function(e1, e2) { # e1 and e2 being each an object of class aquap_data
	if (nrow(e1) != nrow(e2)) {
		stop("The provided datasets do not have the same number of rows.\nFor successful division via '/', both datasets have to have the same number of rows.", call.=FALSE)
	}
	if (ncol(e2$NIR) != 1) {
		stop("For successful division via '/', the second dataset must have only one column in the NIR-data, i.e. contain only a single wavelength.", call.=FALSE)
	}
	e1$NIR <- sweep(e1$NIR, 1, e2$NIR[,1], "/") ### CORE ###
	return(e1)
} #EOF

plot_pca_cube_M <- function(object, ...) {
	plot_pca_cube(cube=object, ...)
} # EOF

plot_pca_data_M <- function(object, ...) {
	plot_pca_data(dataset=object, ...)
} # EOF

plot_pls_cube_M <- function(object, ...) {
	plot_pls_cube(cube=object, ...)
} # EOF

plot_simca_cube_M <- function(object, ...) {
	plot_simca_cube(cube=object, ...)
} # EOF


# classification -----------
plot_da_cube_M <- function(object, ...) {
	plot_da_cube(cube=object, ...)
} # EOF

plot_rnf_cube_M <- function(object, ...) {
	plot_rnf_cube(cube=object, ...)
} # EOF

plot_svm_cube_M <- function(object, ...) {
	plot_svm_cube(cube=object, ...)
} # EOF

plot_nnet_cube_M <- function(object, ...) {
	plot_nnet_cube(cube=object, ...)
} # EOF


# merge datasets ------------
mergeDatasets_two_noLabels_M <- function(ds1, ds2, mergeLabels=NULL, noMatch="ask", dol=TRUE) {
#	print("mergeDatasets_two_noLabels_M"); wait()
	mergeDatasets_two(ds1, ds2, mergeLabels=NULL, noMatch, dol)
} # EOF

mergeDatasets_two_mergeLabels_M <- function(ds1, ds2, mergeLabels, noMatch="ask", dol=TRUE) {
#	print("mergeDatasets_two_mergeLabels_M"); wait()
	mergeDatasets_two(ds1, ds2, mergeLabels, noMatch, dol)
} # EOF

mergeDatasets_list_noLabels_M <- function(ds1, ds2=NULL, mergeLabels=NULL, noMatch="ask",  dol=TRUE) {
#	print("mergeDatasets_list_noLabels_M"); wait()
	mergeDatasets_list(dsList=ds1, mergeLabels=NULL, noMatch, dol)
} # EOF

mergeDatasets_list_mergeLabels_M <- function(ds1, ds2=NULL, mergeLabels, noMatch="ask", dol=TRUE) {
#	print("mergeDatasets_list_mergeLabels_M"); wait()
	mergeDatasets_list(dsList=ds1, mergeLabels, noMatch, dol)
} # EOF

mergeDatasets_list_mergeLabels_2_M <- function(ds1, ds2, mergeLabels=NULL, noMatch="ask", dol=TRUE) {
#	print("mergeDatasets_list_mergeLabels_2_M"); wait()
	mergeDatasets_list(dsList=ds1, mergeLabels=ds2, noMatch, dol)
} # EOF

generateMergeLabels_twoDatasets_M <- function(ds1, ds2, varNames, varTypes, values=NULL) {
#	print("generateMergeLabels_twoDatasets_M"); wait()
	generateMergeLabels_sys(ds1=ds1, ds2=ds2, varNames, varTypes, values)
} # EOF

generateMergeLabels_list_M <- function(ds1, ds2, varNames, varTypes, values=NULL) {
#	print("generateMergeLabels_list_M"); wait()
	generateMergeLabels_sys(ds1=ds1, ds2=NULL, varNames, varTypes, values)
} # EOF

generateMergeLabels_list_2_M <- function(ds1, ds2=NULL, varNames, varTypes, values=NULL) {
#	print("generateMergeLabels_list_2_M"); wait()  # list, character, character, list
	generateMergeLabels_sys(ds1=ds1, ds2=NULL, varNames=ds2, varTypes=varNames, values=varTypes)
} # EOF

generateMergeLabels_list_3_M <- function(ds1, ds2=NULL, varNames, varTypes, values=NULL) {
#	print("generateMergeLabels_list_2_M"); wait() # list, character, character, missing
	generateMergeLabels_sys(ds1=ds1, ds2=NULL, varNames=ds2, varTypes=varNames, values=NULL)
} # EOF

showMergeLabels <- function(object) {
	cat(paste0("An object of class 'aquap_mergeLabels', containing ", length(object@varNames), " new Labels for ", length(object@numVec), " datasets to be merged.\n\n"))
	print(object)
	return(invisible(NULL))
} # EOF

check_sub_input <- function(Data, x) {
# now go check if the input was ok
	for (i in 1: length(x@varTypes)) {
		if (x@varTypes[i] == "c") {
		#	if (!all(is.character(Data[,i]) )) {stop(paste0("Please provide only characters for the variable named '", x@varNames[i], "')"), call.=FALSE)}  	# no, do not check this here. It could be that someone wants strings consisting of numbers ("8484"). That should stay possible.
		} else { # so varType[i] must be "n"
			if ( !any(is.na(Data[,i])) & !all(is.numeric(Data[,i])) ) {stop(paste0("Please provide only numerics for the variable named '", x@varNames[i], "')"), call.=FALSE)}
		} # end else
	} # end for i	
} # EOF
