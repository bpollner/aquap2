plot_da_cube <- function(cube, aps="def", ...) {
	autoUpS()
	printEmpty <- .ap2$stn$gen_plot_printEmptySlots
	#
	ap <- doApsTrick(aps, cube, ...)
	ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset=getDataset(cube[[1]]), haveExc=FALSE) # just take the first dataset, as we mainly need the header (and the wavelengths are already checked.. )
	apCu <- getAnproc(cube)
	if (is.null(ap$classif$da)) {
		if (printEmpty) {
			cat("*** DA model not available or not selected for plotting \n")
		}
		return(NULL)
	}
	plot_classif_generalHandover(cube, ap, slotChar="xda", apCu$classif$da, ap$classif$da)
} # EOF

plot_rnf_cube <- function(cube, aps="def", ...) {
	autoUpS()
	printEmpty <- .ap2$stn$gen_plot_printEmptySlots
	#	
	ap <- doApsTrick(aps, cube, ...)
	ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset=getDataset(cube[[1]]), haveExc=FALSE) # just take the first dataset, as we mainly need the header (and the wavelengths are already checked.. )
	apCu <- getAnproc(cube)
	if (is.null(ap$classif$rnf)) {
		if (printEmpty) {
			cat("*** Random forest model not available or not selected for plotting \n")
		}
		return(NULL)
	}
	plot_classif_generalHandover(cube, ap, slotChar="rnf", apCu$classif$rnf, ap$classif$rnf)	
} # EOF

plot_svm_cube <- function(cube, aps="def", ...) {
	autoUpS()
	printEmpty <- .ap2$stn$gen_plot_printEmptySlots
	#	
	ap <- doApsTrick(aps, cube, ...)
	ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset=getDataset(cube[[1]]), haveExc=FALSE) # just take the first dataset, as we mainly need the header (and the wavelengths are already checked.. )
	apCu <- getAnproc(cube)
	if (is.null(ap$classif$svm)) {
		if (printEmpty) {
			cat("*** SVM model not available or not selected for plotting \n")
		}
		return(NULL)
	}
	plot_classif_generalHandover(cube, ap, slotChar="svm", apCu$classif$svm, ap$classif$svm)
} # EOF

plot_nnet_cube <- function(cube, aps="def", ...) {
	autoUpS()
	printEmpty <- .ap2$stn$gen_plot_printEmptySlots
	#	
	ap <- doApsTrick(aps, cube, ...)
	ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset=getDataset(cube[[1]]), haveExc=FALSE) # just take the first dataset, as we mainly need the header (and the wavelengths are already checked.. )
	apCu <- getAnproc(cube)
	if (is.null(ap$classif$nnet)) {
		if (printEmpty) {
			cat("*** NNET model not available or not selected for plotting \n")
		}
		return(NULL)
	}
	plot_classif_generalHandover(cube, ap, slotChar="ann", apCu$classif$nnet, ap$classif$nnet)
} # EOF


#' @title plot Discriminant Analysis
#' @description Plot all available models for discriminant analysis 
#' classification.
#' @param object An object of class "aquap_cube" (as produced by 
#' \code{\link{gdmm}}).
#' @param ... Optional \code{da.} plotting parameters to override the values in 
#' the analysis procedure - for possible arguments see 
#' \code{\link{plot_discrimAnalysis_args}} and here below: 
#' \describe{
#' \item{aps}{Character length one. The default way to obtain the analysis 
#' procedure. Defaults to "def". Possible values are:
#' \describe{
#' \item{"def"}{The default from the settings.r file is taken. (Argument 
#' \code{gen_plot_anprocSource})}
#' \item{"cube"}{Take the analysis procedure from within the cube, i.e. the 
#' analysis procedure that was used when creating the cube via \code{\link{gdmm}}
#' is used.}
#' \item{"defFile"}{Use the analysis procedure with the default filename as 
#' specified in the settings.r file in \code{fn_anProcDefFile}.}
#' \item{Custom filename}{Provide any valid filename for an analysis procedure to 
#' use as input for specifying the plotting options.}
#' }} 
#' } 
#' @family Plot functions
#' @family DA documentation
#' @name plot_da
NULL

#' @title plot Random Forest
#' @description Plot all available models for Random Forest 
#' classification.
#' @param object An object of class "aquap_cube" (as produced by 
#' \code{\link{gdmm}}).
#' @param ... Optional \code{rnf.} plotting parameters to override the values in 
#' the analysis procedure - for possible arguments see 
#' \code{\link{plot_randomForest_args}} and here below: 
#' \describe{
#' \item{aps}{Character length one. The default way to obtain the analysis 
#' procedure. Defaults to "def". Possible values are:
#' \describe{
#' \item{"def"}{The default from the settings.r file is taken. (Argument 
#' \code{gen_plot_anprocSource})}
#' \item{"cube"}{Take the analysis procedure from within the cube, i.e. the 
#' analysis procedure that was used when creating the cube via \code{\link{gdmm}}
#' is used.}
#' \item{"defFile"}{Use the analysis procedure with the default filename as 
#' specified in the settings.r file in \code{fn_anProcDefFile}.}
#' \item{Custom filename}{Provide any valid filename for an analysis procedure to 
#' use as input for specifying the plotting options.}
#' }} 
#' } 
#' @family Plot functions
#' @family RNF documentation
#' @name plot_rnf
NULL

#' @title plot Discriminant Analysis
#' @description Plot all available models for discriminant analysis 
#' classification
#' @param object An object of class "aquap_cube" (as produced by 
#' \code{\link{gdmm}}).
#' @param ... Optional \code{svm.} plotting parameters to override the values in 
#' the analysis procedure - for possible arguments see 
#' \code{\link{plot_SVM_args}} and here below: 
#' \describe{
#' \item{aps}{Character length one. The default way to obtain the analysis 
#' procedure. Defaults to "def". Possible values are:
#' \describe{
#' \item{"def"}{The default from the settings.r file is taken. (Argument 
#' \code{gen_plot_anprocSource})}
#' \item{"cube"}{Take the analysis procedure from within the cube, i.e. the 
#' analysis procedure that was used when creating the cube via \code{\link{gdmm}}
#' is used.}
#' \item{"defFile"}{Use the analysis procedure with the default filename as 
#' specified in the settings.r file in \code{fn_anProcDefFile}.}
#' \item{Custom filename}{Provide any valid filename for an analysis procedure to 
#' use as input for specifying the plotting options.}
#' }} 
#' } 
#' @family Plot functions
#' @family SVM documentation
#' @name plot_svm
NULL

#' @title plot Discriminant Analysis
#' @description Plot all available models for discriminant analysis 
#' classification
#' @param object An object of class "aquap_cube" (as produced by 
#' \code{\link{gdmm}}).
#' @param ... Optional \code{nnet.} plotting parameters to override the values in 
#' the analysis procedure - for possible arguments see 
#' \code{\link{plot_NNET_args}} and here below: 
#' \describe{
#' \item{aps}{Character length one. The default way to obtain the analysis 
#' procedure. Defaults to "def". Possible values are:
#' \describe{
#' \item{"def"}{The default from the settings.r file is taken. (Argument 
#' \code{gen_plot_anprocSource})}
#' \item{"cube"}{Take the analysis procedure from within the cube, i.e. the 
#' analysis procedure that was used when creating the cube via \code{\link{gdmm}}
#' is used.}
#' \item{"defFile"}{Use the analysis procedure with the default filename as 
#' specified in the settings.r file in \code{fn_anProcDefFile}.}
#' \item{Custom filename}{Provide any valid filename for an analysis procedure to 
#' use as input for specifying the plotting options.}
#' }} 
#' } 
#' @family Plot functions
#' @family NNET documentation
#' @name plot_nnet
NULL

