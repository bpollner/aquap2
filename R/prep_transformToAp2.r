
#' @title Transform into aquap2 structure
#' @description Transform a given object into the aquap2 data structure.
#' @details Class and Numeric prefixes in the column names are expected
#' @param obj The object to be converted
#' @param expName An experiment name
#' @param ncpwl The numbers of characters before the wavelength.
#' @param reduceTRH Logical. If Temp and RelHum values should be reduced.
#' @param md The metadata; an object of class 'aquap_md'.
#' @return An object of class 'aquap_data'
#' @family Development Functions
#' @family Integration Functions
#' @export
ttap <- function(obj,  expName="", ncpwl=1, reduceTRH=FALSE, md=getmd()) {
	autoUpS(cfs=FALSE)
	#
	md$meta$expName <- expName
	#
	header <- obj$header
	class(header) <- "data.frame"
	header <- copyYColsAsClass(header)
	if (reduceTRH) {
		header <- remakeTRHClasses_sys(header)
	}
	colRep <- extractClassesForColRep(header)		## the color representation of the factors
	NIR <- obj$NIR
	fd <- data.frame(I(header), I(colRep), I(NIR))
	fullData <- new("aquap_data", fd)
	fullData@version <- pv_versionDataset # get from the constants -- change at the constants only if the structure of the dataset has changed !!! XXX
	fullData@metadata <- md
	fullData@anproc <- NULL # the ap not yet here of course
	fullData@ncpwl <- ncpwl
	return(fullData)
} # EOF
