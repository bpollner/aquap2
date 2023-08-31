
#' @title Transform into aquap2 structure
#' @description Transform a given object into the aquap2 data structure.
#' @details Class and Numeric prefixes in the column names are expected. 
#' Function \code{ttapM} is a reduced function for internal use only.
#' @param obj The object to be converted
#' @param md The metadata; an object of class 'aquap_md'. expName can be
#' changed.
#' @param dol Logical. If outliers should be detected.
#' @param ncpwl The numbers of characters before the wavelength.
#' @param reduceTRH Logical. If Temp and RelHum values should be reduced.
#' @param stf Logical, if the resulting file should be saved in the r-data folder.
#' @return An object of class 'aquap_data'
#' @family Integration Functions
#' @export
ttap <- function(obj, md=getmd(), dol=TRUE, ncpwl=1, reduceTRH=FALSE, stf=TRUE) {
	autoUpS(cfs=TRUE)
	#
	objName <- deparse(substitute(obj))
	if (!.ap2$stn$allSilent) { cat(paste0("Transforming ", nrow(obj), " scans in '", objName, "' to aquap2 structure... \n")) }
	header <- obj$header
	class(header) <- "data.frame"
	outliers <- flagOutliers_allScope(obj$NIR, detectOutliers=dol)
	header <- cbind(header, outliers)
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
	if (stf) {
		saveAQdata(fullData, md)
	}
#	if (!.ap2$stn$allSilent) { cat("Done.\n")}
	return(fullData)
} # EOF

#' @rdname ttap
#' @export
ttapM <- function(obj) {
#	autoUpS(cfs=FALSE)
	header <- obj$header
	class(header) <- "data.frame"
	colRep <- data.frame(C_all=rep(1, nrow(header)), C_ECRM=rep(1, nrow(header))) # static !!!
	NIR <- obj$NIR
	rownames(header) <- rownames(colRep) <- rownames(NIR) <- paste0("S", 1:nrow(header))
	fd <- data.frame(I(header), I(colRep), I(NIR))
	fullData <- new("aquap_data", fd)
	fullData@version <- pv_versionDataset # get from the constants -- change at the constants only if the structure of the dataset has changed !!! XXX
	fullData@metadata <- list(NULL)
	fullData@anproc <- NULL # the ap not yet here of course
	fullData@ncpwl <- 1
	return(fullData)	
} # EOF
