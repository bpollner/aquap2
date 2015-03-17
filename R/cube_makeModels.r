
calculatePCA <- function(dataset, md, ap) {
	if (is.null(ap$pca)) {
		return(NULL)
	}
	if (!.ap2$stn$allSilent) {cat("      calc. PCA...")}
	pcaModel <- ChemometricsWithR::PCA(scale(dataset$NIR, scale=FALSE))
	if (!.ap2$stn$allSilent) {cat(" ok\n")}
	return(list(model=pcaModel))
} # EOF


calculatePLSR <- function(dataset, md, ap) {
	if (is.null(ap$plsr)) {
		return(NULL)
	}
	if (!.ap2$stn$allSilent) {cat("      calc. PLSR...")}

	if (!.ap2$stn$allSilent) {cat(" ok\n")}
	return(list(plsr=NULL, plsrPlus=NULL))
} # EOF

calculateSIMCA <- function(dataset, md, ap) {
	if (is.null(ap$simca)) {
		return(NULL)
	}
	if (!.ap2$stn$allSilent) {cat("      calc. SIMCA...")}

	if (!.ap2$stn$allSilent) {cat(" ok\n")}
	return(NULL)
} # EOF

calculateAquagram <- function(dataset, md, ap, idString) {
	if (is.null(ap$aquagr)) {
		return(NULL)
	}
	aq_loadGlobalAquagramCalibData()
	if (is.character(ap$aquagr$spectra)) { message <- "      calc. Aquagrams & spectra... "	} else { message <- "      calc. Aquagrams... "	}
	if (!.ap2$stn$allSilent) {cat(message)}
	ap <- aq_getTCalibRange(ap) 	# checks with the calibration file if the temperature range is ok
	aq_makeGlobals(.ap2$tcd, TCalib=ap$aquagr$TCalib, Texp=ap$aquagr$Texp, ot=getOvertoneCut(.ap2$stn$aqg_OT), smoothN=.ap2$stn$aqg_smoothCalib) ## generate the global variables with TCalib and Texp
	##
	vars <- ap$aquagr$vars
	aquCalcRes  <- list()
	length(aquCalcRes) <- lec <- length(vars)
	if (ap$aquagr$bootCI) {
		registerParallelBackend()  ## will be used in the calculation of confidence intervals
	}
	for (i in 1: length(vars)) {
		aquCalcRes[[i]] <- calcAquagramSingle(dataset, md, ap, vars[i], idString)
	} # end for i
	if (!.ap2$stn$allSilent) {cat(" ok\n")}
	return(aquCalcRes)
} # EOF



# works on a single element of the list in the cube
makeAllModels <- function(set, md, ap) {
#	dataset <- set@dataset
	newSet <- new("aquap_set")
	newSet@pca <- calculatePCA(getDataset(set), md, ap)
	newSet@plsr <- calculatePLSR(getDataset(set), md, ap)
	newSet@simca <- calculateSIMCA(getDataset(set), md, ap)
	newSet@aquagr <- calculateAquagram(getDataset(set), md, ap, getIdString(set))
	newSet@dataset <- getDataset(set)
	newSet@idString <- set@idString
	return(newSet)
} # EOF
