
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

calculateAquagram <- function(dataset, md, ap) {
	if (is.null(ap$aquagr)) {
		return(NULL)
	}
	if (!.ap2$stn$allSilent) {cat("      calc. Aquagrams...")}

	if (!.ap2$stn$allSilent) {cat(" ok\n")}
	return(NULL)
} # EOF



# works on a single element of the list in the cube
makeAllModels <- function(set, md, ap) {
#	dataset <- set@dataset
	newSet <- new("aquap_set")
	newSet@pca <- calculatePCA(set@dataset, md, ap)
	newSet@plsr <- calculatePLSR(set@dataset, md, ap)
	newSet@simca <- calculateSIMCA(set@dataset, md, ap)
	newSet@aquagr <- calculateAquagram(set@dataset, md, ap)
	newSet@dataset <- set@dataset
	newSet@idString <- set@idString
	return(newSet)
} # EOF
