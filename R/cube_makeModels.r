
calculatePCA <- function(dataset, md, ap) {
	if (is.null(ap$pca)) {
		return(NULL)
	}
	if (!.ap2$stn$allSilent) {cat("      calc. PCA...")}

	if (!.ap2$stn$allSilent) {cat(" ok\n")}
	return(NULL)
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


## next move: clean out defs at aquagram
## at ap_checExistence: check for the numbers, all the aquagram things
## add check if NOT modells are calculated at all - then appropriate message in line 351 at generate datasets
