makePCAScorePlots <- function(cube, ap, comps=c(1:5), pcs=c(1,2), onMain="", onSub="") {
#	plot(0, type="n")
	for (k in 1: length(cube)) {
		set <- cube[[k]]
		PCAObject <- getPCAObject(set)
		classList <- getPCAClassList(ap)
		numRep <- getColRep(set)
		header <- getHeader(set)
		idString <- getIdString(set)
		for (i in 1: length(classList)) {
			colInd <- which(colnames(header)==classList[i])
			indNumRep <- which(colnames(numRep) == classList[i])
			numRepCol <- numRep[,indNumRep]
			mainText <- paste(onMain, idString)
			subText <- paste("color by ", classList[i], " ", onSub, sep="")
			#
			ChemometricsWithR::scoreplot.PCA(PCAObject, c(pcs[1], pcs[2]), col=numRepCol, pch=16, main=mainText, sub=subText)
			legendText <- unique(header[,colInd])
			lto <- order(legendText)
			legend("topright", legend=legendText[lto], col=unique(numRepCol)[lto], pch=16)
			#
			if (!is.null(comps)) {
				pairs(PCAObject$scores[,comps], col=numRepCol, pch=16, main=mainText, sub=subText )
			}
		} # end for i
	} # end for k
} #EOF

plotPCA_Scores <- function(cube, ap, where="pdf", comps= c(1:5), pcs=c(1,2), onMain="", onSub="", fns="") {
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("Making score plots... \n")}
	expName <- getExpName(cube)
	height <-.ap2$stn$pdf_Height_sq
	width <- .ap2$stn$pdf_Width_sq
	path <- .ap2$stn$fn_results
	suffix <- "pcaScores"
	message <- "PCA Scores"
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	onMain <- paste(expName, onMain, sep=" ")
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
	if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new()}	
	makePCAScorePlots(cube, ap, comps, pcs, onMain, onSub)
	if (where == "pdf") {dev.off()}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat(paste(message, "plotted.\n")) }
} # EOF

makePCALoadingPlots<- function(cube, ap, comps=c(1:5), onMain="", onSub="", where="pdf") {	# not in use; the template from demar
	for (k in 1:  length(cube)) {
		set <- cube[[k]]
		PCAObject <- getPCAObject(set)
		selLoadings <- PCAObject$loadings[, comps]
		allVariancesPCs <- round((ChemometricsWithR::variances(PCAObject) / PCAObject$totalvar)*100, get(".dstn")$nrDigitsVariance)
		ind99 <- which(cumsum(allVariancesPCs) > 99)[1]
		ind90 <- which(cumsum(allVariancesPCs) > 90)[1]
		ind80 <- which(cumsum(allVariancesPCs) > 80)[1]
		selVariancesPCs <- allVariancesPCs[comps]
		legendText <- paste(colnames(selLoadings), " (", selVariancesPCs, "%)", sep="")
		onSubText <- paste(onSub, " ", ind99, " PCs for > 99% var.,  ", ind90, "PCs > 90%var.,  ", ind80, "PCs > 80%var", sep="")
#		xaxis <- 1: ncol(getAllData(demarMod))
#		color <- de_correctForColorsHigherThan8( seq(1, length(comps)) )
#		matplot(xaxis, selLoadings, type="l", lwd=1, main=onMain, sub=onSubText, col= color, xlab="Feature #", ylab="Loadings")
		abline(0,0, col="lightgray")
#		legend("topright", legend=legendText, lty=1:length(comps), col=color, lwd=2.5)
	#	plotFeaturesAreas(demarMod, range(selLoadings), what="all")
		if (get(".dstn")$addScreeplot) {
			if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new()}	
			ChemometricsWithR::screeplot(PCAObject, "percentage", main=onMain, sub=onSub)
			abline(v=ind99, col="red")
			abline(v=ind90, col="green")
			abline(v=ind80, col="blue")
		}
	} # end for k
} # EOF

## npcs defining the range of the loadings that we want to see
makePCALoadingPlots2 <- function (cube, ap, npcs, onMain="", onSub="", where="") { ## nur provisorisch !!! XXX
	for (k in 1: length(cube)) {
		set <- cube[[k]]
		PCAObject <- getPCAObject(set)
		wls <- getWavelengths(getDataset(set))
		color <- npcs
	#	color <- correctForColorsHigherThan8(color)
		#par(mar=c(8, 4, 8, 2) + 0.1)
		matplot(wls, PCAObject$loadings[,npcs], type="l", lwd=1, main=onMain, sub=onSub, col= color, xlab="Wavelength", ylab="Loadings")
		abline(0,0, col="lightgray")
		legend("topright", legend=paste("PC", npcs, col=color))
	} # end for k
} #EOF

plotPCA_Loadings <- function(cube, ap, where="pdf", comps=c(1:5), onMain="", onSub="", fns="") {
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("Making loading plots... \n")}
	expName <- getExpName(cube)
	height <-.ap2$stn$pdf_Height_ws
	width <- .ap2$stn$pdf_Width_ws
	path <- .ap2$stn$fn_results
	suffix <- "pcaLoadings"
	message <- "PCA Loadings"
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	onMain <- paste(expName, onMain, sep=" ")
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
	if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new()}	
	makePCALoadingPlots2(cube, ap, npcs=comps, onMain, onSub, where)
	if (where == "pdf") {dev.off()}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat(paste(message, "plotted.\n")) }
} # EOF

#'  @title PCA - plot
#'  @description Plot PCA scoresplots and / or loadings.
#'  @details The width and height of the resulting pdf can be set in the settings.
#'  @param cube An object of class 'aquap_cube' as produced by \code{\link{gdmm}}.
#'  @param ... Optional pca plotting parameters to override the values in the 
#'  analysis procedure stored in the 'cube' - see below.
#'  @template mr_pca_plot_param
#'  @return A pdf or graphic device.
#'  @family Plot functions
#'	@export
plot_pca <- function(cube, ...) {
	autoUpS()
	ap <- getap(.lafw_fromWhere="cube", cube=cube, ...)			 # the ... are here used for additionally modifying (if matching arguments) the analysis procedure obtained from the cube
	ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset=getDataset(cube[[1]])) # just take the first dataset, as we mainly need the header (and the wavelengths are already checked.. )
	if (is.null(ap$pca)) {
		return(cat("*** PCA model not available or not selected for plotting \n"))
	}
	where <- ap$genPlot$where
	onMain <- ap$genPlot$onMain
	onSub <- ap$genPlot$onSub
	fns <- ap$genPlot$fns
	what <- ap$pca$what
	pcSc <- ap$pca$pcSc
	pcs <- ap$pca$pcs
	pcLo <- ap$pca$pcLo
	##
	pv <- pv_pca_what 		# c("both", "scores", "loadings")
	if (any(c(pv[1], pv[2]) %in% what)) { # scores
		plotPCA_Scores(cube, ap, where, comps=pcSc, pcs, onMain, onSub, fns)	# ok
	}
	if (any(c(pv[1], pv[3]) %in% what)) { # loadings
		plotPCA_Loadings(cube, ap, where, comps=pcLo, onMain, onSub, fns)	# XXX not ok!
	}
} # EOF

# make nice elements for anproc to be used in topic specific documentation
# make documentation for plot_cube and plot_pca
# document general plotting optinos in anproc
