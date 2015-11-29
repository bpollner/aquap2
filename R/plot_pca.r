makePCAScorePlots <- function(cube, ap, comps=c(1:5), pcs=c(1,2), onMain="", onSub="") {
#	plot(0, type="n")
	trRobust <- .ap2$stn$pca_CI_ellipse_robust
	trCenterPch <- .ap2$stn$pca_CI_ellipse_centerPch
	trLty <- .ap2$stn$pca_CI_ellipse_lty
	trLwd <- .ap2$stn$pca_CI_ellipse_lwd
	trElci <- ap$pca$elci
	trKeyBGCol <- "white"
	trAlphaBG <- 0.85 # 0 is completely transparent
	el2colorBy <- ap$pca$elcolorBy
	classList <- getPCAClassList(ap)
	if (!is.null(el2colorBy)) {
		if (length(el2colorBy) == 1) {
			el2colorBy <- rep(el2colorBy, length(classList))
		}
	}
	if (onSub == "") {
		onSubFill <- " "
	} else {
		onSubFill <- paste(" ", onSub, " ", sep="")
	}
	for (k in 1: length(cube)) {	
		set <- cube[[k]]
		PCAObject <- getPCAObject(set)
		idString <- getIdString(set)
		allVariancesPCs <- round((ChemometricsWithR::variances(PCAObject) / PCAObject$totalvar)*100, .ap2$stn$pca_nrDigitsVariance)
		cumSumVars <- cumsum(allVariancesPCs)
		ind99 <- which(cumSumVars >= 99)[1]
		ind99Txt <- paste("   [", ind99, " PCs for 99% var.]", sep="")
		indNoChange <- max(which(diff(cumSumVars) != 0)) # the last one where the variance is increasing; not yet in use
		x <- PCAObject$scores[, pcs[1]]
		y <- PCAObject$scores[, pcs[2]]
		xlab <- paste("PC ", pcs[1], " (", allVariancesPCs[pcs[1]], "%)", ind99Txt, sep="")
		ylab <- paste("PC ", pcs[2], " (", allVariancesPCs[pcs[2]], "%)", sep="")
		mainText <- paste(onMain, idString)
		for (i in 1: length(classList)) {
			###
			aa <- extractColorLegendValues(getDataset(set), classList[i])  # color_data, color_unique, color_legend, txt, txtE, sumPart, dataGrouping
			colorData <- aa$color_data
			colorUnique <- aa$color_unique
			colorLegend <- aa$color_legend
			legendTextExt <- aa$txtE
			sumPart <- aa$sumPart
			grouping <- aa$dataGrouping
			###
			trLegend1 <- list(border=TRUE, points=list(pch=16, col=colorLegend), text=list(legendTextExt), background=trKeyBGCol, alpha.background=trAlphaBG, title=classList[i], cex.title=1)
			subText <- paste("color by ", classList[i], onSubFill, " (total N=", sumPart, ")", sep="")
			trSub <- list(label=subText, fontface=1)
			trLegend_i <- list(inside=list(fun=lattice::draw.key(trLegend1), corner=c(1,1)))
			trelPlot1 <- lattice::xyplot(y ~ x, sub=trSub, main=mainText, xlab=xlab, ylab=ylab, col=colorData, pch=16, cex=0.85, legend=trLegend_i, 
				panel=function(x, y, ...) {
					lattice::panel.xyplot(x, y, ...) # plot the data
		    	    lattice::panel.abline(h=0, v=0, lty=2, col="gray")
					if (!is.null(trElci)) {
						latticeExtra::panel.ellipse(x, y, col=colorUnique, center.pch=trCenterPch, robust=trRobust, groups=grouping, scales="free", level=trElci, lwd=trLwd, lty=trLty, subscripts=TRUE)
					}
				}
			) # end of call to trelPlot1
			print(trelPlot1)
			if (!is.null(el2colorBy) & !is.null(trElci)) { # so we want an other plot with additional CI ellipses			
				###
				bb <- extractColorLegendValues(getDataset(set), el2colorBy[i]) 
				colorUnique_el <- bb$color_unique
				colorLegend_el <- bb$color_legend
				legendTextExt_el <- bb$txtE
				grouping_el <- bb$dataGrouping
				pch_data_el <- bb$pch_data
				pch_legend_el <- bb$pch_legend
				###
				trLegend2 <- list(border=TRUE, points=list(pch=pch_legend_el, col="black"), lines=list(lwd=1, col=colorLegend_el, lty=trLty), text=list(legendTextExt_el), title=el2colorBy[i], cex.title=1, background=trKeyBGCol, alpha.background=trAlphaBG)
				trSub2 <- list(label=paste(subText, ", CI ellipses by ", el2colorBy[i], sep=""), fontface=1)
				trAllLegends <- list(inside=list(fun=lattice::draw.key(trLegend1), corner=c(1,1)), inside=list(fun=lattice::draw.key(trLegend2), corner=c(0,0)))
				trelPlot2 <- lattice::xyplot(y ~ x, sub=trSub2, main=mainText, xlab=xlab, ylab=ylab, col=colorData, pch=pch_data_el, cex=0.85, legend=trAllLegends, 
					panel=function(x, y, ...) {
						lattice::panel.xyplot(x, y, ...) # plot the data
		    	    	lattice::panel.abline(h=0, v=0, lty=2, col="gray")
						latticeExtra::panel.ellipse(x, y, col=colorUnique_el, center.pch=trCenterPch, robust=trRobust, groups=grouping_el, scales="free", level=trElci, lwd=trLwd, lty=trLty, subscripts=TRUE)
					}
				) # end of call to trelPlot2
				print(trelPlot2)
			} # end !is.null(el2colorBy)
			#
			if (!is.null(comps)) {
				pairs(PCAObject$scores[,comps], col=colorData, pch=16, main=mainText, sub=subText )
			}
		} # end for i
	} # end for k
} #EOF

plotPCA_Scores <- function(cube, ap, where="pdf", comps= c(1:5), pcs=c(1,2), onMain="", onSub="", fns="") {
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("Plotting PCA score plots... ")}
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
	if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new(height=height, width=width)}	
	makePCAScorePlots(cube, ap, comps, pcs, onMain, onSub)
	if (where == "pdf") {dev.off()}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
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
makePCALoadingPlots2 <- function (cube, ap, comps, onMain="", onSub="", where="", bandwidth=25, adLines=TRUE) { ## nur provisorisch !!! XXX
	discrim <- .ap2$stn$pca_loadings_discrim
	nrDigitsVariance <- .ap2$stn$pca_nrDigitsVariance
	#
	for (k in 1: length(cube)) {
		set <- cube[[k]]
		mainTxt <- paste(onMain, getIdString(set), sep=" ")
		wavelengths <- getWavelengths(set) # the set contains the dataset
		PCAObject <- getPCAObject(set)
		allVariancesPCs <- round((ChemometricsWithR::variances(PCAObject) / PCAObject$totalvar)*100, nrDigitsVariance)
		ind99 <- which(cumsum(allVariancesPCs) > 99)[1]
		selVariancesPCs <- allVariancesPCs[comps]
		pcaVariances <- list(ind99=ind99, vars=selVariancesPCs)
		#
		pickResults <- pickPeaks(PCAObject, bandwidth, comps, discrim, wavelengths)
		plotPeaks(pickResults, onMain=mainTxt, onSub, adLines, pcaVariances, customColor=NULL, ylim=NULL, wavelengths)		### !! here the plotting !!!
	} # end for k
} #EOF

plotPCA_Loadings <- function(cube, ap, where="pdf", comps=c(1:5), onMain="", onSub="", fns="", bandwidth=25, adLines=TRUE) {
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("Plotting PCA loading plots... ")}
	expName <- getExpName(cube)
	height <-.ap2$stn$pdf_Height_ws
	width <- .ap2$stn$pdf_Width_ws
	path <- .ap2$stn$fn_results
	suffix <- "pcaLoadings"
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	onMain <- paste(expName, onMain, sep=" ")
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
	if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new(height=height, width=width)}	
	makePCALoadingPlots2(cube, ap, comps, onMain, onSub, where, bandwidth, adLines)
	if (where == "pdf") {dev.off()}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF

plot_pca_checkDefaultsParams <- function(ld.bandwidth, ld.adLines) {
	if (all(ld.bandwidth == "def")) {
		ld.bandwidth <- .ap2$stn$pp_bandwidth
	}
	if (!all(is.numeric(ld.bandwidth)) | length(ld.bandwidth) != 1) {
		stop("Please provide a numeric length one for the argument 'ld.bandwidth'.", call.=FALSE)
	}
	assign("ld.bandwidth", ld.bandwidth, pos=parent.frame(n=1))
	##
	aa <- ld.adLines
	if (all(aa == "def")) {
		aa <- .ap2$stn$pca_AdLines
	}
	if (!is.logical(aa)) {
		if (!all(is.wholenumber(aa)) | (min(aa) < 2) | (max(aa) > 5)) {
			stop("Please provide an integer vector ranging from 2..5 to the argument 'ld.adlines'.", call.=FALSE)
		}
	} # end not logical
	assign("ld.adlines", aa, pos=parent.frame(n=1))
} # EOF

#' @title Plot PCA
#' @description Plot PCA scoresplots and / or loadings.
#' @details The width and height of the resulting pdf can be set in the settings.
#' @param cube An object of class 'aquap_cube' as produced by \code{\link{gdmm}}.
#' @param ld.bandwidth Character "def", or numeric length one. The bandwidth of 
#' wavelengths used in the peak-picking process when plotting the loadings. If 
#' left at the default "def", the value from the settings.r file 
#' (parameter \code{pp_bandwidth}) is used.
#' @param ld.adLines Logical, numeric or character 'def'. If set to \code{FALSE},
#' no additional lines, if set to \code{TRUE} all the additional lines will be 
#' plotted in the loading plot. If an integer vector [2..5] is provided, one or 
#' more of the additional lines get plotted. See \code{\link{adLinesToVector}} 
#' for details. If left at the default value 'def', the value from the 
#' settings.r file (parameter \code{pca_AdLines}) is used.
#' @param ... Optional pca plotting parameters to override the values in the 
#'  analysis procedure stored in the 'cube' - for possible arguments see 
#'  \code{\link{plot_pca_args}}.
#' @return A pdf or graphic device.
#' @family Plot functions
#' @family PCA documentation
#' @examples
#'  \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset)
#'  plot(cube)
#'  plot_pca(cube)
#' }
#'@export
plot_pca <- function(cube, ld.bandwidth="def", ld.adLines="def", ...) {
	autoUpS()
#	ap <- getap(.lafw_fromWhere="cube", cube=cube, ...)			 # the ... are here used for additionally modifying (if matching arguments) the analysis procedure obtained from the cube
	ap <- getap(...) # load from file, possibly modify via ...
	ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset=getDataset(cube[[1]])) # just take the first dataset, as we mainly need the header (and the wavelengths are already checked.. )
	if (is.null(ap$pca)) {
		return(cat("*** PCA model not available or not selected for plotting \n"))
	}
	plot_pca_checkDefaultsParams(ld.bandwidth, ld.adLines) # is assigning here !!
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
		plotPCA_Scores(cube, ap, where, comps=pcSc, pcs, onMain, onSub, fns)
	}
	if (any(c(pv[1], pv[3]) %in% what)) { # loadings
		plotPCA_Loadings(cube, ap, where, comps=pcLo, onMain, onSub, fns, bandwidth=ld.bandwidth, adLines=ld.adLines)	
	}
} # EOF


#' @title Plot PCA - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{plot}} and \code{\link{plot_pca}} to override the values 
#' in the analysis procedure file and so to modify the graphics of score and 
#' loading plots - see examples.
#' 
#' \code{plot(cube, ...)}
#' 
#' \code{plot_pca(cube, ...)}
#' 
#' @template mr_details_allParams
#' @template mr_pca_plot_param
#' @template mr_pg_genParams
#' @seealso \code{\link{plot_pca}}
#' @examples
#' \dontrun{
#' dataset <- gfd()
#' cube <- gdmm(dataset)
#' plot(cube, pca.colorBy="C_Group", pca.sc.pairs=NULL) 
#' # will plot every available model
#' plot_pca(cube, pca.colorBy="C_Group", pca.sc.pairs=NULL) 
#' # will plot only the PCA
#' plot_pca(cube, pca.what="scores")
#' }
#' @family Plot arguments
#' @family PCA documentation
#' @name plot_pca_args
NULL


#' @title Calculate PCA - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{getap}}, also within function \code{\link{gdmm}}, to 
#' override the values in the analysis procedure file and so to modify the 
#' calculation of PCA - see examples.
#' 
#' \code{getap(...)}
#'  
#' \code{gdmm(dataset, ap=getap(...))}
#' 
#' @section Note: Calculation of PCA is done with the function 
#' \code{\link[ChemometricsWithR]{PCA}}
#' @template mr_details_allParams
#' @template mr_pca_calc_param
#' @seealso \code{\link{gdmm}}
#' @examples
#' \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset, getap(do.pca=TRUE))
#' }
#' @family Calc. arguments
#' @family PCA documentation
#' @name calc_pca_args
NULL
