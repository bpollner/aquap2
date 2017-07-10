makePCAScorePlots <- function(cube, ap, comps=c(1:5), pcs=c(1,2), onMain="", onSub="") {
#	plot(0, type="n")
	trRobust <- .ap2$stn$pca_CI_ellipse_robust
	trCenterPch <- .ap2$stn$pca_CI_ellipse_centerPch
	trLty <- .ap2$stn$pca_CI_ellipse_lty
	trLwd <- .ap2$stn$pca_CI_ellipse_lwd
	trElci <- ap$pca$elci
	trKeyBGCol <- "white"
	trAlphaBG <- 0.85 # 0 is completely transparent
	minPartElipse <- 3
	#
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
#		idString <- getIdString(set)
		idString <- adaptIdStringForDpt(ap, getIdString(set))
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
			legCex <- aa$legCex
			legNrCols <- aa$legNrCols
			###
			linesFill <- NULL
			if (!is.null(trElci)) {
				elc <- extractColorLegendValues(getDataset(set), classList[i], minPart=minPartElipse, ltyIn=trLty)
				trLtyUse <- trLty
				if (length(elc$color_legend) == 1) { # otherwise we have a second legend element with only a line and no data
					trLtyUse <-trLty[1]
				}
				linesFill <- list(lwd=1, col=elc$color_legend, lty=trLtyUse)
			trLegend1 <- list(border=TRUE, points=list(pch=16, col=colorLegend), lines=linesFill, text=list(legendTextExt), background=trKeyBGCol, alpha.background=trAlphaBG, title=classList[i], cex.title=0.92, cex=legCex, columns=legNrCols)
			} else {
				trLegend1 <- list(border=TRUE, points=list(pch=16, col=colorLegend), text=list(legendTextExt), background=trKeyBGCol, alpha.background=trAlphaBG, title=classList[i], cex.title=0.92, cex=legCex, columns=legNrCols)
			}
			###
			subText <- paste("color by ", classList[i], onSubFill, " (total N=", sumPart, ")", sep="")
			trSub <- list(label=subText, fontface=1)
			trLegend_i <- list(inside=list(fun=lattice::draw.key(trLegend1), corner=c(1,1)))
			trelPlot1 <- lattice::xyplot(y ~ x, sub=trSub, main=mainText, xlab=xlab, ylab=ylab, col=colorData, pch=16, cex=0.85, legend=trLegend_i, 
				panel=function(x, y, ...) {
					lattice::panel.xyplot(x, y, ...) # plot the data
		    	    lattice::panel.abline(h=0, v=0, lty=2, col="gray")
					if (!is.null(trElci)) {
						cel <- elc$color_legend
						cel <- cel[which(!is.na(cel))]
						ltyEl <- elc$ltyNA
						ltyEl <- ltyEl[which(!is.na(ltyEl))]
						latticeExtra::panel.ellipse(x, y, col=cel, center.pch=trCenterPch, robust=trRobust, groups=elc$dataGrouping, scales="free", level=trElci, lwd=trLwd, lty=ltyEl, subscripts=TRUE)
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
				legCex_el <- bb$legCex		
				legNrCols_el <- bb$legNrCols
				elc <- extractColorLegendValues(getDataset(set), el2colorBy[i], minPart=minPartElipse, ltyIn=trLty) # possibly knocking out participants !
				###
				# below was: col=colorLegend_el
				trLegend2 <- list(border=TRUE, points=list(pch=pch_legend_el, col="black"), lines=list(lwd=1, col=elc$color_legend, lty=trLty), text=list(legendTextExt_el), title=el2colorBy[i], cex.title=0.92, cex=legCex_el, columns=legNrCols_el, background=trKeyBGCol, alpha.background=trAlphaBG)
				trSub2 <- list(label=paste(subText, ", CI ellipses by ", el2colorBy[i], sep=""), fontface=1)
				trAllLegends <- list(inside=list(fun=lattice::draw.key(trLegend1), corner=c(1,1)), inside=list(fun=lattice::draw.key(trLegend2), corner=c(0,0)))
				trelPlot2 <- lattice::xyplot(y ~ x, sub=trSub2, main=mainText, xlab=xlab, ylab=ylab, col=colorData, pch=pch_data_el, cex=0.85, legend=trAllLegends, 
					panel=function(x, y, ...) {
						lattice::panel.xyplot(x, y, ...) # plot the data
		    	    	lattice::panel.abline(h=0, v=0, lty=2, col="gray")
		    	    	#
		#				grpEl <- elc$dataGrouping
		#				grpEl <- grpEl[which(!is.na(grpEl))] 
						cel <- elc$color_legend
						cel <- cel[which(!is.na(cel))]
						ltyEl <- elc$ltyNA
						ltyEl <- ltyEl[which(!is.na(ltyEl))]
						latticeExtra::panel.ellipse(x, y, col=cel, center.pch=trCenterPch, robust=trRobust, groups=elc$dataGrouping, scales="free", level=trElci, lwd=trLwd, lty=ltyEl, subscripts=TRUE)
		#				latticeExtra::panel.ellipse(x, y, col=colorLegend_el, center.pch=trCenterPch, robust=trRobust, groups=grouping_el, scales="free", level=trElci, lwd=trLwd, lty=trLty, subscripts=TRUE)
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

# not in use; the template from demar
makePCALoadingPlots<- function(cube, ap, comps=c(1:5), onMain="", onSub="", where="pdf") {	
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
makePCALoadingPlots2 <- function (cube, ap, comps, onMain="", onSub="", where="", bandwidth=25, adLines=TRUE, ccol=NULL, clty=NULL) { 
	discrim <- .ap2$stn$pca_loadings_discrim
	nrDigitsVariance <- .ap2$stn$pca_nrDigitsVariance
	#
	for (k in 1: length(cube)) {
		set <- cube[[k]]
		mainTxt <- paste(onMain, adaptIdStringForDpt(ap, getIdString(set)), sep=" ")
		wavelengths <- getWavelengths(set) # the set contains the dataset
		PCAObject <- getPCAObject(set)
		allVariancesPCs <- round((ChemometricsWithR::variances(PCAObject) / PCAObject$totalvar)*100, nrDigitsVariance)
		ind99 <- which(cumsum(allVariancesPCs) > 99)[1]
		selVariancesPCs <- allVariancesPCs[comps]
		pcaVariances <- list(ind99=ind99, vars=selVariancesPCs)
		#
		pickResults <- pickPeaks(PCAObject, bandwidth, comps, discrim, wavelengths)
		plotPeaks(pickResults, onMain=mainTxt, onSub, adLines, pcaVariances, customColor=ccol, ylim=NULL, wavelengths, clty)		### !! here the plotting !!!
	} # end for k
} #EOF

plotPCA_Loadings <- function(cube, ap, where="pdf", comps=c(1:5), onMain="", onSub="", fns="", bandwidth=25, adLines=TRUE, ccol=NULL, clty=NULL) {
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
	makePCALoadingPlots2(cube, ap, comps, onMain, onSub, where, bandwidth, adLines, ccol, clty)
	if (where == "pdf") {dev.off()}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF

plot_pca_checkDefaultsParams <- function(ld.bandwidth, ld.adLines, ld.col, ld.lty) {
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
	assign("ld.adLines", aa, pos=parent.frame(n=1))
	##
	ccol <- ld.col
	if (all(ccol == "def")) {
		ccol <- .ap2$stn$pca_ld_customColor
	}
	if (!is.null(ccol)) {
		# XXX ? how to check for existence, validity of the provided colors ?
	}
	assign("ld.col", ccol, pos=parent.frame(n=1))
	##
	lty <- ld.lty
	if (all(lty == "def")) {
		lty <- .ap2$stn$pca_ld_customLinetype
	}
	if (!is.null(lty)) {
		if (!all(is.wholenumber(lty)) | !all(lty > 0) ) {
			stop("Please provide a positive integer vector for the argument 'ld.lty'.", call.=FALSE)
		}
	}
	assign("ld.lty", lty, pos=parent.frame(n=1))
	##	
} # EOF

#### CORE ###
plot_pca_cube <- function(cube, aps="def", ld.bandwidth="def", ld.adLines="def", ld.col="def", ld.lty="def", ...) {
	autoUpS()
	ap <- doApsTrick(aps, cube, ...)
	ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset=getDataset(cube[[1]]), haveExc=FALSE) # just take the first dataset, as we mainly need the header (and the wavelengths are already checked.. )
	if (is.null(ap$pca)) {
		return(cat("*** PCA model not available or not selected for plotting \n"))
	}
	plot_pca_checkDefaultsParams(ld.bandwidth, ld.adLines, ld.col, ld.lty) # is assigning here !!
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
		plotPCA_Loadings(cube, ap, where, comps=pcLo, onMain, onSub, fns, bandwidth=ld.bandwidth, adLines=ld.adLines, ccol=ld.col, clty=ld.lty)	
	}
} # EOF

setAllSplitVarsToNull <- function(ap) {
	ucl <- list(splitClasses=NULL, splitWl=NULL)
	ap$ucl=ucl
	ap$dpt$csAvg$doAvg <- FALSE
	ap$dpt$csAvg$useRaw <- TRUE
	ap$dpt$noise$useNoise <- FALSE
	ap$dpt$noise$useRaw <- TRUE
	ap$dpt$excludeOutliers$exOut <- FALSE
	ap$dpt$excludeOutliers$exOutRaw <- FALSE
	ap$dpt$excludeOutliers$exOutVar <- NULL
	dptModules <- list(dptPre=NULL, dptPost=NULL)
	ap$dpt$dptModules=dptModules
	return(ap)	
} # EOF

## core for dataset ##
plot_pca_data <- function(dataset, aps="def", ...) {
	autoUpS()
	aps <- checkApsChar(aps)
	if (aps=="cube") {
		message("'aps=cube' is not meaningful in this context, aps switched to be 'def', meaning the default anproc filename will be used.\nAll is good.")
		aps <- "def"
	}
	check_apDefaults(fn=aps)
	ap <- getap(fn=aps, do.pca=TRUE, do.sim=FALSE, do.pls=FALSE, do.aqg=FALSE, ...) # lets do only the pca!
	ap <- setAllSplitVarsToNull(ap)
	##

	prev <- .ap2$stn$allSilent
	.ap2$stn$allSilent <<- TRUE
	cube <- gdmm(dataset, ap=ap) ##### generate the cube !!! ######
	prevIdString <- getIdString(cube[[1]]) # cube has only 1 set here !!
	origAp <- getAnproc(dataset)
	origDptMods <- origAp$dpt$dptModules
#	cube[[1]]@idString <- adaptIdStringForDpt(dptSource=origAp, prevIdString) # put all the data treatment info in the idString
	cube@anproc$dpt$dptModules <- origDptMods # put the dpt info that we got from the dataset into the cube AFTER the gdmm, so that we do not do the treatment again!!
	.ap2$stn$allSilent <<- prev
	plot_pca_cube(cube, aps="cube", ...)
} # EOF


#' @title Plot PCA
#' @description Plot PCA scoresplots and / or loadings.
#' @details By providing either an object of class "aquap_cube" or of class 
#' "aqua_data" to the argument \code{object}, you can plot either the pca-modells 
#' of the complete cube, or calculate and plot the pca-modell of a single dataset.
#' By providing one or more of the \code{...} arguments you can override the 
#' plotting values in the selected analysis procedure, see below.
#' The width and height of the resulting pdf can be set in the settings.
#' @param object An object of class "aquap_cube" (as produced by 
#' \code{\link{gdmm}}) or of class "aqua_data" (as produced by 
#' \code{\link{gfd}}, see also \code{\link{getcd}}).
#' @param ... Optional pca plotting parameters to override the values in the 
#' analysis procedure and of the arguments listed below - for possible arguments
#' see \code{\link{plot_pca_args}} and here below:
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
#' \item{ld.bandwidth}{Character "def", or numeric length one. The bandwidth of 
#' wavelengths used in the peak-picking process when plotting the loadings. If 
#' left at the default "def", the value from the settings.r file 
#' (parameter \code{pp_bandwidth}) is used.}
#'  \item{ld.adLine}{Logical, numeric or character 'def'. If set to \code{FALSE},
#' no additional lines, if set to \code{TRUE} all the additional lines will be 
#' plotted in the loading plot. If an integer vector [2..5] is provided, one or 
#' more of the additional lines get plotted. See \code{\link{adLinesToVector}} 
#' for details. If left at the default value 'def', the value from the 
#' settings.r file (parameter \code{pca_AdLines}) is used.}
#'  \item{ld.col}{ Character "def" or a valid color vector. A possible custom color 
#' for coloring the loadings in the loading plot. When left at the default "def" 
#' the value fromt the settings.r file is read in (parameter 
#' \code{pca_ld_customColor}). Provide a color-vector to use these colors for 
#' coloring the loading plot.}
#'  \item{ld.lty}{Character "def" or a positive integer vector indicating the 
#' desired line types. When left at the default "def" the value from the 
#' settings.r file is read in (parameter \code{pca_ld_customLinetype}).}
#'  } 
#' @return A pdf or graphic device.
#' @family Plot functions
#' @family PCA documentation
#' @seealso \code{\link{getcd}}
#' @examples
#'  \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset)
#'  plot(cube)
#'  plot_pca(cube)
#'  plot_pca(cube, ld.bandwidth=12) # for detecting more peaks
#'  plot_pca(cube, ld.adLines=FALSE) # no additional lines in the loading plot 
#'  plot_pca(cube, ld.adLines=c(2, 4) # only vertical lines and WAMACs in the 
#'  # loading plot
#'	plot_pca(cube, ld.col=c("red", "blue", "green"))
#'	plot_pca(cube, ld.lty=c(1,2))
#'	###
#'	plot_pca(cube, aps="cube") # to take the analysis procedure from the cube
#'	plot_pca(cube, aps="myAnproc.r") # take a custom analysis procedure file
#'	###
#'	fd <- gfd()
#'	plot_pca(fd, pg.where="")
#'	plot_pca(fd, pg.where="", pca.colorBy="C_Group") # assumes existence of "C_Group"
#' }
#'@name plot_pca
NULL


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
#' \code{plot_pca(dataset, ...)}
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
