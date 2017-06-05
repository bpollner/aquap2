# old above ---------------------------------
aq_makeNicePlottingFrame_circ <- function(aquCalcResult, selWls, masterScale) {
	nrDigits <- .ap2$stn$aqg_nrDigitsAquagram
	dataForPlotting <- aquCalcResult
	colnames(dataForPlotting) <- as.character(selWls)
#	rownames(dataForPlotting) <-calcAquagrResult$avg[,1]
	##
	if (is.null(masterScale)) {
		maxRange_xy <- rep(max(dataForPlotting), ncol(dataForPlotting)) 
		minRange_xy <- rep(min(dataForPlotting), ncol(dataForPlotting))
	} else {
		maxRange_xy <- rep(max(masterScale), ncol(dataForPlotting)) 
		minRange_xy <- rep(min(masterScale), ncol(dataForPlotting))
	}
	maxMin <- rbind(maxRange_xy, minRange_xy) ## whatever to avoid coincidental name-clashes
	colnames(maxMin) <- as.character(selWls)
	dataForPlotting <- rbind(maxMin, dataForPlotting) 		## needed to have same range in every spike of the radarplot
	caxislabels<-round(c(minRange_xy[1], minRange_xy[1] + (maxRange_xy[1] - minRange_xy[1])/4, minRange_xy[1] + (maxRange_xy[1] - minRange_xy[1])/2, minRange_xy[1] + ((maxRange_xy[1] - minRange_xy[1])/4)*3, maxRange_xy[1]), nrDigits) 		## makes for nice 5 labels in the given range
	dataForPlotting <- data.frame( dataForPlotting[,c(1, ncol(dataForPlotting):2)] ) ## re-arrange and make clock-wise
	colnames(dataForPlotting) <- substr(colnames(dataForPlotting), 2, nchar(colnames(dataForPlotting)))
	out <- list(Data=dataForPlotting, Labels=caxislabels)
} # EOF

aq_makeNicePlottingFrame_linear <- function(aquCalcResult, onMain, onSub, mod, Texp, customColor, clt, R, minus, inBoot=FALSE) {
	settingsLT <- .ap2$stn$aqg_linetypes
	ltyCI <- .ap2$stn$aqg_plot_ltyCIs
	##
	if (is.numeric(clt)) {
		ltPlot <- ltLeg <- clt
	} else {
		ltPlot <- ltLeg <- settingsLT
	}	
	##
	if (grepl("dce", mod)) {
		TexpLine <- Texp
	} else {
		TexpLine  <- 0
	}	
	##
	legTitle <- aquCalcResult@classVar
	dataColor <- legendColor <- aq_checkColors(aquCalcResult@colRep, customColor)
	plotData <- aquCalcResult@avg
	possN <- aquCalcResult@possN
#	if (grepl("diff", mod)) {
#		indOut <- which(rownames(plotData) == minus)
#		legendColor <- legendColor[-indOut]
#		legTextExt <- legTextExt[-indOut]
#		possN <- possN[-indOut]
#	}	
	groups <- rownames(plotData)
	legTextExt <- paste0(groups, " N=", possN)
	#
	options(warn=-1)
	grNrs <- as.numeric(groups)
	options(warn=0)
	if (!any(is.na(grNrs))){ # so we have, indeed, all numbers
		ord <- order(grNrs)
	} else {
		ord <- order(groups)
	}
	legTextExt <- legTextExt[ord]
	legendColor <- aq_checkReSortLegendColor(legendColor, ord, aquCalcResult@colRep, customColor)
	plotData <- aq_checkReSortPlotData(plotData, ord, aquCalcResult@colRep, customColor)
	#
	lwd <- 1
	##
	if (inBoot) {
		if (!is.null(aquCalcResult@bootRes)) {
			plotData <- aquCalcResult@bootRes
			dataColor <- rep(dataColor, each=3)
			ltPlot <- c(1, ltyCI, ltyCI) # otherwise could be: ltPlot <- c(ltPlot, 3, 3)
			ltLeg <- 1		## ( the legend text stays the same as above)
			lwd <- c(1, 0.5, 0.5)
			onSub <- paste(onSub, "   95% CI based on", R, "bootstrap replicates (bca)")	
		} else {
			plotData <- NULL
		} 
	}
	return(list(plotData=plotData, onMain=onMain, onSub=onSub, yLab=mod, TexpLine=TexpLine, legTextExt=legTextExt, legTitle=legTitle, dataColor=dataColor, legendColor=legendColor, ltPlot=ltPlot, ltLeg=ltLeg, lwd=lwd))
} # EOF

aq_makeGraphicsTexts <- function(onSub, aqCalcPossNrPart, nrCorr) {
	possNrPartic <- aqCalcPossNrPart
	possNrPart <- paste(possNrPartic, collapse=", ")
	if (nrCorr) {
		eachN_msg <- paste(" (each N=", min(possNrPartic), ")", sep="")
		if (diff(range(possNrPartic)) == 0 ) {
			mText <- ""
		} else {
			mText <- paste("Max. N = ", possNrPart , "\n", eachN_msg, sep="")
		}
	} else {
		if (diff(range(possNrPartic)) == 0 ) {
			eachN_msg <- paste(" (each N=", min(possNrPartic), ")", sep="")
			mText <- ""
		} else {
			eachN_msg <-  " (not N corr.)"
			mText <- paste("N=", possNrPart, collapse=", ")
		}
	}
	onSubNew <- paste(onSub, eachN_msg, sep="")
	out <- list(onSub=onSubNew, mText=mText)
} # EOF

aq_checkReSortLegendColor <- function(legendColor, ord, numRepColor, customColor) {
	if (!is.null(customColor)) {
		if (length(numRepColor) != length(customColor)) {
			return(legendColor[ord]) 	## so either there was a misstake, or the coloring is meant for an other grouping
		}
		return(legendColor) # return the input-custom color
	} else { # so if customColor is NULL
		return(legendColor[ord])
	}
} # EOF

aq_checkReSortPlotData <- function(plotData, ord, numRepColor, customColor) {
	if (!is.null(customColor)) {
		if (length(numRepColor) != length(customColor)) {
			return(plotData) 	## so either there was a misstake, or the coloring is meant for an other grouping
		}
		return(plotData[ord,]) # so only in case of a the right length of custom color we are re-sorting the plotting data
	} else { # so if customColor is NULL
		return(plotData)
	}
} # EOF

aq_checkColors <- function(numRepColor, customColor) {
	if (!is.null(customColor)) {
			Color <- customColor
		if (length(numRepColor) != length(customColor)) {
			Color <- numRepColor 	## so either there was a misstake, or the coloring is meant for an other grouping
		}
	} else { # so if customColor is NULL
		Color <- numRepColor
	}
	return(Color)
} # EOF

aq_checkLegendTextMod <- function(mod, minus, TCalib, Texp) {
#	legTextMod <- "Mode: Classic \nT-Calib: NA \n "
	if (mod == "aucs-diff" | mod == "sfc-diff" | mod == "classic-diff" | mod == "aucs.tn-diff" | mod == "aucs.tn.dce-diff" | mod == "aucs.dce-diff") {
		diffText <- paste("Minus: ", minus, sep="")
	} else {
		diffText <- ""
	}
	if (is.null(TCalib)) { 
		aa <- "Calib: Full Range"
		sfx <- ""
	} else { 
		aa <- paste("Calib: ", paste(TCalib, collapse="-"), sep="")
		sfx <- " deg. C. "
	}
	if (mod == "classic" | mod == "classic-diff") {
		aa <- sfx <- ""
	}
	if (mod == "aucs.tn" | mod == "aucs.tn-diff" | mod == "aucs.tn.dce" | mod == "aucs.tn.dce-diff") {
		texp <- paste("T norm.: ", Texp, " deg. C", sep="")
	} else {
		if (mod == "aucs" | mod == "aucs-diff" | mod == "aucs.dce" | mod == "aucs.dce-diff") {
			texp <- paste("T Exp.: ", Texp, " deg. C", sep="")		
		} else {
			texp <- ""
		}
	}
	r1 <- paste("Mode: ", mod, sep="")
	r2 <- diffText
#	r3 <- paste(paste(aa, collapse="-"), sfx, sep="")
	r3 <- paste(aa, sfx, sep="")
	r4 <- texp
#	legTextMod <- paste("Mode: ", mod, "\n", diffText, "T-Calib: ", paste(aa, collapse="-"), sfx, sep="")
	legTextMod <- c(r1, r2, r3, r4)
} #EOF

aq_checkSelWls <- function(mod, selWls) {
	if ((mod == "aucs") | (mod == "aucs-diff") | (mod == "aucs.tn") | (mod == "aucs.tn.dce") | (mod == "aucs.tn-diff") | (mod == "aucs.tn.dce-diff") | (mod == "aucs.dce") | (mod == "aucs.dce-diff") ) {
	out <- paste("C", getOvertoneColnames(.ap2$stn$aqg_OT), sep="")
	} else {
		out <- selWls
	}
	out
} # EOF

aq_checkPlotType <- function(mod) {
	plotType <- .ap2$stn$aqg_plottingType
	if (grepl(pv_AquagramModes[1], mod)) {
		plotType <- "circular"
	}
	if (!any(c("circular", "linear") %in% plotType)) {
		stop("Please provide either 'linear' or 'circular' to the argument 'aqg_plottingType' in the settings file.", call.=FALSE)
	}
	return(plotType)
}

aq_makePolygons <- function(plotData, legendColor) {
	if (!is.null(plotData)) {
		alpha <- .ap2$stn$aqg_plot_color_alpha_CIfill
		#
		xfwd <- seq(1, ncol(plotData))
		xrev <- rev(xfwd)
		xx <- c(xfwd, xrev)
		for (i in 1: (nrow(plotData)/3) ) {
			rind <- c(i*3+1, i*3+2, i*3+3) - 3 # always get the 3er groups
			curr <- plotData[rind,]
			lower <- curr[2,]
			upper <- curr[3, ]
			polygon(xx, c(lower, rev(upper)), col=makeColorsTransparent(legendColor[i], alpha), border=FALSE)
		} # end for i
	} # end !is.null(plotData)
} # EOF

aq_plotCore_sigTable <- function(aquCalc) {
	plotSig <- .ap2$stn$aqg_plotSigTable
	#
	if (plotSig & !is.null(aquCalc@ciTable)) {
		sigTable <- aquCalc@ciTable
		cns <- colnames(sigTable)
		rnsSig <- rownames(sigTable)
		for (i in 1: ncol(sigTable)) {
			sigTable[,i] <- as.character(sigTable[,i])
		}
		colnames(sigTable) <- cns
		rownames(sigTable) <- rnsSig
		#
		avgTable <- as.data.frame(round(aquCalc@avg, 2))
		rnsAvg <- rownames(avgTable)
		for (i in 1: ncol(avgTable)) {
			avgTable[,i] <- as.character(avgTable[,i])
		}
		colnames(avgTable) <- cns
		rownames(avgTable) <- rnsAvg
		#
		sep <- sigTable[1,,drop=FALSE]
		sep[1,] <- rep("---", ncol(avgTable))
		rownames(sep) <- ""
		colnames(sep) <- colnames(sigTable)
		plotThis <- rbind(sigTable, sep, avgTable)
		# now please plot it !!
		plot.new() # to make a new page
		gridExtra::grid.table(plotThis, theme=gridExtra::ttheme_default(base_size=8, padding=grid::unit(c(4,2), "mm")))
	} # end if plot Sig
} # EOF

plot_aquagram_inner <- function(aquCalc, selWls=.ap2$stn$aqg_wlsAquagram, onSub, onMain, where, customColor, nrCorr, bootCI, mod, TCalib, minus, Texp, masterScaleAQ, masterScaleBoot, clt=NULL, R) {
	if (!is.numeric(selWls)) {
		stop("Please provide a numerical vector as input for the wavelengths. Thank you.", call.=FALSE)
	}
	plotType <- aq_checkPlotType(mod)
	pdfSizeAdd <- .ap2$stn$aqg_plot_pdfSizeAdd
	height <- .ap2$stn$pdf_Height_sq + pdfSizeAdd
	width <- .ap2$stn$pdf_Width_ws + pdfSizeAdd
	xAxisTitle <- .ap2$stn$aqg_linearXaxisTitle
	legCex <- .ap2$stn$aqg_plot_linear_legendCex
	plotWamacsLines <- .ap2$stn$aqg_plotWamacsLines
	alwaysPlotAvgAqg <- .ap2$stn$aqg_alwaysPlotAvgAqg
	doPlotAvg <- TRUE
	maxElmsPerCol <- .ap2$stn$aqg_plot_maxNrLegendElements # the max number of elements in one column
	#
	if (bootCI & !is.null(aquCalc@bootRes) & !alwaysPlotAvgAqg) { 
		doPlotAvg <- FALSE
	}
	onSubLinear <- onSub
	Color <- aq_checkColors(aquCalc@colRep, customColor)
	a <- aq_makeGraphicsTexts(onSub, aquCalc@possN, nrCorr)
		onSub <- a$onSub
		mText <- a$mText
	#################
	getNrOfLegCols <- function(X) {
		out <- 1
		if (nrow(X) > 1* maxElmsPerCol) { out <- 2 }
		if (nrow(X) > 2* maxElmsPerCol) { out <- 3 }
		return(out)
	} # EOIF
	#################
	aq_plotCore_circ <- function(dfpList) {
		dataForPlotting <- dfpList$Data
		ncLeg <- getNrOfLegCols(dataForPlotting)
		caxislabels <- dfpList$Labels
		if(where != "pdf" & Sys.getenv("RSTUDIO") != 1)  {dev.new(height=height, width=width)}
		fmsb::radarchart(dataForPlotting, axistype=4, maxmin=T, axislabcol=1, seg=4, pty=32, caxislabels=caxislabels, pcol=Color, plty=ltPlot, cglwd=0.5, plwd=pLineWi, centerzero=T, cglty=3, sub=onSub, title=onMain)
		legBgCol <- rgb(255,255,255, alpha=.ap2$stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings
		legend("topright", cex=0.8, xjust=0.5, yjust=0.5, legend=legText, col=legColor, lty=ltLeg, lwd=4, bg=legBgCol, ncol=ncLeg)
		legend("bottomleft", cex=0.8, xjust=0.5, yjust=0.5, legend=legTextMod, bg=legBgCol)
		mtext(mText, 1)
	} # EOIF
	########
	aq_plotCore_linear <- function(linData, legTextMod, curYlim, inBoot=FALSE) { #### CORE ####
		yLabMod <- linData$yLab
		onMain <- linData$onMain
		onSub <- linData$onSub
		TexpLine <- linData$TexpLine
		plotData <- linData$plotData
		dataColor <- linData$dataColor
		legendColor <- linData$legendColor
		legTxt <- linData$legTextExt
		legTitle <- linData$legTitle
		ltPlot <- linData$ltPlot
		ltLeg <- linData$ltLeg
		lwd <- linData$lwd
		ncLeg <- getNrOfLegCols(plotData)
		#
		if(where != "pdf" & Sys.getenv("RSTUDIO") != 1)  {dev.new(height=height, width=width)}
		matplot(t(plotData), type="l", xaxt="n", lty=ltPlot, col=dataColor, ylab=yLabMod, xlab=xAxisTitle, ylim=curYlim, main=onMain, sub=onSub, cex.main=0.8, cex.sub=0.8, lwd=lwd) # masterScaleAQ can be NULL
		axis(1, at=seq(1, ncol(plotData)), labels=colnames(plotData))
		if (inBoot) {	
			aq_makePolygons(plotData, legendColor)
		}
		abline(h=TexpLine, col="gray", lwd=0.6) 
		if (plotWamacsLines) {
			abline(v=seq(1, ncol(plotData)), col="lightgray", lwd=0.4)
		}
		legBgCol <- rgb(255,255,255, alpha=.ap2$stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings
		legend("topright", legend=legTxt, title=legTitle, cex=legCex, col=legendColor, lty=ltLeg, bg=legBgCol, ncol=ncLeg)
		legend("bottomleft", legend=legTextMod, cex=legCex, bg=legBgCol)
	} # EOIF		
	#################
	## here always plot the selected aquagram, no fancy CI
	legTextMod <- aq_checkLegendTextMod(mod, minus, TCalib, Texp)
	## for circular only
	legColor <- Color
	selWls <- aq_checkSelWls(mod, selWls)
	standardData <- aq_makeNicePlottingFrame_circ(aquCalc@avg, selWls, masterScaleAQ)
	legText <- rownames(standardData$Data)[-c(1,2)]	
	## \end for circ only
	if (doPlotAvg) {
		if (plotType == "circular") {
			if (is.numeric(clt)) {
				ltPlot <- ltLeg <- clt
			} else {
				ltPlot <- ltLeg <- .ap2$stn$aqg_linetypes
			}
			pLineWi <- 2.3
	 		aq_plotCore_circ(standardData)
		} else { # so we want to plot linear !! :-)
			linData <- aq_makeNicePlottingFrame_linear(aquCalc, onMain, onSubLinear, mod, Texp, customColor, clt, R, minus, inBoot=FALSE)
			aq_plotCore_linear(linData, legTextMod, curYlim=masterScaleAQ, inBoot=FALSE)
		} # end else
	} # end if doPlotAvg
	##
	if (bootCI) {
		if (!is.null(aquCalc@bootRes)) {
			if (plotType == "circular") {
				if (mod == "aucs-diff" | mod == "sfc-diff" | mod == "classic-diff") {
					Color <- Color[ -(which(rownames(aquCalc@avg) == minus)) ]
				}
				Color <- rep(Color, each=3)
				ltPlot <- c(1,3,2)
				ltLeg <- 1		## ( the legend text stays the same as above)
				pLineWi <- c(1.8, 1.2, 1.2)
				onSub <- paste(onSub, " 95% CI based on", R, "bootstrap replicates (bca)")			
				ciData <- aq_makeNicePlottingFrame_circ(aquCalc@bootRes, selWls, masterScaleBoot)
			 	aq_plotCore_circ(ciData)
			 	aq_plotCore_sigTable(aquCalc)
			} else { # so we want to plot linear !! :-)
				linData <- aq_makeNicePlottingFrame_linear(aquCalc, onMain, onSubLinear, mod, Texp, customColor, clt, R, minus, inBoot=TRUE)
				aq_plotCore_linear(linData, legTextMod, curYlim=masterScaleBoot, inBoot=TRUE)
			 	aq_plotCore_sigTable(aquCalc)
			}
		} # end if !is.null bootRes
	} # end if bootCI	
} # EOF

plotRawSpectra <- function(rawSpectra, onSub, onMain, nrCorr, possNrPartic, ncpwl) {
	a <- ncpwl
	cns <- colnames(rawSpectra$NIR)
	wls <- as.numeric(substr(cns, a+1, nchar(cns)))
	onSub <- paste("Raw Aquagram Spectra", onSub)
	possNrPart <- paste(possNrPartic, collapse=", ")
	if (nrCorr) {
		eachN_msg <- paste(" (each N=", min(possNrPartic), ")", sep="")
		if (diff(range(possNrPartic)) == 0 ) {
			mText <- ""
		} else {
			mText <- paste("Max. N = ", possNrPart , "\n", eachN_msg, sep="")
		}
	} else {
		if (diff(range(possNrPartic)) == 0 ) {
			eachN_msg <- paste(" (each N=", min(possNrPartic), ")", sep="")
			mText <- ""
		} else {
			eachN_msg <-  " (not N corr.)"
			mText <- paste("N=", possNrPart, collapse=", ")
		}
	}
	onSub <- paste(onSub, eachN_msg, sep="")
	Color <- rawSpectra$colRep
	if (!is.numeric(Color)) {
		Color <- as.character(Color)
	} 
#	legText <- unique(rownames(rawSpectra))
#	legColor <- unique(Color)
	matplot(wls, t(rawSpectra$NIR), type="l", lty=1, col=Color, xlab="Wavelength", ylab="avg Absorbance", sub=onSub, main=onMain)
	abline(h=0, col="gray")
#	legend("topright", legend=legText, col=legColor, lwd=2.5, lty=1)
	mtext(mText, 4)
} # EOF

plotAvgSpectra <- function(avgSpectra, onSub, onMain, nrCorr, possNrPartic, ncpwl) {
	a <- ncpwl
	cns <- colnames(avgSpectra$NIR)
	wls <- as.numeric(substr(cns, a+1, nchar(cns)))
	onSub <- paste("Averaged Aquagram Spectra", onSub)
	possNrPart <- paste(possNrPartic, collapse=", ")
	if (nrCorr) {
		eachN_msg <- paste(" (each N=", min(possNrPartic), ")", sep="")
		if (diff(range(possNrPartic)) == 0 ) {
			mText <- ""
		} else {
			mText <- paste("Max. N = ", possNrPart , "\n", eachN_msg, sep="")
		}
	} else {
		if (diff(range(possNrPartic)) == 0 ) {
			eachN_msg <- paste(" (each N=", min(possNrPartic), ")", sep="")
			mText <- ""
		} else {
			eachN_msg <-  " (not N corr.)"
			mText <- paste("N=", possNrPart, collapse=", ")
		}
	}
	onSub <- paste(onSub, eachN_msg, sep="")
	Color <- avgSpectra$colRep
	if (!is.numeric(Color)) {
		Color <- as.character(Color)
	} 
	matplot(wls, t(avgSpectra$NIR), type="l", lty=1, col=Color, xlab="Wavelength", ylab="avg Absorbance", sub=onSub, main=onMain)
	abline(h=0, col="gray")
	legBgCol <- rgb(255,255,255, alpha=.ap2$stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings
	legend("topright", legend=rownames(avgSpectra), col=Color, lwd=2.5, lty=1, bg=legBgCol)
	mtext(mText, 4)
} # EOF

plotSubtrAvgSpectra <- function(subtrSpectra, onSub, onMain, nrCorr, possNrPartic, adPeakPlot=FALSE, adLines=FALSE, discrim=FALSE, itemIndex=NULL, minus, ranSubtrSpec, ncpwl) {
	a <- ncpwl
	cns <- colnames(subtrSpectra$NIR)
	wls <- as.numeric(substr(cns, a+1, nchar(cns)))
	onSub <- paste("Aquagram Subtraction Spectra", onSub)
	possNrPart <- paste(possNrPartic, collapse=", ")
	if (nrCorr) {
		eachN_msg <- paste(" (each N=", min(possNrPartic), ")", sep="")
		if (diff(range(possNrPartic)) == 0 ) {
			mText <- ""
		} else {
			mText <- paste("Max. N = ", possNrPart, "\n", eachN_msg, sep="")
		}
	} else {
		if (diff(range(possNrPartic)) == 0 ) {
			eachN_msg <- paste(" (each N=", min(possNrPartic), ")", sep="")
			mText <- ""
		} else {
			eachN_msg <-  " (not N corr.)"
			a <- paste(strsplit(possNrPart, ",")[[1]][-itemIndex], collapse=",")
			substr(a, 1, 1) <- ""
			mText <- paste("N=", a, collapse=", ")
		}
	}
	onSub <- paste(onSub, eachN_msg, sep="")
	Color <- subtrSpectra$colRep
	if (!is.numeric(Color)) {
		Color <- as.character(Color)
	}
	legendText <- paste("Minus:", minus)
	matplot(wls, t(subtrSpectra$NIR), type="l", lty=1, col=Color, xlab="Wavelength", ylab="delta avg Absorbance", sub=onSub, main=onMain, ylim=ranSubtrSpec)
	abline(h=0, col="gray")
	legBgCol <- rgb(255,255,255, alpha=.ap2$stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings
	legend("topright", legend=rownames(subtrSpectra), col=Color, lwd=2.5, lty=1, bg=legBgCol)
	legend("bottomleft", legend=legendText, bg=legBgCol)
	mtext(mText, 4)
	if (adPeakPlot) {
		bw <- .ap2$stn$pp_bandwidth
		NIRnice <- as.data.frame(matrix(subtrSpectra$NIR, nrow=nrow(subtrSpectra$NIR)))
		colnames(NIRnice) <- colnames(subtrSpectra$NIR)
		rownames(NIRnice) <- rownames(subtrSpectra)
		NIR <- as.data.frame(t(NIRnice))
		pickResults <- pickPeaks(NIR, bandwidth=bw, comps=NULL, discrim, wavelengths=wls)
		plotPeaks(pickResults, onMain, onSub, adLines, pcaVariances=NULL, customColor=Color, ylim=ranSubtrSpec, wavelengths=wls, clty=NULL)
	}
} # EOF

aq_getFixScaleText <- function(fsa, fss) {
	if (!is.logical(fsa) | !is.logical(fss)) {
		if (all(fsa == "both") | all(fss == "both")) {
			fsText <- "_ind+fixScale"
		} else {
			if (all(fsa == "only") | is.numeric(fsa) | all(fss=="only") | is.numeric(fss)) {
				fsText <- "_fixScale"
			}
		}
	} else {
		fsText <- ""
	}
	return(fsText)
} # EOF

aq_getMinusText <- function(minus, mod) {
	if (is.null(minus)) {
		minusText <- ""
	} else {
		if (grepl("diff", mod)) {
			minusText <- paste("_min.", minus, sep="")
		} else {
			minusText <- ""			
		}
	}
	return(minusText)
} # EOF

plotAquagram_single <- function(aquCalc, classVarRanges, where, onSub, onMain, customColor, plotSpectra, adPeakPlot, adLines, discrim, clt, mod, TCalib, Texp, selWls, nrCorr, bootCI, minus, fsa, fss, R, ncpwl, setIdString, ap) {
#	classVarRanges: a list with one element for each kind of range throughout the whole set (within a singel classVar)
#	idString <- getIdString(aquCalc)
	idString <- adaptIdStringForDpt(ap, setIdString)
	classVar <- getClassVar(aquCalc)
	itemIndex <- getItemIndex(aquCalc)
	onMain <- paste(onMain, ", ", idString, sep="")
	textForSub <- paste(onSub, " grouping by ", classVar, sep="")
	ranAvg <- list(classVarRanges$ranAvg)
	ranBootRes <- list(classVarRanges$ranBootRes)
	ranSubtrSpec <- list(classVarRanges$ranSubtrSpec)
	##
	if (is.numeric(fsa)) {
		ranAvg <- list(fsa) 			## here we take the fix scale provided by the user
		ranBootRes <- list(fsa)
	}
	if (is.numeric(fss)) {
		ranSubtrSpec <- list(fss)
	}
	cntAqu <- cntSpec <- 1
	if (all(fsa == "both")){
		cntAqu <- 2
		ranAvg <- c(list(NULL), ranAvg)
		ranBootRes <- c(list(NULL), ranBootRes)
	}
	if (all(fss == "both")) {
		cntSpec <- 2
		ranSubtrSpec <- c(list(NULL), ranSubtrSpec)
	}
	##
	if (all(fsa == FALSE) & !is.logical(fss)) {
		ranAvg <- NULL
		ranBootRes <- NULL
	}
	if (all(fss == FALSE) & !is.logical(fsa)) {
		ranSubtrSpec <- NULL
	}
	##
	for (i in 1: cntAqu) {
		plot_aquagram_inner(aquCalc, selWls, onSub=textForSub, onMain, where, customColor, nrCorr, bootCI, mod, TCalib, minus, Texp, masterScaleAQ=ranAvg[[i]], masterScaleBoot=ranBootRes[[i]], clt, R)
	} # end for i
	##
	if( any(c("raw", "all") %in% plotSpectra) ) {
		plotRawSpectra(aquCalc@rawSpec, onSub=textForSub, onMain, nrCorr, aquCalc@possN, ncpwl)		
	}
	if( any(c("avg", "all") %in% plotSpectra) ) {
		plotAvgSpectra(aquCalc@avgSpec, onSub=textForSub, onMain, nrCorr, aquCalc@possN, ncpwl)
	}
	if( any(c("subtr", "all") %in% plotSpectra) ) {
		for (k in 1: cntSpec){
			plotSubtrAvgSpectra(aquCalc@subtrSpec, onSub=textForSub, onMain, nrCorr, aquCalc@possN, adPeakPlot, adLines, discrim, itemIndex, minus, ranSubtrSpec[[k]], ncpwl)		
		} # end for k
	}
} # EOF

aq_checkTrippleDotsCalc <- function(...) {
	a <- substitute(c(...))
	chars <- names(eval(a))
	if ( any(chars %in% pv_modifyAquagram_calc) ) {
		stop("Sorry, you can not provide aquagram calculation arguments to the plotting function.", call.=FALSE)
	}
} # EOF

# new below --------------------------------
#' @title Plot Aquagram
#' @description Plot Aquagram XXX.
#' @details The width and height of the resulting pdf can be set in the settings.
#' @param cube An object of class 'aquap_cube' as produced by \code{\link{gdmm}}.
#' @param ... Optional 'aqg' plotting parameters to override the values in the 
#'  analysis procedure - for possible arguments see 
#'  \code{\link{plot_aqg_args}}.
#' @param aps Character length one. The default way to obtain the analysis 
#' procedure containing the aquagram \strong{plotting} parameters. Defaults to 
#' "def". Possible values are:
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
#' }
#' @return A pdf or graphic device.
#' @family Plot functions
#' @family Aquagram documentation
#' @examples
#'  \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset)
#'  plot(cube)
#'  plot_aqg(cube)
#'  plot_aqg(cube, aps="fooBar.r") # obtain the analysis procedure with the 
#'  # plotting parameters from the file 'fooBar.r'.
#'  }
#' @export
plot_aqg <- function(cube, aps="def", ...) {
  	autoUpS()
  	if (class(cube) != "aquap_cube") {
  		stop("Please provide an object of class 'aquap_cube' to the argument 'cube'", call.=FALSE)
  	}
  	aq_checkTrippleDotsCalc(...) # to prevent the user to provide a calculation argument to the plotting function
	ap <- doApsTrick(aps, cube, ...)
  	ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset=getDataset(cube[[1]]), haveExc=FALSE) # just take the first dataset, as we mainly need the header (and the wavelengths are already checked.. )
  	apCube <- getap(.lafw_fromWhere="cube", cube=cube)	
  	if (is.null(ap$aquagr)) {
  	 	return(cat("*** Aquagram not available or not selected for plotting \n"))
 	}
  	where <- ap$genPlot$where
  	fns <- ap$genPlot$fns
 	#
  	aq <- ap$aquagr
  	plotType <- aq_checkPlotType(aq$mod) # stops if plot type is neither circular nor linear
  	if (!is.logical(ap$aquagr$spectra)) {add <- "+spectra"; suffix <- "aquagr+Spect" } else {add <- ""; suffix <- "aquagr"}
  	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat(paste("Plotting Aquagrams", add, "... ", sep="")) }
	pdfSizeAdd <- .ap2$stn$aqg_plot_pdfSizeAdd
 	if (plotType == "circular") {
 	 	height <-.ap2$stn$pdf_Height_sq
  		width <- .ap2$stn$pdf_Width_sq
  	} else {
 	 	height <-.ap2$stn$pdf_Height_ws + pdfSizeAdd
  		width <- .ap2$stn$pdf_Width_ws + pdfSizeAdd
  	}
  	path <- .ap2$stn$fn_results
  	expName <- getExpName(cube)
  	minusText <- aq_getMinusText(aq$minus, aq$mod)	
  	filename <- paste(expName, suffix, sep="_")
  	filename <- paste(path, "/", filename, fns, "_", aq$mod, minusText, aq_getFixScaleText(aq$fsa, fss=FALSE), ".pdf", sep="")
  	ap$genPlot$onMain <- paste(expName, " ", ap$genPlot$onMain, sep="")
  	###
 	###
  	if (where == "pdf") {
  		pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12)
	}
	ncpwl <- getNcpwl(getDataset(cube[[1]])) # all are the same
	a <- ap$aquagr
	aC <- apCube$aquagr # to make it impossible to change the calculated values, the info depending on the calculation etc.
	b <- ap$genPlot
	for (va in 1: length(aC$vars)) { # was length(ap$aquagr$vars)
		for (cu in 1: length(cube)) {
			plotAquagram_single(getAqgResList(cube[[cu]])[[va]], cube@aqgRan[[va]], where, b$onSub, b$onMain, a$ccol, a$spectra, a$pplot, a$plines, a$discr, a$clt, aC$mod, aC$TCalib, aC$Texp, aC$selWls, aC$nrCorr, aC$bootCI, aC$minus[va], a$fsa, a$fss, aC$R, ncpwl, getIdString(cube[[cu]]), ap=apCube)
		} # end for i
	} # end for va
	if (where == "pdf") {dev.off()}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF


#' @title Plot Aquagram - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{plot}} and \code{\link{plot_aqg}} to override the values 
#' in the analysis procedure file and so to modify the graphics - see examples.
#' 
#' \code{plot(cube, ...)}
#' 
#' \code{plot_aqg(cube, ...)}
#' 
#' @template mr_details_allParams
#' @template mr_aqg_plot_param
#' @template mr_pg_genParams
#' @seealso \code{\link{plot_aqg}}
#' @examples
#' \dontrun{
#' dataset <- gfd()
#' cube <- gdmm(dataset)
#' plot(cube)
#' }
#' @family Plot arguments
#' @family Aquagram documentation
#' @name plot_aqg_args
NULL



#' @title Calculate Aquagram - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{getap}}, also within function \code{\link{gdmm}}, to 
#' override the values in the analysis procedure file and so to modify the 
#' calculation of Aquagrams - see examples.
#' 
#' \code{getap(...)}
#'  
#' \code{gdmm(dataset, ap=getap(...))}
#' 
## @section Note: Calculation of PLSR models is done with the function \code{\link[pls]{plsr}}. 
#' @references XXX hopefully a nice reference to the Aquagram paper!! XXX
#' @template mr_details_allParams
#' @template mr_aqg_calc_param
#' @seealso \code{\link{gdmm}}
#' @examples
#' \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset, ap=getap())
#'  cube <- gdmm(dataset, ap=getap())
#' }
#' @family Calc. arguments
#' @family Aquagram documentation
#' @name calc_aqg_args
NULL



#' @title Calculate Aquagram - Modes
#' @description The following values can be provided to the argument \code{aqg.mod} 
#' in \code{getap}, specifiying what type of Aquagram should be calculated.
#' @details XXX
#' @section Possible Values: Possible Aquagram modes are
#' \itemize{
#' \item \code{classic}: The classical aquagram - smoothed, MSC, normalized
#' \item \code{classic-diff}: same as above + one group subtracted from all the
#' others. 
#' \item \code{sfc}: "Scaled, foreign center": smoothed, MSC, centered on the mean of
#' selected calibration data, scaled. 
#' \item \code{sfc-diff}: same as above + one group subtracted from all the others.
#' \item \code{aucs}: "Area Under Curve Stabilized": the area under the curve (auc)
#' from the spectra in all the 12/15 coordinates in a set of calibration data
#' at varying temperature is calculated, then divided by the total auc. Then
#' the smalles and biggest value is taken as 0% and 100%. The same auc is
#' calculated for every sample, and its value in percent ... XXX 
#' \item \code{aucs-diff}: same as above + one group subtracted from all the others. 
#' \item \code{aucs.tn}: aucs + temperature normalization: .... XXX the auc from a
#' sample at Texp gets subtracted from all the auc of the samples. 
#' \item \code{aucs.tn-diff}: same as above + one group subtracted from all the
#' others
#' \item \code{aucs.tn.dce}: same as aucs.tn, but the scale calculated to show
#' degrees celsius equivalent
#' \item \code{aucs.tn.dce-diff}: same as above + one group subtracted from all the
#' others
#' \item \code{aucs.dce}: same as aucs, but the scale calculated to show degrees
#' celsius equivalent
#' \item \code{aucs.dce-diff}: same as above + one group subtracted from all the
#' others
#' }
#' @seealso \code{\link{calc_aqg_args}}, for recording your own temperature 
#' calibration spectra: \code{\link{genTempCalibExp}}
#' @references XXX the Aquagram paper XXX
#' @name calc_aqg_modes
NULL

