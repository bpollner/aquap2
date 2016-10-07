# extract some error values ----------------------------------------
getRMSEC <- function(plsModel) {
	a <- c(pls::RMSEP(plsModel, estimate="train")$val)
	b <- a[length(a)]
	out <- round(b, .ap2$stn$plsr_nrDigitsRMSEx)
} #EOF

getRMSECV <- function(plsModel) {
	a <- c(pls::RMSEP(plsModel, estimate="CV")$val)
	b <- a[length(a)]
	out <- round(b, .ap2$stn$plsr_nrDigitsRMSEx)
} #EOF

getRMSEP <- function(plsModel, newdata) {
	a <- c(pls::RMSEP(plsModel, estimate="test", newdata=newdata)$val)
	b <- a[length(a)]
	out <- round(b, .ap2$stn$plsr_nrDigitsRMSEx)
} # EOF

getR2C <- function(plsModel) {
	a <- c(pls::R2(plsModel, estimate="train")$val)
	b <- a[length(a)]
	out <- round(b, .ap2$stn$plsr_nrDigitsRMSEx)
} #EOF

getR2CV <- function(plsModel) {
	a <- c(pls::R2(plsModel, estimate="CV")$val)
	b <- a[length(a)]
	out <- round(b, .ap2$stn$plsr_nrDigitsRMSEx)
} #EOF

getR2P <- function(plsModel, newdata) {
	a <- c(pls::R2(plsModel, estimate="test", newdata=newdata)$val)
	b <- a[length(a)]
	out <- round(b, .ap2$stn$plsr_nrDigitsRMSEx)
} # EOF
####
getVecRMSEC <- function(plsModel) {			## for error plot
	out <- (c(pls::RMSEP(plsModel, estimate="train", intercept=TRUE)$val[,1,]))
} # EOF

getVecRMSECV <- function(plsModel) {		## for error plot
	out <- (c(pls::RMSEP(plsModel, estimate="CV", intercept=TRUE)$val[,1,]))
} # EOF

getVecRMSECV_adj <- function(plsModel) {	## for error plot
	out <- (c(pls::RMSEP(plsModel, estimate="adjCV", intercept=TRUE)$val[,1,]))
} # EOF
####
convertToRDP <- function(errorValue, ClassVar, header) {	# the dataset with all the observations that form the range	
	sdY <- sd(header[,ClassVar], na.rm=TRUE)
	 out <- round(sdY/errorValue, .ap2$stn$plsr_nrDigitsRMSEx)
} # EOF

# plotting ----------------------------------------
plot_plsr_error <- function(plsModel, plsPlusModel, dataset, ClassVar, onMain="", onSub="", inRDP=FALSE) { # ClassVar = regrOn
	modCorrCol <- .ap2$stn$plsr_colorForBestNumberComps
	header <- getHeader(dataset)
	## the correct model
	vecRMSEC <- getVecRMSEC(plsModel)
	vecRMSECV <- getVecRMSECV(plsModel)
	vecRMSECV_adj <- getVecRMSECV_adj(plsModel)
	vecsCorr <- data.frame(RMSEC=vecRMSEC, RMSECV=vecRMSECV, RMSECV_adj=vecRMSECV_adj)
	## the plus model
	vecRMSEC_plus <- getVecRMSEC(plsPlusModel)
	vecRMSECV_plus <- getVecRMSECV(plsPlusModel)
	vecRMSECV_adj_plus <- getVecRMSECV_adj(plsPlusModel)
	vecsPlus <- data.frame(RMSEC=vecRMSEC_plus, RMSECV=vecRMSECV_plus, RMSECV_adj=vecRMSECV_adj_plus)
	#
	ylab= "error value"
	legPos="topright"
	if (inRDP) {
		sdY <- sd(header[,ClassVar], na.rm=TRUE)
		## the correct model
		vecRMSEC <- sdY/vecRMSEC
		vecRMSECV <- sdY/vecRMSECV
		vecRMSECV_adj <- sdY/vecRMSECV_adj
		vecsCorr <- data.frame(RMSEC=vecRMSEC, RMSECV=vecRMSECV, RMSECV_adj=vecRMSECV_adj)
		## the plus model
		vecRMSEC_plus <- sdY/vecRMSEC_plus
		vecRMSECV_plus <- sdY/vecRMSECV_plus
		vecRMSECV_adj_plus <- sdY/vecRMSECV_adj_plus
		vecsPlus <- data.frame(RMSEC=vecRMSEC_plus, RMSECV=vecRMSECV_plus, RMSECV_adj=vecRMSECV_adj_plus)
		ylab <- "error value [RDP]"
		legPos <- "topleft"
	}
	regrOnMsg <- paste("   regr. on: ", ClassVar, "   ",sep="")
	ncompMsg <- paste("   ", plsModel$ncomp, " comps.", sep="")
	Nmsg <- paste("   N=", nrow(header), sep="")
	subText <- paste(onSub, regrOnMsg, ncompMsg, Nmsg, sep="")
	xax <- 0:plsPlusModel$ncomp
	yax <- vecsPlus
#	xax <- 0:plsModel$ncomp
#	yax <- vecsCorr
	matplot(xax, yax, type="l", xlab="Nr of components", ylab=ylab, main=onMain, sub=subText)
	abline(v=plsModel$ncomp, col=modCorrCol, lwd=0.4)
	legBgCol <- rgb(255,255,255, alpha=.ap2$stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings
	legend(legPos, legend=c("RMSEC", "RMSECV", "RMSECV_adj"), col=1:3, lty=1:3, bg=legBgCol)
} # EOF

plot_plsr_calibration_classic <- function(plsModel, dataset, regrOn, classFCol, onMain="", onSub="",  inRDP=FALSE) {
	colLm <- .ap2$stn$plsr_color_lm_training
	ltTarg <- .ap2$stn$plsr_linetypeTargetLine
	ltLm <- .ap2$stn$plsr_linetypeLinearModel
	#
	header <- getHeader(dataset)
	if (is.null(classFCol)) {
		color <- 1
		colorMsg <- ""
		colLegend <- FALSE
	} else {
		clv <- extractColorLegendValues(dataset, groupBy=classFCol) # returns a list: color_data, color_unique, color_legend, txt, txtE, sumPart, dataGrouping, pch_data, pch_legend
		color <- clv$color_data
		colorMsg <- " color by: "
		colLegend <- TRUE
	}
	RMSEC <- getRMSEC(plsModel)
	RMSEC_rdp <- convertToRDP(RMSEC, regrOn, header)
	R2C <- getR2C(plsModel)
	ncomp <- plsModel$ncomp
	yvar <- plsModel$model$yvar
	yvarFitted <- plsModel$fitted.values[ , , ncomp]
	regrOnMsg <- paste("   regr. on: ", regrOn, "   ",sep="")
	ncompMsg <- paste("   ", ncomp, " comps.", sep="")
	Nmsg <- paste("   N=", nrow(header), sep="")
	subText <- paste(onSub, regrOnMsg, colorMsg, classFCol, ncompMsg, Nmsg, sep="")
	pls::predplot(plsModel, which="train" , main=paste(onMain, "- Training"), sub=subText, col=color)
	abline(0,1, col="gray", lty=ltTarg, lwd=1)
	abline(lm(yvarFitted ~ yvar), lty=ltLm, lwd=1, col=colLm) # fitting linear model
	if (inRDP) {
		legendText <- paste("RMSEC: ", RMSEC, "\nRMSEC[RDP]: ", RMSEC_rdp, "\nR2C: ", R2C, "\n\n ", sep="")
	} else {
		legendText <- paste("RMSEC: ", RMSEC, "\nR2C: ", R2C, sep="")
	}
	legend("topleft", legend=legendText)
	if (colLegend) {
		legBgCol <- rgb(255,255,255, alpha=.ap2$stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings
		legend("bottomright", legend=clv$txtE, col=clv$color_legend, pch=16, bg=legBgCol)
	}
} # EOF

plot_plsr_validation_classic <- function(plsModel, dataset, regrOn, classFCol, onMain="", onSub="", inRDP=FALSE, valid="") {
	colLm <- .ap2$stn$plsr_color_lm_crossvalid
	ltTarg <- .ap2$stn$plsr_linetypeTargetLine
	ltLm <- .ap2$stn$plsr_linetypeLinearModel
	#
	header <- getHeader(dataset)
	if (is.null(classFCol)) {
		color <- 1
		colorMsg <- ""
		colLegend <- FALSE
	} else {
		clv <- extractColorLegendValues(dataset, groupBy=classFCol) # returns a list: color_data, color_unique, color_legend, txt, txtE, sumPart, dataGrouping, pch_data, pch_legend
		color <- clv$color_data
		colorMsg <- " color by: "
		colLegend <- TRUE
	}	
	RMSECV <- getRMSECV(plsModel)
	RMSECV_rdp <- convertToRDP(RMSECV, regrOn, header)
	R2CV <- getR2CV(plsModel)
	ncomp <- plsModel$ncomp
	yvar <- plsModel$model$yvar
	yvarFitted <- plsModel$validation$pred[ , , ncomp]	
	regrOnMsg <- paste("   regr. on: ", regrOn, "   ",sep="")
	ncompMsg <- paste("   ", ncomp, " comps.", sep="")
	Nmsg <- paste("   N=", nrow(header), sep="")
	mainText <- paste(onMain, " - Validation (", valid, ")", sep="")
	subText <- paste(onSub, regrOnMsg, colorMsg, classFCol, ncompMsg, Nmsg, sep="")
	pls::predplot(plsModel, which="validation" , main=mainText, sub=subText, col=color)
	abline(0,1, col="gray", lty=ltTarg, lwd=1)
	abline(lm(yvarFitted ~ yvar), lty=ltLm, lwd=1, col=colLm) # fitting linear model	
	if (inRDP) {
		legendText <- paste("RMSECV: ", RMSECV, "\nRMSECV[RDP]: ", RMSECV_rdp, "\nR2CV: ", R2CV, "\n\n ", sep="")
	} else {
		legendText <- paste("RMSECV: ", RMSECV, "\nR2CV: ", R2CV, sep="")
	}
	legend("topleft", legend=legendText)	
	if (colLegend) {
		legBgCol <- rgb(255,255,255, alpha=.ap2$stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings	
		legend("bottomright", legend=clv$txtE, col=clv$color_legend, pch=16, bg=legBgCol)
	}
} # EOF

plot_plsr_calibValidSwarm <- function(plsModel, dataset, regrOn, classFCol, onMain="", onSub="", inRDP=FALSE, valid="", psd) { ##### CORE ###
	colLmTrain <- .ap2$stn$plsr_color_lm_training
	colLmCV <- .ap2$stn$plsr_color_lm_crossvalid
	ltTarg <- .ap2$stn$plsr_linetypeTargetLine
	ltLm <- .ap2$stn$plsr_linetypeLinearModel
	plotSwarm <- .ap2$stn$plsr_plotDataInSwarm
	secAlpha <- .ap2$stn$plsr_color_alpha_secondaryData
	pchPrim <- .ap2$stn$plsr_color_pch_primaryData
	pchSec <- .ap2$stn$plsr_color_pch_secondaryData
	#
	header <- getHeader(dataset)
	if (is.null(classFCol)) {
		color <- 1
		colorMsg <- ""
		colLegend <- FALSE
	} else {
		clv <- extractColorLegendValues(dataset, groupBy=classFCol) # returns a list: color_data, color_unique, color_legend, txt, txtE, sumPart, dataGrouping, pch_data, pch_legend
		color <- clv$color_data
		colorMsg <- " color by: "
		colLegend <- TRUE
	}
	ncomp <- plsModel$ncomp
	#
	RMSEC <- getRMSEC(plsModel)
	RMSEC_rdp <- convertToRDP(RMSEC, regrOn, header)
	R2C <- getR2C(plsModel)
	#
	RMSECV <- getRMSECV(plsModel)
	RMSECV_rdp <- convertToRDP(RMSECV, regrOn, header)
	R2CV <- getR2CV(plsModel)
	#
	yvar <- plsModel$model$yvar
	yvarFittedCalib <- plsModel$fitted.values[ , , ncomp]
	yvarFittedCV <- plsModel$validation$pred[ , , ncomp]	
	#
	regrOnMsg <- paste("   regr. on: ", regrOn, "   ",sep="")
	ncompMsg <- paste("   ", ncomp, " comps.", sep="")
	Nmsg <- paste("   N=", nrow(header), sep="")
	mainText <- paste0(onMain, " (valid. ", valid, ")")
	subText <- paste(onSub, regrOnMsg, colorMsg, classFCol, ncompMsg, Nmsg, sep="")
	#
	datCV <- data.frame(xval=yvar, yval=yvarFittedCV)
	datCalib <- data.frame(xval=yvar, yval=yvarFittedCalib)
	xlab <- "measured value"
	ylab <- "predicted value"
	######
	plotPlsrErrorPoints(DF=datCV, colors=color, xlab, ylab, mainText, cex.main=1, subText, ppch=pchPrim) ### CORE ### CORE	
	legPchSec <- NA	
	if (psd) {
		plotPlsrErrorPoints(DF=datCalib, colors=makeColorsTransparent(color, secAlpha), ppch=pchSec, pointsOnly=TRUE) 	
		legPchSec <- pchSec
	}
	######
	abline(0,1, col="gray", lty=ltTarg, lwd=1)
	abline( lm(yvarFittedCalib ~ yvar), lty=ltLm, lwd=1, col=colLmTrain) 
	abline(lm(yvarFittedCV ~ yvar), lty=ltLm, lwd=1, col=colLmCV) 
	#
	nrComps <- paste0("# comps.: ", ncomp)
	rmsec <- paste0("RMSEC: ", RMSEC)
	rmsec_rdp <- paste0("RMSEC[RDP]: ", RMSEC_rdp)
	r2c <- paste0("R2C: ", R2C)
	rmsecv <- paste0("RMSECV: ", RMSECV)
	rmsecv_rdp <- paste0("RMSECV[RDP]: ", RMSECV_rdp)
	r2cv <- paste0("R2CV: ", R2CV)	
	if (inRDP) {
		legendText <- c(nrComps, rmsec, rmsec_rdp, r2c, rmsecv, rmsecv_rdp, r2cv)
		legTxtCol <- c("black", rep(colLmTrain, 3), rep(colLmCV, 3))
		legPch <- c(NA, rep(legPchSec, 3), rep(pchPrim, 3))	
	} else {
		legendText <- c(nrComps, rmsec, r2c, rmsecv, r2cv)
		legTxtCol <- c("black", rep(colLmTrain, 2), rep(colLmCV, 2))
		legPch <- c(NA, rep(legPchSec, 2), rep(pchPrim, 2))		
	}
	legBgCol <- rgb(255,255,255, alpha=.ap2$stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings	
	legCex <- 0.8	
	legend("topleft", legend=legendText, text.col=legTxtCol, pch=legPch, bg=legBgCol, cex=legCex)
	if (colLegend) {
		legend("bottomright", legend=clv$txtE, col=clv$color_legend, pch=pchPrim, bg=legBgCol, cex=legCex)
	}
} # EOF

## not (yet) in use
plot_plsr_errorBars <- function(errorFrame, groupBy=FALSE, whichB="R2CV", onMain="", onSub="", where) {
	if (groupBy == "all") {
		indGroup <- grep(.ap2$stn$p_classVarPref, colnames(errorFrame))
		if (length(indGroup) > 2) {
			stop("Sorry, at the moment not more than 2 grouping possibilites supported. Please pre-split data accordingly.", call.=FALSE)
		}
	} else {
		indGroup <- which(colnames(errorFrame) == groupBy)
	}
	if (any(whichB == "all")) {
		indErr <- (ncol(errorFrame)-3) : ncol(errorFrame)
	} else {
	#	indErr <- which(colnames(errorFrame) == whichB)
		atf <- colnames(errorFrame) %in% whichB
		indErr <- which(atf==TRUE)
	}
	for (indE in indErr) {
		smallTable <- cbind(errorFrame[indGroup], errorFrame[indE])
#		print(smallTable); wait()
		for (indG in indGroup) {
			levelsChar <- levels(factor(smallTable[,indG]))
			if (length(indGroup) > 1) {
				if (indG == 1) {
					other <- nlevels(factor(smallTable[,2]))
					otherNames <- levels(factor(smallTable[,2]))
				} else {
					other <- nlevels(factor(smallTable[,1]))
					otherNames <- levels(factor(smallTable[,1]))
				}
			} else { # so if length of indGroup == 1
			#	other <- length(levelsChar)
			#	otherNames <- levelsChar
				other <- 1
				otherNames <- "value"
			}
			collect <- matrix(NA, nrow=other)
			for (char in levelsChar) {
				ind <- which(smallTable[indG] == char)
				a <- smallTable[ind,]
				collect <- cbind(collect, a[ncol(a)])
			} # end for char
			collect <- collect[-1]
			colnames(collect) <- levelsChar
			rownames(collect) <- otherNames
			collect <- as.matrix(collect)
	#		print(collect); wait()
			if (where != "pdf") {dev.new()}
			if (length(otherNames) > 1) {
				colors <- 1:length(otherNames)
			} else {
				colors <- "lightgrey"
			}
			barplot(collect, beside=T, ylab=colnames(errorFrame[indE]), cex.names=0.8, las=2, main=onMain, sub=onSub, col=colors, ylim= c(0,1))
			legend("top", legend=otherNames, text.col=colors, xpd=TRUE, horiz=TRUE, inset=c(0,-0.11), bty="n")
#			box(bty="l")
		} # end for i
	} # end for indE
} # EOF
####
makePLSRRegressionVectorPlots_inner <- function(plsModels, regrOn, onMain, onSub, dataset, idString, inRDP, bw, adLines, ccol, clty) { # is cycling through all the regrOn of a single set;
	discrim <- .ap2$stn$plsr_regressionVector_discrim
	bandwidth <- bw
	##
	wls <- getWavelengths(dataset)
	mainTxt <- paste(onMain, idString)
	header <- getHeader(dataset)
	##
	for (i in 1: length(plsModels)) { 
		RMSECV <- getRMSECV(plsModels[[i]])
		RMSECV_rdp <- convertToRDP(RMSECV, regrOn[[i]], header)
		R2CV <- getR2CV(plsModels[[i]])
		if (inRDP) {
			legendText <- paste("   RMSECV: ", RMSECV, ", RMSECV[RDP]: ", RMSECV_rdp, ", R2CV: ", R2CV, "", sep="")
		} else {
			legendText <- paste("   RMSECV: ", RMSECV, ", R2CV: ", R2CV, sep="")
		}
		subText <- paste(onSub, " regr. on: ", regrOn[[i]], "   ", legendText, sep="")
		##	
		pickResults <- pickPeaks(plsModels[[i]], bandwidth, comps=NULL, discrim, wavelengths=wls)
		plotPeaks(pickResults, onMain=mainTxt, onSub=subText, adLines, pcaVariances=NULL, customColor=ccol, ylim=NULL, wavelengths=wls, clty)		### !! here the plotting !!!	
	} # end for i
} # EOF

makePLSRRegressionVectorPlots <- function(cube, ap, onMain, onSub, inRDP, bw, adLines, ccol, clty) {
	for (i in 1: length(cube)) {
		aa <- getPLSRObjects(cube[[i]])
		dataset <- getDataset(cube[[i]])
		idString <- adaptIdStringForDpt(ap, getIdString(cube[[i]]))
		makePLSRRegressionVectorPlots_inner(aa$model, aa$regrOn, onMain, onSub, dataset, idString, inRDP, bw, adLines, ccol, clty) #### handing down data from a single set; model, modelPlus and RegrOn are each a list having 1 to n elements !!
	} # end for i
} # EOF

plsr_plotRegressionVectors <- function(cube, ap, bw, adLines, ccol, clty) {
	where <- ap$genPlot$where
	onMain <- ap$genPlot$onMain
	onSub <- ap$genPlot$onSub
	fns <- ap$genPlot$fns
  	inRDP <- ap$plsr$inRdp
	#
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("Plotting PLSR regression vectors... ")}
	expName <- getExpName(cube)
	height <-.ap2$stn$pdf_Height_ws
	width <- .ap2$stn$pdf_Width_ws
	path <- .ap2$stn$fn_results
	suffix <- "plsRegrVec"
	message <- "Regression vectors"
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	onMain <- paste(expName, onMain, sep=" ")
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
	if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new(height=height, width=width)}	
	makePLSRRegressionVectorPlots(cube, ap, onMain, onSub, inRDP, bw, adLines, ccol, clty)
	if (where == "pdf") { dev.off() }
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF

makePLSRErrorPlots_inner <- function(plsModels, plsPlusModels, regrOn, onMain, onSub, classForColoring, dataset, inRDP, idString, psd, finalValid) { # is cycling through all the regrOn of a single set; has the data from a single set; all models(plus) and the regrOn are a list with 1 to n elements !
	plotSwarm <- .ap2$stn$plsr_plotDataInSwarm
	#
	onMainOrig <- onMain
	for (i in 1: length(plsModels)) { # just take the 'plsModels', as they all have the same length
		validChar <- plsModels[[i]]$validation$method
		foldnes <- length(plsModels[[i]]$validation$segments)
		valid <- paste(validChar, " ", foldnes, "", sep="")
		if (foldnes == nrow(dataset)) {
			valid <- "LOO"
		}
		if (is.character(finalValid)) {
			if (finalValid %in% colnames(dataset$header)) {
				valid <- paste0(valid, " by ", finalValid)
			}
		}
		onMain <- paste(onMainOrig, idString)
		#
		plot_plsr_calibValidSwarm(plsModels[[i]], dataset, regrOn[[i]], classForColoring, onMain, onSub, inRDP, valid, psd)
		plot_plsr_error(plsModels[[i]], plsPlusModels[[i]], dataset, regrOn[[i]], onMain, onSub, inRDP)
		if (FALSE) { # abandoned !!
			plot_plsr_calibration_classic(plsModels[[i]], dataset, regrOn[[i]], classForColoring, onMain, onSub, inRDP)
			plot_plsr_validation_classic(plsModels[[i]], dataset, regrOn[[i]], classForColoring, onMain, onSub, inRDP, valid)
		}
	} # end for i
} # EOF

makePLSRErrorPlots <- function(cube, ap, onMain, onSub, where, inRDP, psd) { # is cycling through the cube
	classForColoring <- ap$plsr$colorBy
	##
	for (i in 1: length(cube)) {
		aa <- getPLSRObjects(cube[[i]])
		dataset <- getDataset(cube[[i]])
		idString <- adaptIdStringForDpt(ap, getIdString(cube[[i]]))
		makePLSRErrorPlots_inner(aa$model, aa$modelPlus, aa$regrOn, onMain, onSub, classForColoring, dataset, inRDP, idString, psd, aa$valid) #### handing down data from a single set; model, modelPlus and RegrOn are each a list having 1 to n elements !!
	} # end for i
} # EOF

plsr_plotErrors <- function(cube, ap, psd) {
	where <- ap$genPlot$where
	onMain <- ap$genPlot$onMain
	onSub <- ap$genPlot$onSub
	fns <- ap$genPlot$fns
  	inRDP <- ap$plsr$inRdp
	#
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("Plotting PLSR error plots... ")}
	expName <- getExpName(cube)
	height <-.ap2$stn$pdf_Height_sq
	width <- .ap2$stn$pdf_Width_sq
	path <- .ap2$stn$fn_results
	suffix <- "plsErrors"
	message <- "PLSR Errors"
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	onMain <- paste(expName, onMain, sep=" ")
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
	if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new(height=height, width=width)}	
	makePLSRErrorPlots(cube, ap, onMain, onSub, where, inRDP, psd) ### HERE ###
	if (where == "pdf") { dev.off() }
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF

plot_plsr_checkDefaultsParams <- function(rv.bandwidth, rv.adLine, rv.col, rv.lty, psd) {
	if (all(rv.bandwidth == "def")) {
		rv.bandwidth <- .ap2$stn$pp_bandwidth
	}
	if (!all(is.numeric(rv.bandwidth)) | length(rv.bandwidth) != 1) {
		stop("Please provide a numeric length one for the argument 'rv.bandwidth'.", call.=FALSE)
	}
	assign("rv.bandwidth", rv.bandwidth, pos=parent.frame(n=1))
	##
	aa <- rv.adLine
	if (all(aa == "def")) {
		aa <- .ap2$stn$plsr_AdLines
	}
	if (!is.logical(aa)) {
		if (!all(is.wholenumber(aa)) | (min(aa) < 2) | (max(aa) > 5)) {
			stop("Please provide an integer vector ranging from 2..5 to the argument 'rv.adlines'.", call.=FALSE)
		}
	} # end not logical
	assign("rv.adLine", aa, pos=parent.frame(n=1))
	##
	ccol <- rv.col
	if (all(ccol == "def")) {
		ccol <- .ap2$stn$pca_ld_customColor
	}
	if (!is.null(ccol)) {
		# XXX ? how to check for existence, validity of the provided colors ?
	}
	assign("rv.col", ccol, pos=parent.frame(n=1))
	##
	lty <- rv.lty
	if (all(lty == "def")) {
		lty <- .ap2$stn$pca_ld_customLinetype
	}
	if (!is.null(lty)) {
		if (!all(is.wholenumber(lty)) | !all(lty > 0) | length(lty) != 1) {
			stop("Please provide a positive integer length one for the argument 'rv.lty'.", call.=FALSE)
		}
	}
	assign("rv.lty", lty, pos=parent.frame(n=1))
	##
	if (all(psd == "def")) {
		psd <- .ap2$stn$plsr_plot_secondaryData
	}
	if (!all(is.logical(psd)) | length(psd) != 1) {
		stop("Please provide either TRUE or FALSE to the argument 'psd' resp. to the argument 'plsr_plot_secondaryData' in the settings file.", call.=FALSE)
	}
	assign("psd", psd, pos=parent.frame(n=1))
	##	
} # EOF

### CORE ### CORE ###
plot_pls_cube <- function(cube, aps="def", rv.bandwidth="def", rv.adLine="def", rv.col="def", rv.lty="def", psd="def", ...) {
	autoUpS()
	ap <- doApsTrick(aps, cube, ...)	
	ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset=getDataset(cube[[1]]), haveExc=FALSE) # just take the first dataset, as we mainly need the header (and the wavelengths are already checked.. )
	if (is.null(ap$plsr)) {
		return(cat("*** PLSR model not available or not selected for plotting \n"))
	}
	plot_plsr_checkDefaultsParams(rv.bandwidth, rv.adLine, rv.col, rv.lty, psd) # is assigning here !!
	##
	wch <- pv_plsr_what  # c("both", "errors", "regression")
  	what <- ap$plsr$what
  	if( any(c(wch[2], wch[1]) %in% what) ) {
		plsr_plotErrors(cube, ap, psd)  		  	
  	}
  	if( any(c(wch[3], wch[1]) %in% what) ) { 
  		plsr_plotRegressionVectors(cube, ap, bw=rv.bandwidth, adLines=rv.adLine, ccol=rv.col, clty=rv.lty) 	
  	}
  	return(invisible(NULL))
} # EOF

###
# plot_pls_indepPred , i.e. plotting independent plsr predictions, is in the calc_plsr.r file (as there is plotting and calculation...)
###


# documentation ----------------------------------------
#' @title Plot PLSR
#' @description Plot PLSR error and calibration / crossvalidation plots. 
#' @details By providing one or more of the \code{...} arguments you can 
#' override the plotting values in the selected analysis procedure, see examples.
#' The width and height of the resulting pdf can be set in the settings.
#' @param object An object of class 'aquap_cube' as produced by \code{\link{gdmm}}.
#' @param ... Optional 'pls' plotting parameters to override the values in the 
#'  analysis procedure - for possible arguments see \code{\link{plot_pls_args}} 
#' and below:
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
#' \item{rv.bandwidth}{Character "def", or numeric length one. The bandwidth of 
#' wavelengths used in the peak-picking process when plotting the regression 
#' vector. If left at the default "def", the value from the settings.r file 
#' (parameter \code{pp_bandwidth}) is used.}
#'  \item{rv.adLine}{Logical, numeric or character 'def'. If set to \code{FALSE},
#' no additional lines, if set to \code{TRUE} all the additional lines will be 
#' plotted in the loading plot. If an integer vector [2..5] is provided, one or 
#' more of the additional lines get plotted. See \code{\link{adLinesToVector}} 
#' for details. If left at the default value 'def', the value from the 
#' settings.r file (parameter \code{plsr_AdLines}) is used.}
#'  \item{rv.col}{ Character "def" or a valid color. A possible custom color 
#' for coloring the plsr regression vector. When left at the default "def" 
#' the value fromt the settings.r file is read in (parameter 
#' \code{plsr_rv_customColor}). Provide a color-vector to use it for 
#' coloring the plsr regression vector.}
#'  \item{rv.lty}{Character "def" or a positive integer indicating the 
#' desired line type. When left at the default "def" the value from the 
#' settings.r file is read in (parameter \code{plsr_rv_customLinetype}).}
#' \item{psd}{'plot secondary data'; either character 'def' or logical. If 
#' secondary (i.e. calibration data) should be plotted as well. Leave at the 
#' default 'def' to take the value from the parameter 
#' \code{plsr_plot_secondaryData} in the settings file, or provide TRUE or FALSE.
#' The alpha level for the secondary data can be set in parameter 
#' \code{plsr_color_alpha_secondaryData} in the settings file.}
#' }
#' @return A pdf or graphic device.
#' @examples
#'  \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset)
#'  plot(cube)
#'  plot_pls(cube)
#'  plot_pls(cube, aps="foobar.r") # to get the plotting parameters 
#'  # from the file "foobar.r".
#'  plot_pls(cube, lv.col="red") # to have the regression vector in red
#'  plot(cube, what="pls", pg.where="", rv.col="red", rv.lty=2)
#'  plot(cube, pg.where="", rv.col="red", rv.lty=2) 
#'  plot(cube, pls.what="regression") # for plsr, only plot the regression vector
#' }
#' @family Plot functions
#' @family PLSR documentation
#' @name plot_pls
NULL





#' @title Plot PLSR - Arguments
#' @description The following parameters can be used in the \code{...} argument 
#' e.g. in function \code{\link{plot}} and \code{\link{plot_pls}} to override 
#' the values in the analysis procedure file and so to modify the graphics - 
#' see examples.
#' \describe{
#' \item{\code{plot(cube, ...)}}{ }
#' \item{ \code{plot_pls(cube, ...)}}{ }
#' }
#' @template mr_details_allParams
#' @template mr_pls_plot_param
#' @template mr_pg_genParams
#' @seealso \code{\link{plot_pls}}
#' @examples
#' \dontrun{
#' dataset <- gfd()
#' cube <- gdmm(dataset)
#' plot(cube, do.pls=FALSE) # to plot everything availalbe except the plsr-models
#' plot(cube, pls.colorBy="C_Temp")
#' plot_pls(cube, pls.colorBy="C_Temp")
#' }
#' @family Plot arguments
#' @family PLSR documentation
#' @name plot_pls_args
NULL



#' @title Calculate PLSR - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{getap}}, also within function \code{\link{gdmm}}, to 
#' override the values in the analysis procedure file and so to modify the 
#' calculation of PLSR models - see examples.
#' \describe{
#' \item{\code{getap(...)}}{ }
#' \item{\code{gdmm(dataset, ap=getap(...))}}{ }
#' }
#' @section Note: Calculation of PLSR models is done with the function  
#' \code{\link[pls]{plsr}}. 
#' @template mr_details_allParams
#' @template mr_pls_calc_param
#' @seealso \code{\link{gdmm}}
#' @examples
#' \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset, ap=getap(pls.regOn="Y_Temp"))
#'  cube <- gdmm(dataset, ap=getap(pls.ncomp=5, pls.regOn="Y_Foo"))
#' }
#' @family Calc. arguments
#' @family PLSR documentation
#' @name calc_pls_args
NULL
