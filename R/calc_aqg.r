## not in use !
calc_groupAverages <- function(dataset, colInd, nrCorr, nrPartic) {
	classVar <- colnames(dataset)[colInd]
	charLevels <- levels(dataset[, colInd])
	minPartic <- min(nrPartic)
	NIR <- NULL
	selIndOut <- list()
	for (i in 1: length(charLevels)) {
		a <- ssc(dataset, list(list(classVar, charLevels[i])), T, T)
		if (nrCorr & (nrow(a) > minPartic) ) {
			selInd <- sample(1:nrow(a), minPartic)
		} else {
			selInd <- 1:nrow(a)
		}
		avgNIR <- apply(a$NIR[selInd, ], 2, mean)
		NIR <- rbind(NIR, avgNIR)
		selIndOut <- c(selIndOut, list(selInd))
	} # end for i
	avgs <- data.frame(charLevels, NIR, row.names=charLevels)
	colnames(avgs) <- c("Group", colnames(dataset$NIR))
	out <- list(avgs=avgs, selInd=selIndOut)
} # EOF

## gives back a data frame with a row for every group and all the wavelengths; the first column contains the levels
do_ddply <- function(dataset, colInd) {
#	header <- isolateHeader(dataset)
	charLevels <- levels(dataset$header[, colInd])
	NIRnice <- as.data.frame(matrix(dataset$NIR, ncol=ncol(dataset$NIR)))
	res <- plyr::ddply(NIRnice, plyr::.(dataset$header[,colInd]), plyr::colwise(mean))
	colnames(res) <- c("Group", colnames(dataset$NIR))
	rownames(res) <- charLevels
	return(res[,-1])	## leave out the first column with the group
} # EOF

aquCoreCalc_Classic <- function(dataset, smoothN, reference, msc, selIndsWL, colInd) {
	if (is.numeric(smoothN)){
		dataset <- do_sgolay(dataset, p=2, n=smoothN, m=0)
	}
	if (msc==TRUE) {
		dataset <- do_msc(dataset, reference)
	}
	dataset$NIR <- dataset$NIR[,selIndsWL]
	dataset <- do_scale(dataset)		## now using normalize (different results with scale !!)
	groupAverage <- do_ddply(dataset, colInd)
} #EOF

aquCoreCalc_Classic_diff <- function(dataset, smoothN, reference, msc, selIndsWL, colInd, minus) {
	classic <- aquCoreCalc_Classic(dataset, smoothN, reference, msc, selIndsWL, colInd)
	ind <- which(rownames(classic) == minus)
	if (length(ind) < 1) {
		stop("\nI am sorry, please provide a valid value for 'minus' to perform subtractions within the aquagram. Thanks.", call.=FALSE)
	}
	subtr <- as.numeric(classic[ind,])
	out <- t(apply(classic, 1, function(x) x-subtr))
} # EOF

aquCoreCalc_AUCstabilized <- function(dataset, smoothN, colInd) {
	if (is.numeric(smoothN)){
		dataset <- do_sgolay(dataset, p=2, n=smoothN, m=0)
	}
	dataset$NIR <- calcAUCtable(dataset$NIR)$aucd 		## "NIR" being actually the area under the curve divided by its fullArea for every row in every coordinate
	groupAverages <- do_ddply(dataset, colInd)	## the group averages of the area under the curve, still in raw area units
	perc <- calcAUCPercent(groupAverages, .ap2$aucEx) ## .aucEx being the package-based calibration data for the min. and max. AUC for each coordinate.
} #EOF

aquCoreCalc_AUCstabilized_diff <- function(dataset, smoothN, colInd, minus) {
	perc <- aquCoreCalc_AUCstabilized(dataset, smoothN, colInd)
	ind <- which(rownames(perc) == minus)
	if (length(ind) < 1) {
		stop("\nI am sorry, please provide a valid value for 'minus' to perform subtractions within the aquagram. Thanks.", call.=FALSE)
	}
#	subtr <- as.numeric(perc[ind,])
#	out <- t(apply(perc, 1, function(x) x-subtr))
	out <- sweep(perc, 2, perc[ind,])
	out
} # EOF

aquCoreCalc_NormForeignCenter <- function(dataset, smoothN, reference, msc, selIndsWL, colInd) {
	if (is.numeric(smoothN)){
		dataset <- do_sgolay(dataset, p=2, n=smoothN, m=0)
	}
	if (msc==TRUE) {
		dataset <- do_msc(dataset, reference)
	}
	dataset$NIR <- dataset$NIR[,selIndsWL]
	dataset <- do_scale_fc(dataset, .ap2$tempCalibFCtable[, selIndsWL])
	groupAverage <- do_ddply(dataset, colInd)
} # EOF

aquCoreCalc_NormForeignCenter_diff <- function(dataset, smoothN, reference, msc, selIndsWL, colInd, minus) {
	values <- aquCoreCalc_NormForeignCenter(dataset, smoothN, reference, msc, selIndsWL, colInd)
	ind <- which(rownames(values) == minus)
	if (length(ind) < 1) {
		stop("\nI am sorry, please provide a valid value for 'minus' to perform subtractions within the aquagram. Thanks.", call.=FALSE)
	}
#	out <- sweep(values, 2, values[ind,])
	subtr <- as.numeric(values[ind,])
	out <- t(apply(values, 1, function(x) x-subtr))
} # EOF

aquCoreCalc_aucs_tempNorm <- function(dataset, smoothN, colInd) {
	perc <- aquCoreCalc_AUCstabilized(dataset, smoothN, colInd) ## this is the percentage from the real data, the measurement
	percDiff <- sweep(perc, 2, .ap2$tempNormAUCPerc)		## .tempNormAUCPerc is the percentage of AUC of a selection of calibration data with only the T of the experiment
} # EOF

aquCoreCalc_aucs_tempNorm_diff <- function(dataset, smoothN, colInd, minus) {
	values <- aquCoreCalc_aucs_tempNorm(dataset, smoothN, colInd)
	ind <- which(rownames(values) == minus)
	if (length(ind) < 1) {
		stop("\nI am sorry, please provide a valid value for 'minus' to perform subtractions within the aquagram. Thanks.", call.=FALSE)
	}
	subtr <- as.numeric(values[ind,])
	out <- t(apply(values, 1, function(x) x-subtr))
} # EOF

aquCoreCalc_aucs_tempNorm_DCE <- function(dataset, smoothN, colInd, TCalib, Texp) {
	percDiff <- aquCoreCalc_aucs_tempNorm(dataset, smoothN, colInd)
	if (is.null(TCalib)) {
		stop("Please provide two numerical values for the temperature calibration range for this mode", call.=FALSE)
	}
	deltaCalib <- diff(TCalib)
	# value * delta / 100
	deltaTemp <- apply(percDiff, 1, function(x, dCal) {
		(x * dCal) / 100
	}, dCal=deltaCalib)
	deltaTemp <- t(deltaTemp)
} # EOF

aquCoreCalc_aucs_tempNorm_DCE_diff <- function(dataset, smoothN, colInd, TCalib, Texp, minus) {
	values <- aquCoreCalc_aucs_tempNorm_DCE(dataset, smoothN, colInd, TCalib, Texp)
	ind <- which(rownames(values) == minus)
	if (length(ind) < 1) {
		stop("\nI am sorry, please provide a valid value for 'minus' to perform subtractions within the aquagram. Thanks.", call.=FALSE)
	}
	subtr <- as.numeric(values[ind,])
	out <- t(apply(values, 1, function(x) x-subtr))
} # EOF

aquCoreCalc_aucs_DCE <- function(dataset, smoothN, colInd, TCalib) {
	perc <- aquCoreCalc_AUCstabilized(dataset, smoothN, colInd)
	if (is.null(TCalib)) {
		stop("Please provide two numerical values for the temperature calibration range for this mode", call.=FALSE)
	}
	deltaCalib <- diff(TCalib)
	fac <- 100 / deltaCalib
	# divide every value with fac	
	out <- perc / fac
	out <- out + min(TCalib)
} # EOF

aquCoreCalc_aucs_DCE_diff <- function(dataset, smoothN, colInd, TCalib, minus) {
	values <- aquCoreCalc_aucs_DCE(dataset, smoothN, colInd, TCalib)
	ind <- which(rownames(values) == minus)
	if (length(ind) < 1) {
		stop("\nI am sorry, please provide a valid value for 'minus' to perform subtractions within the aquagram. Thanks.", call.=FALSE)
	}
	subtr <- as.numeric(values[ind,])
	out <- t(apply(values, 1, function(x) x-subtr))	
} # EOF

##############
calc_aquagr_CORE <- function(dataset, smoothN, reference, msc, selIndsWL, colInd, mod, minus, TCalib, Texp) {
	if (mod == "classic") {
		return(aquCoreCalc_Classic(dataset, smoothN, reference, msc, selIndsWL, colInd))
	}
	if (mod == "aucs") {
		return(aquCoreCalc_AUCstabilized(dataset, smoothN, colInd))
	}
	if (mod == "aucs-diff") {
		return(aquCoreCalc_AUCstabilized_diff(dataset, smoothN, colInd, minus))
	}
	if (mod == "sfc") {
		return(aquCoreCalc_NormForeignCenter(dataset, smoothN, reference, msc, selIndsWL, colInd))
	}
	if (mod == "sfc-diff") {
		return(aquCoreCalc_NormForeignCenter_diff(dataset, smoothN, reference, msc, selIndsWL, colInd, minus))
	}
	if (mod == "classic-diff") {
		return(aquCoreCalc_Classic_diff(dataset, smoothN, reference, msc, selIndsWL, colInd, minus))
	}
	if (mod == "aucs.tn") {
		return(aquCoreCalc_aucs_tempNorm(dataset, smoothN, colInd))
	}
	if (mod == "aucs.tn.dce") {
		return(aquCoreCalc_aucs_tempNorm_DCE(dataset, smoothN, colInd, TCalib, Texp))
	}
	if (mod == "aucs.tn-diff") {
		return(aquCoreCalc_aucs_tempNorm_diff(dataset, smoothN, colInd, minus))
	}
	if (mod == "aucs.tn.dce-diff") {
		return(aquCoreCalc_aucs_tempNorm_DCE_diff(dataset, smoothN, colInd, TCalib, Texp, minus))
	}
	if (mod == "aucs.dce") {
		return(aquCoreCalc_aucs_DCE(dataset, smoothN, colInd, TCalib))
	}
	if (mod == "aucs.dce-diff") {
		return(aquCoreCalc_aucs_DCE_diff(dataset, smoothN, colInd, TCalib, minus))
	}	
	stop("Please provide a valid value for the 'mod' argument", call.=FALSE)											
} # EOF
##############

calc_aquagr_bootCI <- function(dataset, smoothN, reference, msc, selIndsWL, colInd, useMC, R, mod, minus, TCalib, Texp, parChar, stnLoc) {
	fnAnD <- stnLoc$fn_analysisData
	saveBootResult <- stnLoc$aqg_saveBootRes
	path <- paste(fnAnD, "bootResult", sep="/")
	#
	if (!dir.exists(fnAnD)) {
		ok <- dir.create(fnAnD)
		if (!ok) {
			saveBootResult <- FALSE
		}
	}
	innerWorkings <- function(x, ind) {
		out <- as.matrix(calc_aquagr_CORE(x[ind,], smoothN, reference, msc, selIndsWL, colInd, mod, minus, TCalib, Texp))
	} # EOIF
	if (!stnLoc$allSilent) {cat(paste0("      calc.", R, " bootstrap replicates (", parChar, ") ... ")) }
	thisR <- R
	nCPUs <- getDesiredNrCPUs(allowNA=FALSE)
	bootResult <- boot::boot(dataset, innerWorkings, R=thisR, strata=dataset$header[,colInd], parallel=useMC, ncpus=nCPUs)   	### here the bootstrap replicates happen
	if (!stnLoc$allSilent) {cat("ok\n")}
	if (saveBootResult) {
		save(bootResult, file=path)
	}
#	load(path)
#	print(str(bootResult)); print(bootResult$t0); print(bootResult$t[1:5, 1:12]); wait()
#	if (mod == "aucs-diff" | mod == "nfs-diff" | mod == "classic-diff" | mod == "aucs.tn-diff" |  mod == "aucs.tn.dce-diff" | mod == "aucs.dce-diff")
	origBRt0 <- bootResult$t0
	if (grepl("diff", mod)) {
	#	colSeq <- seq(1, ncol(bootResult$t0)*nrow(bootResult$t0), by=nrow(bootResult$t0))
		zeroInd <- which(apply(bootResult$t, 2, function(x) all(x == 0)) == TRUE)
		bootResult$t <- bootResult$t[, -zeroInd]		## clean out all the zeros
		selInd <- which(rownames(bootResult$t0) == minus)
		bootResult$t0 <- bootResult$t0[-selInd,]
		a <- which(bootResult$strata == minus)
		bootResult$strata <- as.factor(as.character(bootResult$strata[-a]))
		bootResult$weights <- bootResult$weights[-a]
		bootResult$data <- bootResult$data[-a,]
	}	
#	print(str(bootResult)); print(bootResult$t0); print(bootResult$t[1:5, 1:12]); wait()
	nRows <- dim(bootResult$t0)[1]
	nCols <- dim(bootResult$t0)[2]
	if (!.ap2$stn$allSilent) {cat("      calc. confidence intervals... ")}
#	ciMat <- matrix(NA, nRows*2, nCols)
#	kseq <- seq(1, nRows*2, by=2)
#	for (i in 1: nCols) {
#		for (k in 1: nRows) {
#			cind <- (i-1)*nRows + k
#			ciMat[c(kseq[k], kseq[k]+1), i] <- boot.ci(bootResult, index = cind, type="bca")$bca[,4:5]    #### here the CIs are calculated 
#		} # end for k
#	} # end for i
#	####
	mat2er <- foreach(i = 1: (nRows*nCols), .combine="cbind") %dopar% {
			a <- boot::boot.ci(bootResult, index = i, type="bca")$bca[,4:5]    #### here the CIs are calculated 
	} # end dopar i
	if (!stnLoc$allSilent) {cat("ok\n")}
	ciMat <- matrix(mat2er, ncol=nCols) 
	####
	origMat <- bootResult$t0
	colnames(ciMat) <- colnames(origMat)
	fusionMat <- NULL
	kseq <- seq(1, nRows*2, by=2)
	for (i in 1: nRows) {				## fuse together with original data
		a <- matrix(origMat[i,], nrow=1)
		rownames(a) <- rownames(origMat)[i]
		b <- ciMat[c(kseq[i], kseq[i]+1), ]
		fusionMat <- rbind(fusionMat, rbind(a,b))
	} # end for i
#	print(fusionMat); wait()	
	####
	if (grepl("diff", mod)) {			### re-insert the cut-out part in case of a "diff" mode so that the legend and colors is the same with the normal aquagram
		fillIn <- matrix(0, 3, nCols)
		selInd <- which(rownames(origBRt0) == minus)
		if (selInd == 1) {
			outMat <- rbind(fillIn, fusionMat)
		} else {
			if (selInd == nrow(origBRt0)) { 	## so the last group was cut away
				outMat <- rbind(fusionMat, fillIn)
			} else {		## so we have to insert somewhere in between
				tf <- 1; tt <- (selInd-1) * 3
				topSegment <- fusionMat[tf:tt, ]
				bf <- (nrow(fusionMat) - ((nrow(origBRt0) - selInd) * 3)) +1 ; bt <- nrow(fusionMat)
				bottomSegment <- fusionMat[bf:bt, ]
				outMat <- rbind(topSegment, fillIn, bottomSegment)
			}			
		}
	} else {  	## so we do NOT have a "diff" mod
		outMat <- fusionMat
	}
	rownames(outMat) <- make.unique(rep(rownames(origBRt0), each=3))
#	print(outMat)
	return(outMat)
} # EOF

copy_aquagram_rawspectra <- function(dataset, classVar, selInds) { 
	dataset <- dataset[selInds,]			## to have the same observations as used in the aquagram
#	colInd <- which(colnames(dataset) == classVar)
	colRepInd <- which(colnames(dataset$colRep) == classVar)
	NIR <- dataset$NIR
	out <- data.frame(colRep=dataset$colRep[,colRepInd], I(NIR))
} # EOF

calc_avg_aquagram_spectra <- function(dataset, classVar, selInds) { 
	dataset <- dataset[selInds,]			## to have the same observations as used in the aquagram
	colInd <- which(colnames(dataset$header) == classVar)
	colRepInd <- which(colnames(dataset$colRep) == classVar)
	charLevels <- levels(dataset$header[, colInd])
	NIR <- NULL
	colRepOut <- NULL
	for (i in 1: length(charLevels)) {
		a <- ssc_s(dataset, classVar, charLevels[i], keepEC=FALSE)
#		a <- a[selInds[[i]], ] 		
		avgNIR <- apply(a$NIR, 2, mean)
		colRep <- unique(a$colRep[, colRepInd])
		NIR <- rbind(NIR, avgNIR)
		colRepOut <- c(colRepOut, colRep)
	}
	rownames(NIR) <- charLevels
	out <- data.frame(colRep=colRepOut, I(NIR))
} # EOF
 
calc_minus_avg_aquagram_spectra <- function(avgAquagrSpectra, minus) {
	avgs <- avgAquagrSpectra
	rowInd <- which(rownames(avgs) == minus)
	if (length(rowInd) < 1) {
		stop("Please provide a valid value for 'minus' to perform subtractions of averaged Aquagram-spectra", call.=FALSE)
	}
	minusSpectrum <- avgs$NIR[rowInd,]
	avgs$NIR <- sweep(avgs$NIR, 2, minusSpectrum)
	rownames(avgs) <- rownames(avgs$NIR) <- paste(rownames(avgs), minus, sep=" - ")
	avgs[-rowInd,]
} # EOF 

 ######################
 
tempCalibTransformDataset <- function(tempCalibDataset) {
	if (!.ap2$stn$allSilent) {cat(" * Reading in temperature data... ")}
	yPref <- .ap2$stn$p_yVarPref
	wtcn <- paste0(yPref, pv_YcolumnNameSampleTemp)
	header <- data.frame(sample=rep("RM", nrow(tempCalibDataset)), temp_W=as.numeric(tempCalibDataset$header[,wtcn])) # we already checked for the existence of the water temp column
	nir <- getNIR(tempCalibDataset)
	out <- cbind(header, nir)
	if (!.ap2$stn$allSilent) {cat("ok\n")}
	return(out)
} # EOF

tempCalibMakeTable <- function(fdata, TRange=NULL, ot=c(1300, 1600)) {
	if (is.null(TRange)) {
		a <- 1:nrow(fdata)
	} else {
#		a <- which(fdata$temp_W %in% TRange)
		a <- which(fdata$temp_W %in% round(seq(min(TRange), max(TRange), by=0.5),1) ) ## XXXMODXXX
	}
#	fdata <- fdata[min(a):max(a), ]
	fdata <- fdata[a, ]
	rownames(fdata) <- make.unique(as.character(fdata$temp_W))
	spect <- fdata[ , -c(1,2)]
	wls <- as.numeric(substr(colnames(spect), 2, nchar(colnames(spect)) ))
#	b <- which(wls %in% ot)
	b <- range(which(wls >= ot[1] & wls <= ot[2])) # is an index !!
	spectOt <- spect[, b[1]:b[2]]
	wlsOt <- wls[b[1]:b[2]]
	colnames(spectOt) <- paste("w", wlsOt, sep="")
	return(spectOt)
} # EOF

tempCalibMakeAvgTable <- function(fdata, smoothN=17, TRange=NULL, ot=c(1300, 1600)) {
	if (is.null(TRange)) {
		a <- 1:nrow(fdata)
	} else {
		a <- which(fdata$temp_W %in% TRange)
	}
	fdata <- fdata[min(a):max(a), ]
	spect <- t(apply(fdata[,-(1:2)], 1, signal::sgolayfilt, p=2, n=smoothN, m=0))
	colnames(spect) <- colnames(fdata)[-c(1,2)]
	wls <- as.numeric(substr(colnames(spect), 2, nchar(colnames(spect)) ))
	OverTone <- ot
#	a <- which(wls %in% OverTone) # XXX modify this for better safety !!! XXXMODXXX
	a <- range(which(wls >= OverTone[1] & wls <= OverTone[2])) # is an index !!
	spectOt <- spect[, a[1]:a[2]]
	wlsOt <- wls[a[1]:a[2]]
	Tnames <- fdata$temp_W
	avgSpect <- plyr::ddply(as.data.frame(spectOt), plyr::.(Tnames), plyr::colwise(mean)) ## average the single spectra to one row with a single temperature each
	rownames(avgSpect) <- TnamesAvg <- avgSpect[,1]
#	colnames(avgSpect) <- paste("w", wlsOt, sep="")
	return(avgSpect[, -1])
} # EOF

calcUnivAucTable <- function(smoothN=17, ot=c(1300, 1600), tcdName) {
	dataset <- get(tcdName, pos=.ap2)
	if (!.ap2$stn$allSilent) {cat(" * Calculating universal AUC table... ")}
	avgTable <- tempCalibMakeAvgTable(dataset, smoothN, TRange=NULL, ot)
	aucd <- calcAUCtable(avgTable)$aucd
	if (!.ap2$stn$allSilent) {cat("ok\n")}
	return(aucd)
} #EOF

## !gives back a list!; 
## calculates the AUC-value in every coordinate for every single row (so we get back same number of rows, but only e.g. 15 columns)
calcAUCtable <- function(NIRdata) { 
	wls <- as.numeric(substr(colnames(NIRdata), 2, nchar(colnames(NIRdata)) ))
#	Call <- t(readInSpecAreasFromSettings())
	Call <- getOvertoneWls(.ap2$stn$aqg_OT)
#	wlCrossPoint=1438
#	indCrossPoint <- which(wlsOt == wlCrossPoint)
	saCorRes <- NULL
	saCorRes_d <- NULL
###  looping through the single rows
	for (i in 1: nrow(NIRdata)) { # of avgSpect  ## takes the first of the averaged spectra, each representing a unique temperature
		singleSpect <- NIRdata[i,] 
		lmy <- as.numeric(singleSpect[ c(1, length(singleSpect)) ] )
		lmx <- c(wls[ c(1, length(singleSpect)) ] )
		funkyLM <- lm(lmy~lmx)
		areaf1x <- c(wls, wls[1])
		areaf1y <- c(singleSpect, singleSpect[1])
		fullArea <- geometry::polyarea(areaf1x, as.numeric(areaf1y))
		########
		saCorOut <- NULL
		saCorOut_d <- NULL
		for (k in 1: nrow(Call)) {
			pp1 <- Call[k, 1] * funkyLM$coefficients[2] + funkyLM$coefficients[1]
			pp2 <- Call[k, 2] * funkyLM$coefficients[2] + funkyLM$coefficients[1]
	#		a <- which(wls %in% Call[k,]) ## indices of the boundaries of the wavelength of the current coordinate
			a <- range(which(wls >= Call[k,][1] & wls <= Call[k,][2]))
			wlsCoord <- wls[a[1]:a[2]]
			spectValCoord <- as.numeric(singleSpect[a[1]:a[2]])	
			areaCordx <- c( wlsCoord, wls[a[2]], wls[a[1]], wls[a[1]])
			areaCordy <- c( spectValCoord, pp2, pp1, spectValCoord[1])
			singleAreaCord <- geometry::polyarea(areaCordx, areaCordy)
			saCorOut <- c(saCorOut, singleAreaCord)
			saCorOut_d <- c(saCorOut_d, singleAreaCord / fullArea)
		} # end for k
		saCorRes <- rbind(saCorRes, saCorOut)
		saCorRes_d <- rbind(saCorRes_d, saCorOut_d)
	} # end for i
	rownames(saCorRes) <- rownames(saCorRes_d) <- rownames(NIRdata)
	colnames(saCorRes) <- colnames(saCorRes_d) <- getOvertoneColnames(.ap2$stn$aqg_OT)
	return(list(auc=saCorRes, aucd=saCorRes_d))
} # EOF
	
calcAUCextrema <- function(aucList) {
	aucTable <- aucList$aucd
	minIndex <- apply(aucTable, 2, function(x) which(x==min(x)))
	maxIndex <- apply(aucTable, 2, function(x) which(x==max(x)))	
#	minIndex <- rep(1, ncol(aucTable))
#	maxIndex <- rep(nrow(aucTable), ncol(aucTable))
	minVal <- sapply(1:ncol(aucTable), function(i, x, mInd) {
		x[,i][mInd[i]]
	}, x=aucTable, mInd=minIndex)
	maxVal <- sapply(1:ncol(aucTable), function(i, x, mInd) {
		x[,i][mInd[i]]
	}, x=aucTable, mInd=maxIndex)
	out <- rbind(minVal, maxVal)
	colnames(out) <- colnames(aucTable)
	out
} # EOF

calcAUCPercent <- function(aucTable, aucCalibExtrema) {
	diffs <- apply(aucCalibExtrema, 2, diff)
	exMins <- aucCalibExtrema[1,]
	aa <- sweep(aucTable, 2, exMins)	## subtract the minimum
	perc <- apply(aa, 1, function(x,g) {
		(x*100) / g
	}, g=diffs)
	perc <- t(perc)
#	print(aa); print(diffs); print(perc); wait()
	return(perc)
} # EOF
## used by the routine to prepare the calibration data file for the package
getAUCcalibExtrema_OLD <- function(dataset, TRange=NULL, smoothN=17, ot=c(1300, 1600)) {
	avgTable <- tempCalibMakeAvgTable(dataset, smoothN, TRange, ot)
	auc <- calcAUCtable(avgTable)
	aucEx <- calcAUCextrema(auc)	
} # EOF
getTempNormAUCPercTable_OLD <- function(fdata, smoothN, TRange, ot, aucExtrema) {
	dataTable <- tempCalibMakeTable(fdata, TRange, ot)
	coln <- colnames(dataTable)
	if (is.numeric(smoothN)){
		dataTable <- as.data.frame(t(apply(dataTable, 1, signal::sgolayfilt, p=2, n=smoothN, m=0)))
		colnames(dataTable) <- coln
	}
	aucd <- calcAUCtable(dataTable)$aucd
	aucd <- matrix(apply(aucd, 2, mean), nrow=1)
	normPerc <- calcAUCPercent(aucd, aucExtrema)
} # EOF

getAUCcalibExtrema <- function(univAucTable, TCalib) {
	aucd <- univAucTable
	temp <- as.numeric(rownames(aucd))
	if (is.null(TCalib)) {
		TCalib <- c(min(temp), max(temp))
	}
	if (all(TCalib %in% temp)) { # so we already have the exact temperature, we do not need the interpolate the auc
		ind <- which(temp %in% TCalib)
		out <- aucd[ind,]
		return(calcAUCextrema(list(aucd=out)))
	}
	## now we do NOT find the exact calibration temperature, we have to interpolate it using loess
	out <- NULL
	for (i in 1: ncol(aucd)) {
		loessMod <- loess(aucd[,i] ~ temp, family ="symmetric") # loess here to have *every* auc, also in the "between points" available
		a <- matrix(predict(loessMod, TCalib), ncol=1)
		out <- cbind(out, a)
	} # end for i
	colnames(out) <- colnames(aucd)
	rownames(out) <- as.character(TCalib)
	a <- list(aucd=out) 	# because the next one needs a list as input (a bit silly... yes..)
	aucEx <- calcAUCextrema(a)
	return(aucEx)
} # EOF

getTempNormAUCPercTable <- function(univAucTable, Texp, aucExtrema) {
	aucd <- univAucTable
	temp <- as.numeric(rownames(aucd))
	if (Texp %in% temp) {
		ind <- which(temp == Texp)
		out <- matrix(aucd[ind,], nrow=1)
		return(calcAUCPercent(out, aucExtrema))
	}
	## now we do NOT have the exact temp. of the experiment, we have to interpolate using loess
	out <- NULL
	for (i in 1: ncol(aucd)) {
		loessMod <- loess(aucd[,i] ~ temp, family ="symmetric")
		a <- matrix(predict(loessMod, Texp), ncol=1)
		out <- cbind(out, a)
	}
	normPerc <- calcAUCPercent(out, aucExtrema)
	return(normPerc)
} # EOF

## gets called once in gdmm only if we will calculate an aquagram, so if tempCalibDataset does NOT come in as NULL
aq_loadGlobalAquagramCalibData <- function(tempCalibDataset, tempFile) {
	if (!is.null(tempCalibDataset)) {
		tcdName <- paste0(tempFile, "_tcd")
		if (!exists(tcdName, where=.ap2)) {
			assign(tcdName, tempCalibTransformDataset(tempCalibDataset), pos=.ap2) 
		}
		if (!exists("aquagramPSettings",  where=.ap2)) {
			assign("aquagramPSettings", readInAquagramPSettings(), pos=.ap2)
		}
		univAucTableName <- paste0(tempFile, "_univAucTable")
		if (!exists(univAucTableName, where=.ap2)) {
			aut <-  calcUnivAucTable(smoothN=.ap2$stn$aqg_smoothCalib, ot=getOvertoneCut(.ap2$stn$aqg_OT), tcdName)
			assign(univAucTableName, aut, pos=.ap2)
		}
	} # end !is.null(tempCalibDataset)
} # EOF

## gets called inside the aquagram
aq_makeGlobals <- function(TCalib, Texp, ot, smoothN, tempFile) {
	univAucTableName <- paste0(tempFile, "_univAucTable")
	#
	dataset <- get(paste0(tempFile, "_tcd"), pos=.ap2)
	assign("tempCalibFCtable", tempCalibMakeTable(dataset, TCalib, ot), pos=.ap2)	# probably only used for mode "sfc"	
	assign("aucEx", getAUCcalibExtrema(get(univAucTableName, pos=.ap2), TCalib), pos=.ap2)
	assign("tempNormAUCPerc", getTempNormAUCPercTable(get(univAucTableName, pos=.ap2), Texp, .ap2$aucEx), pos=.ap2)
} # EOF

#########
readInAquagramPSettings <- function() {
	if (exists(".devMode", envir=.ap2)) {
		filepath <- .ap2$.devDataPath
		filepath <- paste(filepath, "aqugrStngs", sep="")
	} else {
		a <- path.package("aquap2")
		File <- "/pData/aqugrStngs"
		filepath <- paste(a, File, sep="")
	}
#	load(filepath)
	return(eval(parse(text=load(filepath))))
#	out <- get(".aquagramPSettings")
#	return(out)
} #EOF

getOvertoneCut <- function(otNumberChar) {
	if (otNumberChar == "1st") {
		return(.ap2$aquagramPSettings$ot1$cut)
	}
} # EOF

getOvertoneWls <- function(otNumberChar) {
	if (otNumberChar == "1st") {
		if (.ap2$stn$aqq_nCoord == 12) {
			return(.ap2$aquagramPSettings$ot1$wls$wls12)
		} else {
			if (.ap2$stn$aqq_nCoord == 15) {
				return(.ap2$aquagramPSettings$ot1$wls$wls15)
			} else {
				stop("Please provide either '12' or '15' as the numbers of coordinates for the first overtone in the settings. Thank you.", call.=FALSE)
			}
		}		
	} # end 1st
} # EOF

getOvertoneColnames <- function(otNumberChar) {
	if (otNumberChar == "1st") {
		if (.ap2$stn$aqq_nCoord == 12) {
			return(.ap2$aquagramPSettings$ot1$cns$cns12)
		} else {
			if (.ap2$stn$aqq_nCoord == 15) {
				return(.ap2$aquagramPSettings$ot1$cns$cns15)
			} else {
				stop("Please provide either '12' or '15' as the numbers of coordinates for the first overtone in the settings. Thank you.", call.=FALSE)
			}
		}		
	} # end 1st
} # EOF
##########################
##########################
aq_checkTempCalibRangeFromUnivFile <- function(TCalibRange, tempFile) {
	temp <- as.numeric(rownames(get(paste0(tempFile, "_univAucTable"), pos=.ap2)))
	if (all(TCalibRange >= min(temp)) & all(TCalibRange <= max(temp)) ) { ## to check if we are in the temperature-range of the calibration file
		return(TCalibRange)
	} else {
		message <- paste0("The requested temperature calibration range (", paste(TCalibRange, collapse=" to "), " deg.C.) is out of range of the available temperature data. \nPlease observe that the available temperature range in the selected temperature data file '", tempFile, "' in the AQUAP2SH folder is between ", min(temp), " and ", max(temp), " degrees celsius.")
		stop(message, call.=FALSE)
	}
} # EOF

aq_getTCalibRange <- function(ap, tempFile) {
	if (!is.null(ap$aquagr)) { 
		if (!haveClassicAqg(ap)) { 
			TCalib <- ap$aquagr$TCalib # ! can still be NULL
			Texp <- ap$aquagr$Texp
			# we did extensive checks before, so now everything should be correct
			if (!is.null(TCalib)) {
				if (is.character(TCalib)) {
					if (grepl("symm@", TCalib)) {
						a <- as.numeric(strsplit(TCalib, "@")[[1]][2])
						TCalib <- aq_checkTempCalibRangeFromUnivFile(c(Texp-a, Texp+a), tempFile)
					} else {
					#	TCalib <- aq_checkTempCalibRangeFromUnivFile(TCalib, tempFile)
					}
				} else {
					TCalib <- aq_checkTempCalibRangeFromUnivFile(TCalib, tempFile)
				}
			} else { # so TCalib is null
				if (!haveClassicAqg(ap)) { 
					temp <- as.numeric(rownames(get(paste0(tempFile, "_univAucTable"), pos=.ap2)))
					TCalib <- range(temp)
				} 
			}
			ap$aquagr$TCalib <- TCalib
			return(ap)
		} else {
			return(ap)
		}
	} else { # end if !is.null
		return(ap)
	}
	# yes, I know, this function is kind of messy, always just adapted and fixed. Sorry. :-)
} # EOF

aq_checkTCalibRange <- function(ap, tempFile) {
	ap <- aq_getTCalibRange(ap, tempFile)
	return(ap)
} # EOF

aq_cleanOutAllZeroRows <- function(dataset) {  
	a <- apply(dataset$NIR, 1, function(x) {
		all(x == 0)
	} )
	nonZeros <- which(a == FALSE)
	return(reFactor(dataset[nonZeros,]))
} # EOF

checkNrOfParticipants <- function(dataset, colInd, nrCorr) {
	classVar <- colnames(dataset$header)[colInd]
	charLevels <- levels(dataset$header[, colInd])
	nrPartOut <- NULL
#	selIndOut <- list()
	selRownamesOut <- NULL
	for (item in charLevels) {
		a <- ssc_s(dataset, classVar, item, keepEC=FALSE)
		nrPartOut <- c(nrPartOut,  nrow(a))
	}
	minPartic <- min(nrPartOut)
	for (i in 1: length(charLevels)) {
		a <- ssc_s(dataset, classVar, charLevels[i], keepEC=FALSE)
		if (nrCorr & (nrow(a) > minPartic) ) {
			selInd <- sample(1:nrow(a), minPartic)
		} else {
			selInd <- 1:nrow(a)
		}
		selRownamesOut <- c(selRownamesOut, rownames(a)[selInd])
#		selIndOut <- c(selIndOut, list(selInd))
	} # end for i
	selIndsDataset <- which(rownames(dataset) %in% selRownamesOut)
	return(list(nrPart=nrPartOut, selInds=selIndsDataset))
} # EOF

calcSpectra <- function(dataset, classVar, selInds, minus, plotSpectra) {
	avgSpec <- subtrSpec <- rawSpec <-  NULL
	a <- c("all", "raw")
	if (any(plotSpectra %in% a)) {
		rawSpec <- copy_aquagram_rawspectra(dataset, classVar, selInds)
	}
	a <- c("all", "avg", "subtr") # because we need the average for the subtraction
	if (any(plotSpectra %in% a)) {			
		avgSpec <- calc_avg_aquagram_spectra(dataset, classVar, selInds)
	}
	if (!is.null(minus)) {
		a <- c("all", "subtr")
		if (any(plotSpectra %in% a)) {
			subtrSpec <- calc_minus_avg_aquagram_spectra(avgSpec, minus)
		}
	}
	return(list(rawSpec=rawSpec, avgSpec=avgSpec, subtrSpec=subtrSpec))
} # EOF

aq_calculateCItable <- function(bootRes, groupAvg) {
	if (!is.null(bootRes)) {
		doSingleCol <- function(x, m, rNames) {
			sigOut <- NULL
			rnOut <- NULL
			for (i in 1:(m-1)) {
				for (k in (i+1):m) {
					siRn <- paste0(rNames[i], "~", rNames[k])
					first <- c(x[(i*2+1)-2], x[(i*2+2)-2])
					second <- c(x[(k*2+1)-2], x[(k*2+2)-2])
					if (min(first) > max(second) | max(first) < min(second)) {
						sig <- "*"	
					} else {
						sig <- ""
					}
					sigOut <- c(sigOut, sig)
					rnOut <- c(rnOut, siRn)
				} # end for k
			} # end for i
			out <- data.frame(sigOut)
			rownames(out) <- rnOut
			return(out)
		} # EOIF
		###
		bootResRed <- bootRes[-(seq(1, nrow(bootRes), by=3)),] # first kick out the avg value
		mm <- nrow(groupAvg)
		rns <- rownames(groupAvg)
		allColSigsList <- apply(bootResRed, 2, doSingleCol, m=mm, rNames=rns)
		outTable <- data.frame(rep(NA, nrow(allColSigsList[[1]])))
		for (i in 1: length(allColSigsList)) {
			outTable <- cbind(outTable, allColSigsList[[i]])
		}
		outTable <- outTable[,-1] # get rid of the NAs
		colnames(outTable) <- colnames(bootRes)
		return(outTable)	
		} else {
			return(NULL)
	} # end !is.null(bootRes)	
} # EOF

calcAquagramSingle <- function(dataset, md, ap, classVar, minus, idString, stnLoc) {
	##
	a <- ap$aquagr
	nrCorr <- a$nrCorr
	plotSpectra <- a$spectra
#	minus <- a$minus
	mod <- a$mod
	TCalib <- a$TCalib
	Texp <- a$Texp
	bootCI <- a$bootCI
	R <- a$R
	smoothN <- a$smoothN
	selWls <- a$selWls
	msc <- a$msc
	reference <- a$reference	
	##
	dataset <- aq_cleanOutAllZeroRows(dataset) 	# to avoid errors when all is 0 in a row when subtracting consecutive scans
	charLevels <- levels(dataset$header[, which(colnames(dataset$header) == classVar)])
	itemIndex <- which(charLevels == minus)	
	wls <- getWavelengths(dataset)
	selIndsWL <- which(wls %in% selWls)			 ## only used for "classic"
	if (!is.numeric(classVar)) {
		colInd <- which(colnames(dataset$header) == classVar)
	} else {
		colInd <- classVar
	}
	levelsOrder <- order(unique(dataset$header[, colInd]))
	colorInd <- which(colnames(dataset$colRep) == classVar)
	colRep <- unique(dataset$colRep[,colorInd])
	colRep <- colRep[levelsOrder]
	checkRes <- checkNrOfParticipants(dataset, colInd, nrCorr)
		possibleNrPartic <- possN <- checkRes$nrPart
		selInds <- checkRes$selInds
	dataset <- dataset[selInds,]  	### it might be reduced or not
	groupAverage <- avg <- as.matrix(calc_aquagr_CORE(dataset, smoothN, reference, msc, selIndsWL, colInd, mod, minus, TCalib, Texp))
	avgSpec <- subtrSpec <- rawSpec <-  NULL
	if (is.character(plotSpectra)) {
		a <- calcSpectra(dataset, classVar, selInds, minus, plotSpectra)
		rawSpec <- a$rawSpec
		avgSpec <- a$avgSpec
		subtrSpec <- a$subtrSpec
	} # end calc spectra
	if (bootCI) {
		if (.ap2$stn$aqg_bootUseParallel == TRUE) {
			if (Sys.info()["sysname"] == "Windows") {
				useMC <- "snow"
			} else {
				useMC <- "multicore"		
			}
			parChar <- "par."
		} else {
			useMC <- "no"
			parChar <- "ser."
		}
		bootRes <- try(calc_aquagr_bootCI(dataset, smoothN, reference, msc, selIndsWL, colInd, useMC, R, mod, minus, TCalib, Texp, parChar, stnLoc))
		if (class(bootRes) == "try-error") {
			bootRes <- NULL
		}
	} else {
		bootRes <- NULL
	} # end calc boot
#	aqRes <- new("aquCalc", ID, classVar, avg, numRep, possN, selInds, bootRes, rawSpec, avgSpec, subtrSpec) # ? does not work ?? 
	## now make a nice CI table comparing all groups against each other, for each WAMAC
	ciTable <- aq_calculateCItable(bootRes, groupAverage)
	##
	aqRes <- new("aqg_calc")
	aqRes@ID <- idString
	aqRes@classVar <- classVar
	aqRes@itemIndex <- itemIndex
	aqRes@avg <- groupAverage
	aqRes@colRep <- colRep
	aqRes@possN <- possibleNrPartic
	aqRes@selInds <- selInds
	aqRes@bootRes <- bootRes
	aqRes@ciTable <- ciTable
	aqRes@rawSpec <- rawSpec
	aqRes@avgSpec <- avgSpec
	aqRes@subtrSpec <- subtrSpec
	return(aqRes)
} # EOF
##########################
##########################


#' @title Generate temperature recording experiment
#' @description Generate the folder structure for a new experiment and populate 
#' it with the metadata suggested for recording then the temperature 
#' calibration-spectra used e.g. in the aquagram (see argument \code{aqg.TCalib} 
#' and \code{aqg.Texp} in \code{\link{calc_aqg_args}}).
#' @details This generates the folder structure for a standard experiment and is 
#' adapting the metadata to record spectra at various temperatures in each 3 
#' consecutive scans. For a possible workflow please see examples.
#' @param Tcenter Numeric length one. The temperature at which usually the 
#' measurements are performed. The final temperature will range from 
#' Tcenter-Tdelta to Tcenter+Tdelta, in steps given by argument 'stepsBy'. 
#' @param Tdelta Numeric length one, defaults to 5. The temperature range below 
#' and above 'Tcenter'.
#' @param stepBy Numeric length one, defaults to 1. The temperature step between 
#' each single temperature in the range from Tcenter-Tdelta to Tcenter+Tdelta.
#' @param repls Numeric length one. How many replicates of each single temperature 
#' to record. Defaults to 4.
#' @section Important: When exporting the sample list via \code{\link{esl}}, make 
#' sure to export it \strong{non randomized} - please see examples.
#' @section Warning: Do not change the name of the columns in the sample list 
#' before importing the dataset; if the numerical column \code{smpTemp} is not 
#' present, the temperature calibration data can not be used.
#' @family Temperature procedures
#' @examples
#' \dontrun{
#' genTempCalibExp(Tcenter=30) # generate the folder structure in the current 
#' working directory
#' esl(rnd=FALSE) # export a *non* randomized sample list
#' #### now record the temperature-spectra #### (move sample list to folder 'sl_in')
#' gfd <- gfd() # imports temperature raw-data and saves an R-data file in the 
#' # R-data folder, from where you take it and move it into your 
#' # AQUAP2SH folder
#' }
#' @seealso \code{\link{tempCalib_procedures}}, \code{\link{genFolderStr}}, 
#' \code{\link{genNoiseRecExp}} 
#' @family Helper Functions
#' @family Temperature calibration procedures
#' @export
genTempCalibExp <- function(Tcenter=NULL, Tdelta=5, stepBy=1, repls=4) {
	if(is.null(Tcenter)) {
		stop("Please provide a numeric value for 'Tcenter'.", call.=FALSE)
	}
	genFolderStr()
	fn_metadata <- .ap2$stn$fn_metadata # folder name for metadata
	fn_mDataDefFile <- .ap2$stn$fn_mDataDefFile
	deleteCol <- .ap2$stn$p_deleteCol
	clPref <- .ap2$stn$p_ClassVarPref
	yPref <- .ap2$stn$p_yVarPref
	#
	temps <- as.character(Tcenter + seq(-Tdelta, Tdelta, by=stepBy))
	temps <- rep(temps, each=repls)
	temps <- paste(temps, collapse="\",\"")
	#
	pathMd <- paste(fn_metadata, fn_mDataDefFile, sep="/")
	con <- file(pathMd, open="rt")
	txt <- readLines(con)
	close(con)
	txt <- mod_md_txt("expName", pv_initialTempCalibFilename, txt)
	txt <- mod_md_logic("TimePoints", FALSE, txt)
	txt <- mod_md_logic("spacing", FALSE, txt)
	txt <- mod_md_txt("columnNamesL1", paste0(yPref, pv_YcolumnNameSampleTemp), txt)
	txt <- mod_md_txt("columnNamesL2", paste0(clPref, "DELETE"), txt)
	txt[grep("L1  <-", txt)] <- paste0("\tL1  <- list(list(\"", temps, "\"))")
	txt[grep("L2  <-", txt)] <- paste0("\tL2  <- list(list(\"", temps, "\"))")
	txt <- mod_md_num("Repls", 1, txt)
	txt <- mod_md_num("nrConScans", 3, txt)
	txt <- mod_md_txt("Group", "no", txt)
	con <- file(pathMd, open="wt")
	writeLines(txt, con)
	close(con)
	return(invisible(NULL))
} # EOF



#' @title Record and use temperature calibration data
#' @description Record a special temperature dataset and use these data as a 
#' kind of calibration data for all of the aquagram calculations except the 
#' 'classic' and 'sfc' modes. In other words, you need the temperature dataset 
#' in order to be able to calculate Aquagrams of the 'auc'modes. It is strongly 
#' recommended that you do generate the temperature data and so can also use 
#' the advanced features of these AUC (area under curve) stabilized Aquagrams.
#' @details For generating a new experiment with all the necessary defaults to 
#' record the temperature data, please use \code{\link{genTempCalibExp}} (see 
#' examples there). After having recorded the spectra in their resp. temperature, 
#' import the raw data (\code{\link{gfd}}) and move the resulting R-data file 
#' into your \code{AQUAP2SH} folder. Now in the metadata of any experiment 
#' (parameter \code{tempCalibFileName}) or in the corresponding parameter in the 
#' settings file (\code{aqg_tempCalib_Filename}) provide the name of the R-data 
#' object that you moved into the \code{AQUAP2SH} folder. Whenever now an Aquagram 
#' is calculated, first and only once per R-session this temperate data file is 
#' read in and used to calculate the necessary data enabling the calculation of 
#' 'auc' Aquagrams. These temperature-datafile specific objects are stored in 
#' the environment \code{.ap2} (\code{ls(.ap2)}), starting with the name of the 
#' temperature data file followed by an '_' underscore.
#' @section Procedure: The procedure to work with a temperature-data file (or 
#' more of them of course) and use it to calculate area-under-curve 'auc' 
#' stabilized Aquagrams is as follows: 
#' \describe{
#' \item{Record temperature spectra}{Use the function 
#' \code{\link{genTempCalibExp}} to generate a folder structure for an experiment, 
#' export the sample list \strong{non randomized}, then record the 
#' temperature-spectra. Finally, use \code{\link{gfd}} to import the raw-data 
#' and create the R-data file (in the folder 'R-data' in the working directory 
#' of the experiment).}
#' \item{Move R-data file}{Move the resulting R-data file containing the 
#' temperature-data from the R-data folder into your \code{AQUAP2SH} folder, 
#' i.e. the folder also containing e.g. the settings.r file.}
#' \item{Specify temperature-data file}{In your actual experiment, specify the 
#' name of the file (residing in the folder \code{AQUAP2SH}) containing the 
#' temperature-spectra either in the metadata or at the argument \code{tempFile} in the function 
#' \code{\link{gdmm}} - see examples.}
#' \item{Choose to calculate an Aquagram}{In your actual experiment, choose to 
#' actually calculate an Aquagram and specify all the necessary parameters in the 
#' analysis procedure. You can override the values for the Aquagram-calculations 
#' via the \code{...} argument in the function \code{\link{getap}} in \code{\link{gdmm}} - please 
#' see examples and \code{\link{calc_aqg_args}}.}
#' }
#' @examples 
#' \dontrun{
#' fd <- gfd()
#' cube <- gdmm(fd)
#' cube <- gdmm(fd, tempFile="def") # to use the default from the settings file, 
#' # same as above
#' cube <- gdmm(fd, tempFile="FooBar") # use the temperature-data file 'FooBar'
#' # residing in the AQUAP2SH folder
#' cube <- gdmm(fd, getap(do.aqg=TRUE))
#' cube <- gdmm(fd, getap(do.aqg=TRUE, aqg.bootCI=TRUE))
#' cube <- gdmm(fd, getap(aqg.mod="aucs.dce"))
#' cube <- gdmm(fd, getap(aqg.mod="aucs.dce-diff", aqg.minus="C_Cont"), tempFile="FooBar2") 
#' }
#' @family Temperature procedures
#' @family Aquagram documentation
#' @seealso \code{\link{genTempCalibExp}}
#' @name tempCalib_procedures
NULL
