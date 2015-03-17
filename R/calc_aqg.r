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
		stop("I am sorry, please provide a valid value for 'minus' to perform subtractions within the aquagram. Thanks.", call.=FALSE)
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
		stop("I am sorry, please provide a valid value for 'minus' to perform subtractions within the aquagram. Thanks.", call.=FALSE)
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
		stop("I am sorry, please provide a valid value for 'minus' to perform subtractions within the aquagram. Thanks.", call.=FALSE)
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
		stop("I am sorry, please provide a valid value for 'minus' to perform subtractions within the aquagram. Thanks.", call.=FALSE)
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
		stop("I am sorry, please provide a valid value for 'minus' to perform subtractions within the aquagram. Thanks.", call.=FALSE)
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
		stop("I am sorry, please provide a valid value for 'minus' to perform subtractions within the aquagram. Thanks.", call.=FALSE)
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

calc_aquagr_bootCI <- function(dataset, smoothN, reference, msc, selIndsWL, colInd, useMC, R, mod, minus, TCalib, Texp) {
	path <- paste(get("stngs")$analysisData, "bootResult", sep="")
	saveBootResult <- get("stngs")$aquagr_saveBootRes
	innerWorkings <- function(x, ind) {
		out <- as.matrix(calc_aquagr_CORE(x[ind,], smoothN, reference, msc, selIndsWL, colInd, mod, minus, TCalib, Texp))
	} # EOIF
	if (!get("stngs")$allSilent) {cat(paste("      Calculating", R, "bootstrap replicates... \n")) }
	thisR <- R
	bootResult <- boot::boot(dataset, innerWorkings, R=thisR, strata=dataset[,colInd], parallel=useMC, ncpus=get("stngs")$numberOfCPUs)   	### here the bootstrap replicates happen
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
	if (!get("stngs")$allSilent) {cat("      Calculating confidence intervals... \n")}
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
 
tempCalibReadRaw <- function() {
	if (exists(".devMode", envir=.ap2)) {
		filepath <- .ap2$.devDataPath
		filepath <- paste(filepath, "tcalib", sep="") 	## tcalib is an r-file manually prepared from the temperature calibration data
	} else {
		a <- path.package("aquap2")
		File <- "/pData/tcalib"
		filepath <- paste(a, File, sep="")
	}
	if (!.ap2$stn$allSilent) {cat(" * Reading in calibration data... ")}
	load(filepath)
	out <- get("tcalib")
#	rm(tcalib)
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
	b <- which(wls %in% ot)
	spectOt <- spect[, b[1]:b[2]]
	wlsOt <- wls[b[1]:b[2]]
	colnames(spectOt) <- paste("w", wlsOt, sep="")
	spectOt
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
	a <- which(wls %in% OverTone) # XXX modify this for better safety !!! XXXMODXXX
	spectOt <- spect[, a[1]:a[2]]
	wlsOt <- wls[a[1]:a[2]]
	Tnames <- fdata$temp_W
	avgSpect <- plyr::ddply(as.data.frame(spectOt), plyr::.(Tnames), plyr::colwise(mean)) ## average the single spectra to one row with a single temperature each
	rownames(avgSpect) <- TnamesAvg <- avgSpect[,1]
#	colnames(avgSpect) <- paste("w", wlsOt, sep="")
	return(avgSpect[, -1])
} # EOF

calcUnivAucTable <- function(dataset, smoothN=17, ot=c(1300, 1600)) {
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
			a <- which(wls %in% Call[k,]) ## indices of the boundaries of the wavelength of the current coordinate
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
} # EOF

getTempNormAUCPercTable <- function(univAucTable, Texp, aucExtrema) {
	aucd <- univAucTable
	temp <- as.numeric(rownames(aucd))
	out <- NULL
	for (i in 1: ncol(aucd)) {
		loessMod <- loess(aucd[,i] ~ temp, family ="symmetric")
		a <- matrix(predict(loessMod, Texp), ncol=1)
		out <- cbind(out, a)
	}
	normPerc <- calcAUCPercent(out, aucExtrema)
} # EOF

## gets called once at the first call to sdrc_plotAquagram
aq_loadGlobalAquagramCalibData <- function() {
	if (!exists("tcd", where=.ap2)) {
	assign("tcd", tempCalibReadRaw(), pos=.ap2) 
	}
	if (!exists("aquagramPSettings",  where=.ap2)) {
	assign("aquagramPSettings", readInAquagramPSettings(), pos=.ap2)
	}
	if (!exists("univAucTable", where=.ap2)) {
	aut <-  calcUnivAucTable(.ap2$tcd, smoothN=.ap2$stn$aqg_smoothCalib, ot=getOvertoneCut(.ap2$stn$aqg_OT))
	assign("univAucTable", aut, pos=.ap2)
	}
} # EOF

## gets called inside the aquagram
aq_makeGlobals <- function(dataset, TCalib, Texp, ot, smoothN) {
	assign("tempCalibFCtable", tempCalibMakeTable(dataset, TCalib, ot), pos=.ap2)	# probably only used for mode "sfc"	
	assign("aucEx", getAUCcalibExtrema(.ap2$univAucTable, TCalib), pos=.ap2)
	assign("tempNormAUCPerc", getTempNormAUCPercTable(.ap2$univAucTable, Texp, .ap2$aucEx), pos=.ap2)
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
	load(filepath)
	out <- get(".aquagramPSettings")
#	rm(.aquagramPSettings)
	return(out)
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
aq_checkTempCalibRangeFromUnivFile <- function(TCalibRange) {
	temp <- as.numeric(rownames(.ap2$univAucTable))
	if (all(TCalibRange >= min(temp)) & all(TCalibRange <= max(temp)) ) { ## to check if we are in the temperature-range of the calibration file
		return(TCalibRange)
	} else {
		message <- paste("Please observe that the current available temperature range for calibration data is between", min(temp), "and", max(temp), "degrees celsius.\n", sep=" ")
		stop(message, call.=FALSE)
	}
} # EOF

aq_getTCalibRange <- function(ap) {
	TCalib <- ap$aquagr$TCalib
	Texp <- ap$aquagr$Texp
	# we did extensive checks before, so now everything should be correct
	if (is.character(TCalib)) {
		if (grepl("symm@", TCalib)) {
			a <- as.numeric(strsplit(TCalib, "@")[[1]][2])
			TCalib <- aq_checkTempCalibRangeFromUnivFile(c(Texp-a, Texp+a))
		} else {
			if (!is.null(TCalib)) {
				TCalib <- aq_checkTempCalibRangeFromUnivFile(TCalib)
			}
		}
	}
#	stop("Please provide either: \n -) a character 'symm@x' (with x being a number) for automatic symmetric picking of the calibration range x degrees plus and minus from 'Texp', the temperature of the experiment; \n -) or a numeric length two [c(x1,x2)] for manual input of the temperature calibration range \nin the 'TCalib' argument. Thank you.", call.=FALSE)
	ap$aquagr$TCalib <- TCalib
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

calcAquagramSingle <- function(dataset, md, ap, classVar, idString) {
	##
	a <- ap$aquagr
	nrCorr <- a$nrCorr
	plotSpectra <- a$spectra
	minus <- a$minus
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
#	groupAverage <- avg <- calc_aquagr_CORE(dataset, smoothN, reference, msc, selIndsWL, colInd, mod, minus, TCalib, Texp)
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
		} else {
			useMC <- "no"
		}
		bootRes <- try(calc_aquagr_bootCI(dataset, smoothN, reference, msc, selIndsWL, colInd, useMC, R, mod, minus, TCalib, Texp))
		if (class(bootRes) == "try-error") {
			bootRes <- NULL
		}
	} else {
		bootRes <- NULL
	} # end calc boot
#	aqRes <- new("aquCalc", ID, classVar, avg, numRep, possN, selInds, bootRes, rawSpec, avgSpec, subtrSpec) # ? does not work ?? 
	aqRes <- new("aqg_calc")
	aqRes@ID <- idString
	aqRes@classVar <- classVar
	aqRes@itemIndex <- itemIndex
	aqRes@avg <- groupAverage
	aqRes@colRep <- colRep
	aqRes@possN <- possibleNrPartic
	aqRes@selInds <- selInds
	aqRes@bootRes <- bootRes
	aqRes@rawSpec <- rawSpec
	aqRes@avgSpec <- avgSpec
	aqRes@subtrSpec <- subtrSpec
	return(aqRes)
} # EOF

collectRanges <- function(aquCalcRes, lengthClasses) {
	ranAvg <- ranBootRes <- ranSubtrSpec <- NULL
	ranColl <- list() # the range collection
	length(ranColl) <- lengthClasses
	iwo <- function(x, xslot, varn=NULL) {
		if (!is.null(slot(x, xslot))) {
			if (is.null(varn)) {
				return(range(slot(x, xslot)))
			} else {
				a <- slot(x, xslot)
				return(range(a[varn]))
			}
		} else {
			return(1212)
		}
	} # EOIF
	owo <- function(classObj, xslot, varn=NULL) {
		a <- range(sapply(classObj, iwo, xslot, varn))
		if (a[1] == 1212) {
			return(NULL)
		} else {
			return(a)
		}
	} # EOIF
	for (i in 1: lengthClasses) {
			ranAvg <- owo(aquCalcRes[[i]], "avg")
			ranBootRes <-  owo(aquCalcRes[[i]], "bootRes")
			ranSubtrSpec <-  owo(aquCalcRes[[i]], "subtrSpec", varn="NIR")
			ranColl[[i]] <- list(ranAvg=ranAvg, ranBootRes=ranBootRes, ranSubtrSpec=ranSubtrSpec)
	} # end for i
	return(ranColl)
} # EOF
