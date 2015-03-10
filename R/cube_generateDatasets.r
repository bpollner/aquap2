
makeSingleBlock <- function(ClassVar, header, metaData) {
	ind <- which(colnames(header)==ClassVar)
	levelsChar <- levels(header[,ind])
	classPrefix <- .ap2$stn$p_ClassVarPref
	strippedClassVar <- unlist(strsplit(ClassVar, classPrefix))[2]
	if (is.na(strippedClassVar)) {
		classPrefix <- .ap2$stn$p_yVarPref
		strippedClassVar <- unlist(strsplit(ClassVar, classPrefix))[2]
	}
	EClabel <- metaData$postProc$ECRMLabel[1]
#	if (get("stngs")$autoRemoveECs & (strippedClassVar != get("stngs")$ECRMCol))
	if (TRUE & (strippedClassVar != .ap2$stn$p_ECRMCol)) { 
		ind <- which(levelsChar==EClabel)
		if (length(ind) == 0) {		## so if is a logical(0) ((also possible: !length(ind) -- gives a TRUE or FALSE
			ind <- 0				## to avoid comparing against "integer(0)" in the next step
		}		
		if (ind > 0) { 		## so if there is the EC label in levelsChar
			levelsChar <- levelsChar[-ind]
		}
	}
	classCol <- matrix(ClassVar, nrow=length(levelsChar) , ncol=1)
	valueCol <- matrix(levelsChar)
	out <- cbind(classCol, valueCol)
	out
} # EOF

multiplyBlocks <- function(block1, block2) {
	multipliedNextBlock <- NULL
	for (i in 1: nrow(block2)) {
		a <- matrix(block2[i,1], nrow(block1))
		b <- matrix(block2[i,2], nrow(block1))
		d <- cbind(a,b)
		multipliedNextBlock <- rbind(multipliedNextBlock, d)
	} # end for i
	multipliedPrevBlock <- NULL
	for (k in 1: nrow(block2)) {
		multipliedPrevBlock <- rbind(multipliedPrevBlock, block1)
	} # end for k
	out <- cbind(multipliedPrevBlock, multipliedNextBlock)
} # EOF

makeSplitByClassFrame <- function(splitGroup, header, metaData) { ## makes a matrix with split-values in a row
	firstBlock <- multBlock <- makeSingleBlock(splitGroup[1], header, metaData)
	if (length(splitGroup) > 1) {
		previousBlock <- firstBlock
		for (i in 2: length(splitGroup)) {			## go through all the items in the split-group
			ClassVar <- splitGroup[i]
			nextBlock <- makeSingleBlock(ClassVar, header, metaData)
			multBlock <- multiplyBlocks(previousBlock, nextBlock)
			previousBlock <- multBlock
			} # end for i
	} # end if > 1
	colnames(multBlock) <- rep(c("class", "value"), ncol(multBlock)/2)
#	classInd <- seq(1, ncol(multBlock), by=2)
#	valueInd <- seq(2, ncol(multBlock), by=2)
#	classes <- multBlock[, classInd]
#	values <- multBlock[, valueInd]
	return(multBlock)
} # EOF

multiplyByWlSplits <- function(splitClassFrame, analysisProcedure) {
	multClassFrame <- NULL
	wlChar <- analysisProcedure$ucl$splitWl
	for (k in 1: length(wlChar)) {
		multClassFrame <- rbind(multClassFrame, splitClassFrame)
	} #end for k
	a <- rep(wlChar, each=nrow(splitClassFrame))
	wlCol <- data.frame(wlSplits=a)
	out <- cbind(multClassFrame, wlCol)
	return(out)
} # EOF

multiplyByBinary <- function(frame, useRaw, useApplied, colname) {
	if (useRaw==TRUE & useApplied==TRUE) {
		multFrame <- rbind(frame, frame)
		chars <- rep(c("no", "yes"), each=nrow(frame))
		logic <- rep(c(FALSE, TRUE), each=nrow(frame))
	} else {
		if (useRaw==FALSE & useApplied==TRUE) {
			chars <- rep("yes", nrow(frame))
			logic <- rep(TRUE, nrow(frame))
			multFrame <- frame			
		} else {
			if (useRaw==TRUE & useApplied==FALSE) {
				chars <- rep("no", nrow(frame))
				logic <- rep(FALSE, nrow(frame))
				multFrame <- frame			
			} else {
				if (useRaw==FALSE & useApplied==FALSE) {
					stop("Check settings for smoothing please. One has to be TRUE.", call.=FALSE)
				}
			}
		}
	}
	dfChars <- data.frame(X=chars)
	colnames(dfChars) <- colname
	dfLogic <- data.frame(X=logic)
	colnames(dfLogic) <- paste(colname, "Logic", sep="")
	out <- cbind(multFrame, dfChars, dfLogic)
out
} # EOF

makeCPT <- function(patt) {
	aquapCPT <- new("aquap_cpt")
	a <- which(colnames(patt) == "class")
	classes <- patt[, a]
	a <- which(colnames(patt) == "value")
	values <- patt[, a]
	aquapCPT@splitVars <- list(classes=classes, values=values)
	a <- as.character(patt[, which(colnames(patt) == "wlSplits")])
	aquapCPT@wlSplit <- lapply(strsplit(a, "-to-"), as.numeric) 
	aquapCPT@smoothing <- patt[, which(colnames(patt) == "SmoothingLogic")]	
	aquapCPT@noise <- patt[, which(colnames(patt) == "NoiseLogic")]
	aquapCPT@len <- nrow(patt)
	return(aquapCPT)
} # EOF

cleanCPPattern <- function(cp) {
	ind <- grep("Logic", colnames(cp))
	return(cp[,-ind])
} #	EOF

makeCompPattern <- function(header, md, ap) {
	splitGroup <- ap$ucl$splitClasses
	#
	patt <- makeSplitByClassFrame(splitGroup, header, md)
	#
	patt <- multiplyByWlSplits(patt, ap)
	#
	useRaw  <- ap$dpt$smoothing$useRaw
	useSmooth <- ap$dpt$smoothing$useSmooth
	patt <- multiplyByBinary(patt, useRaw, useSmooth, "Smoothing")
	#
	useRaw  <- ap$dpt$noise$useRaw
	useNoise <- ap$dpt$noise$useNoise
	patt <- multiplyByBinary(patt, useRaw, useNoise, "Noise")
	#
	cpt <- makeCPT(patt)
	cp <- cleanCPPattern(patt)
	return(list(cp=cp, cpt=cpt))
} # EOF

ap_cleanOutZeroValues <- function(ap, dataset) {
	cPref <- .ap2$stn$p_ClassVarPref
	yPref <- .ap2$stn$p_yVarPref
	noSplitCol <- .ap2$stn$p_commonNoSplitCol
	if (is.null(ap$ucl$splitClasses)) {
		stdCol <- paste(cPref, noSplitCol, sep="")
		ind <-which(colnames(dataset$header) == stdCol)
		if (length(ind) == 0) {
			stop("Sorry, it appears that you do not have the standard column \"", stdCol, "\" in your dataset.", call.=FALSE)
		}
		ap$ucl$splitClasses <- stdCol
	}
	if (is.null(ap$ucl$splitWl)) {
		wls <- getWavelengths(dataset)
		ap$ucl$splitWl <- paste(min(wls), "-to-", max(wls), sep="")
	}
	le <- nchar(cPref)
	cns <- colnames(dataset$header)
	cnsM <- substr(cns, 1, le)
	ind <- which(cnsM == cPref)
	if (is.null(ap$pca$colorBy)) {
		ap$pca$colorBy <- cns[ind]
	}
	if (is.null(ap$simca$simcOn)) {
		ap$simca$simcOn <- cns[ind]
	}
	if (is.null(ap$plsr$regressOn)) {
		le <- nchar(yPref)
		cnsM <- substr(cns, 1, le)
		ind <- which(cnsM == yPref)
		ap$plsr$regressOn <- cns[ind]
	}
	return(ap)
} # EOF

ap_checExistence <- function(ap, dataset) {
	cPref <- .ap2$stn$p_ClassVarPref
	yPref <- .ap2$stn$p_yVarPref
	cns <- colnames(dataset$header)
	checkEx <- function(charVec, where, what) {
		a <- which(!charVec %in% cns)
		if (length(a) != 0) {
			vars <- paste(charVec[a], collapse=", ")
			stop(paste("Sorry, the variable \"", vars, "\" appears not to exist in your dataset. \nPlease check the ", where, " part of the analysis procedure.", sep=""), call.=FALSE)
		}
		a <- substr(charVec, 1, nchar(what))
		if (any(a != what)) {
			stop(paste("Not all of the provided variables are of the the required type \"", what, "\".\nPlease check the ", where, " part of the analysis procedure.", sep=""), call.=FALSE)
		}
	} # EOIF
	checkEx(ap$ucl$splitClasses, "variable split", cPref)
	checkEx(ap$pca$colorBy, "PCA", cPref)
	checkEx(ap$simca$simcOn, "SIMCA", cPref)
	checkEx(ap$plsr$regressOn, "PLSR (regress on)", yPref)
	checkEx(ap$plsr$colorBy, "PLSR (color by)", cPref)
	if (!is.null(ap$aquagr$vars)) {
		checkEx(ap$aquagr$vars, "Aquagram", cPref)
	}
	wls <- getWavelengths(dataset)
	splitWl <- ap$ucl$splitWl
	for (i in 1: length(splitWl)) {
		options(warn=-1)
		nums <- as.numeric(strsplit(splitWl[i], "-to-")[[1]])
		options(warn=0)
		if (any(is.na(nums))) {
			stop(paste("Please check the analysis procedure at the wavelength-split input; provide the wavelength split in the format like e.g. '1300-to-1600'."), call.=FALSE)
		}
		if (min(nums) < min(wls) | max(nums) > max(wls)) {
			stop(paste("Sorry, the specified wavelengths \"", splitWl[i], "\" are out of the available range.", sep=""), call.=FALSE)
		}
	}
} # EOF

#' @title Generate Datasets and make Models
#' @description Generate several datasets by splitting up the original dataset 
#' according to the variables and values provided in the analysis procedure and 
#' then calculate the models as specified in the analysis procedure on all of the 
#' datasets.
#' @details XXX
#' @param dataset An object of class 'aquap_data'
#' @param md The metadata, an object of class 'aquap_md'
#' @param ap The analysis procedure, an object of class 'aquap_ap'
#' @return XXX
#' @family Core functions
#' @export
gdmm <- function(dataset, md=getmd(), ap=getap()) {
	autoUpS()
	ap <- ap_cleanOutZeroValues(ap, dataset)
	ap_checExistence(ap, dataset)
	a <- makeCompPattern(dataset$header, md, ap)
	cp <- a$cp
	cpt <- a$cpt
	len <- cpt@len
	cube <- new("aquap_cube")
	##
	for (i in 1:len) {
		# now make all the datasets according to the specifications in cpt
	} # end for i
	##
	cube@metadata <- md
	cube@anproc <- ap
	cube@cp <- cp
	cube@cpt <- cpt
	return(cube)
} # EOF
