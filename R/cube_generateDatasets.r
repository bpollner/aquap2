
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
	classes <- as.data.frame(patt[, a])
	colnames(classes) <- rep("classes", ncol(classes))
	a <- which(colnames(patt) == "value")
	values <- as.data.frame(patt[, a])
	colnames(values) <- rep("values", ncol(values))
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
	if (!is.null(ap$pca)) {
		if (is.null(ap$pca$colorBy)) {
			ap$pca$colorBy <- cns[ind]
		}
	}
	if (!is.null(ap$simca)) {
		if (is.null(ap$simca$simcOn)) {
			ap$simca$simcOn <- cns[ind]
		}
	}
	if (!is.null(ap$plsr)) {
		if (is.null(ap$plsr$regressOn)) {
			le <- nchar(yPref)
			cnsM <- substr(cns, 1, le)
			ind <- which(cnsM == yPref)
			ap$plsr$regressOn <- cns[ind]
		}
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

#' @title Select Wavelengths
#' @description Select wavelenghts from a dataset.
#' @details It is not necessary to exactly know the numbers of the wavelengths 
#' present in the dataset, as for the lower limit the same or the next highest 
#' wavelength as \code{from}, and for the upper limit the same or the next 
#' lowest wavelength as \code{to} will be selected.
#' @param dataset An object of class 'aquap_data'
#' @param from Numeric length one. The lower limit of the wavelengths to be 
#' selected.
#' @param to Numeric lengtho one. The upper limit of the wavelengths to be 
#' selected.
#' @return The standard dataset (class 'aquap_data')
#' @family Data pre-preatment functions
#' @export
selectWls <- function(dataset, from, to) {
	wls <- getWavelengths(dataset)
	lowInd <- min(which(wls >= from)) # so we do not have to know the exact wavelength
	highInd <- max(which(wls <= to))
	NIRsel <- dataset$NIR[,lowInd:highInd]
	dataset$NIR <- NIRsel
	return(dataset)
} # EOF

performSmoothing <- function(dataset, siSmooth) {
	if (siSmooth) {
		sgp <- .ap2$stn$sm_savGolayOrder_p
		sgn <- .ap2$stn$sm_savGolayLength_n
		sgm <- .ap2$stn$sm_savGolayDeriv_m 
		if (!.ap2$stn$allSilent) {cat("      smoothing...")}
		dataset$NIR <- t(apply(dataset$NIR, 1, signal::sgolayfilt, p=sgp, n=sgn, m=sgm))
		if (!.ap2$stn$allSilent) {cat(" ok\n")}
	}
	return(dataset)
} # EOF

performNoise <- function(dataset, siNoise) {
	if (siNoise) {
		noiseLevel <- .ap2$stn$noi_noiseLevel
		if (!.ap2$stn$allSilent) {cat("      adding noise...")}
		dataset$NIR <- dataset$NIR * rnorm(1, mean=1, sd=noiseLevel)
		if (!.ap2$stn$allSilent) {cat(" ok\n")}
	}
	return(dataset)
} # EOF

makeIdString <- function(siClass, siValue, siWlSplit, siSmooth, siNoise) {
	# siClass and siValue come in as dataframes with one row and one or more columns
	varString <- NULL
	for (i in 1: ncol(siClass)) {
		varString <- paste(varString, siClass[1,i], ":", siValue[1,i], ", ", sep="")
	}
	varString <- substr(varString, 1, (nchar(varString)-2)) # to get rid of the last ","
	varString <- paste(varString, " ", sep="")
	more <- FALSE
	if (siSmooth) {
		more <- TRUE
		smoothing <- "smo. "
	} else {
		smoothing <- ""
	}
	if (siNoise) {
		more <- TRUE
		noise <- "noise "
	} else {
		noise <- ""
	}
	if (more) {
		moreAdd <- ", "
	} else {
		moreAdd <- ""
	}
	easy <- paste(varString, "@", siWlSplit[1], "-to-", siWlSplit[2], moreAdd, smoothing, noise, sep="")
	return(easy)
} # EOF

processSingleRow_CPT <- function(dataset, siClass, siValue, siWlSplit, siSmooth, siNoise) { # si for single
	keepEC <- .ap2$stn$gd_keepECs
	newDataset <- ssc_s(dataset, siClass, siValue, keepEC) 
	if (is.null(newDataset)) { # NULL is returned if a variable combination yields no data
		return(NULL)
	}
	newDataset <- selectWls(newDataset, siWlSplit[1], siWlSplit[2])
	newDataset <- performSmoothing(newDataset, siSmooth)
	newDataset <- performNoise(newDataset, siNoise)
	idString <- makeIdString(siClass, siValue, siWlSplit, siSmooth, siNoise)
	out <- new("aquap_set", dataset=newDataset, idString=idString)
	return(out)
} # EOF

correct_cpt <- function(cpt, nullInd) {
	n <- nullInd
	cpt@splitVars$classes <- cpt@splitVars$classes[-n,]
	cpt@splitVars$values <- cpt@splitVars$values[-n,]
	cpt@wlSplit <- cpt@wlSplit[-n]
	cpt@smoothing <- cpt@smoothing[-n]
	cpt@noise <- cpt@noise[-n]
	cpt@len <- length(cpt@noise)
	return(cpt)
} # EOF

printIds <- function(cube) {
	for (i in 1: cube@cpt@len) {
		print(cube[[i]]@idString)
	}
} # EOF

#' @rdname gdmm
#' @export
gdmm <- function(dataset, ap=getap(), md=getmd() ) {
	autoUpS()
	ap <- ap_cleanOutZeroValues(ap, dataset)
	ap_checExistence(ap, dataset)
	a <- makeCompPattern(dataset$header, md, ap)
	cp <- a$cp
	cpt <- a$cpt
	len <- cpt@len
	cubeList <- list()
	length(cubeList) <- len
	##
	classes <- cpt@splitVars$classes # ! is a data frame
	values <- cpt@splitVars$values # !is a data frame
	wlSplit <- cpt@wlSplit # ! is a list
	smoothing <- cpt@smoothing # is a vector
	noise <- cpt@noise # is a vector
	
	### generate datasets
	if (!.ap2$stn$allSilent) {cat("Generating Datasets...\n")}
	for (i in 1:len) {
		if (!.ap2$stn$allSilent) {cat(paste("   Working on #", i, " of ", len, "\n", sep=""))}
		cubeList[[i]] <- processSingleRow_CPT(dataset, as.data.frame(classes[i,]), as.data.frame(values[i,]), wlSplit[[i]], smoothing[i], noise[i]) # the "as.data.frame" is necessary if we only have one column
	} # end for i
	if (!.ap2$stn$allSilent) {cat("Done.\n")}
	###
	
	
	### clean out the NULL-datasets
	nullInd <- which(unlist(lapply(cubeList, is.null))) # which one of the split by variable resulted in zero rows (has been returned as NULL in ssc_s)
	if (length(nullInd) > 0) {
		cubeList <- cubeList[-nullInd]
		cp <- cp[-nullInd,]
		rownames(cp) <- 1:nrow(cp)
		cpt <- correct_cpt(cpt, nullInd)
		message(paste(length(nullInd), " of the split-by-variable combinations resulted in no data. Those datasets have been omitted.", sep=""))
	}
	###
	
	### now make the models (so far, we have only the dataset and the id-string in the set)
	if (!.ap2$stn$allSilent) {cat("\nCalculating models...\n")}
	for (i in 1: cpt@len) {
		if (!.ap2$stn$allSilent) {cat(paste("   Working on dataset #", i, " of ", cpt@len, "\n", sep=""))}
		cubeList[[i]] <- makeAllModels(cubeList[[i]], md, ap)
	} # end for i
	if (!.ap2$stn$allSilent) {cat("Done.\n")}
	###
	
	##
	cube <- new("aquap_cube", cubeList)
	cube@metadata <- md
	cube@anproc <- ap
	cube@cp <- cp
	cube@cpt <- cpt
	return(cube)
} # EOF
