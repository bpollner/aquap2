
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

ap_checkAquagramDefaults <- function(ap, header) {
	if (!is.null(ap$aquagr)) {
		a <- ap$aquagr
		###
		nrCorr <- a$nrCorr
		if (all(nrCorr == "def")) {
			nrCorr <- .ap2$stn$aqg_correctNrOfObs
		}
		if (!is.logical(nrCorr)) {
			stop("Please provide either 'def', 'TRUE' or 'FALSE' for the argument 'aqg.nrCorr'.", call.=FALSE)
		}
		ap$aquagr$nrCorr <- nrCorr
		###
		spectra <- a$spectra
		pvSubSpectra <- c("subtr", "all") # XXXVARXXX
		if (any(spectra != FALSE)) {
			possibleValues <- c("raw", "avg", "subtr", "all") # XXXVARXXX
			if (!any(spectra %in% possibleValues) | all(spectra == TRUE)) {
				stop("Please provide either 'FALSE', or one or more of 'raw', 'avg', 'subtr', or 'all' to the argument 'aqg.spectra'.", call.=FALSE)
			}
			if ((any(spectra %in% pvSubSpectra)) & is.null(a$minus)) {
				stop("You have to provide a value for 'aqg.minus' in order to plot subtracted spectra", call.=FALSE)
			}
		}
		###
		mod <- a$mod
		possibleValues <- pv_AquagramModes
		if (all(mod=="def")) {
			mod <- .ap2$stn$aqg_defaultMod
		}
		if (!any(mod %in% possibleValues) | length(mod) !=1 ) {
			stop(paste("Please provide one of \n", paste(possibleValues, collapse=", "), "; \nor 'def' for reading in the default from the settings.r file to the argument 'aqg.mod'.", sep=""), call.=FALSE)
		}		
		ap$aquagr$mod <- mod
		###
		minus <- a$minus
		if (!is.null(minus)) {
			if (!all(is.character(minus)) | length(minus) != 1) {
				stop("Please provide a character length one for the argument 'aqg.minus'", call.=FALSE)
			}
			groupingVars <- a$vars
			for (i in 1: length(groupingVars)) {
				ind <- which(colnames(header) == groupingVars[i])
				levelsChar <- levels(header[,ind])
				if ((!minus %in% levelsChar) & grepl("diff", mod) ) {
					stop(paste("Sorry, it appears that the provided value \"", minus, "\" for the argument 'aqg.minus' is not present in the Aquagram grouping variable \"", groupingVars[i], "\". \nPlease check your input at 'aqg.minus' and 'aqg.vars'", sep=""), call.=FALSE)
				}
				if ((!minus %in% levelsChar) & any(spectra %in% pvSubSpectra)) {
					stop(paste("Sorry, you have to provide a value for 'aqg.minus' in order to plot subtracted spectra"), call.=FALSE)
				}
			} # end for i
		}
		###
		if (grepl("diff", mod) & is.null(minus)) {
			stop(paste("Please provide a value for 'minus' to perform subtractions within the aquagram."), call.=FALSE)
		}
		###
		TCalib <- a$TCalib
		if (all(TCalib=="def")) {
			TCalib <- .ap2$stn$aqg_calibTRange
		}
		if (!is.null(TCalib)) {
			if (!any(grepl("symm@", TCalib)) ) {
				if (!all(is.numeric(TCalib)) | length(TCalib) !=2) {
					stop("Please provide either 'def', 'symm@x' with x being a numeric, or a length 2 numeric for the argument 'aqg.TCalib'.", call.=FALSE)	
				}
			} else { # so we have "symm@" 
				options(warn=-1)
				nums <- as.numeric(unlist(strsplit(TCalib, "@")))[2]
				options(warn=0)
				if (any(is.na(nums))) {
					stop(paste("Please provide an input in the format 'symm@x', with x being a numeric, to the argument 'aqg.TCalib'"), call.=FALSE)
				}
			}
		}
		ap$aquagr$TCalib <- TCalib
		###
		Texp <- a$Texp
		if (all(Texp=="def")) {
			Texp <- .ap2$stn$aqg_Texp
		}
		if (!is.numeric(Texp) | (length(Texp) != 1) ) {
			stop("Please provide either 'def', or a length 1 numeric for the argument 'aqg.Texp'.", call.=FALSE)			
		}
		ap$aquagr$Texp <- Texp
		###
		bootCI <- a$bootCI
		if (all(bootCI=="def")) {
			bootCI <- .ap2$stn$aqg_bootCI
		}
		if (!is.logical(bootCI)) {
			stop("Please provide either 'def', 'TRUE' or 'FALSE' for the argument 'aqg.bootCI'.", call.=FALSE)		
		}
		ap$aquagr$bootCI <- bootCI	
		###
		if (nrCorr & bootCI) {
			msg <- ("WARNING: \nPerforming number correction AND bootstrap on the aquagram can lead to unrepresentative / irreal graphics \n------\nDo you want to continue anyway? \n( y / n)")
			message(msg)
			a <- readLines(n=1)
			if (a != "y" & a != "Y") {
				stop("'gdmm' aborted.", call.=FALSE)		
			}	
		}
		###
		R <- a$R
		if (all(R=="def")) {
			R <- .ap2$stn$aqg_bootR
		}
		if (!grepl("nrow@", R)) {
			if (!is.numeric(R) | (length(R)!=1)) {
				stop("Please provide either 'def', 'nrow@x' (with x being a number),  or a length 1 numeric for the argument 'aqg.R'.", call.=FALSE)			
			}
		}
		if (!is.numeric(R)) {
			num <- as.numeric(unlist(strsplit(R, "@"))[2])
			R <- round(nrow(header) * num, 0)
		}
		ap$aquagr$R <- R
		###
		if (!is.numeric(a$smoothN) | length(a$smoothN)!=1) {
			stop("Please provide an odd length 1 numeric to the argument 'aqg.smoothN'", call.=FALSE)
		}
		###
		selWls <- a$selWls
		if (all(selWls=="def")) {
			selWls <- .ap2$stn$aqg_wlsAquagram
		}
		if ( !all(is.numeric(selWls)) ) {
			stop("Please provide either 'def',  or a numeric vector for the argument 'aqg.selWls'.", call.=FALSE)			
		}
		ap$aquagr$selWls <- selWls
		###
		if (!is.logical(a$msc)) {
			stop("Please provide either TRUE or FALSE to the argument 'aqg.msc'", call.=FALSE)
		}
		###
		fsa <- a$fsa
		if (all(!is.logical(fsa)) & any(fsa!=TRUE)) {
			if (!is.character(fsa)) {
				if (!all(is.numeric(fsa)) | length(fsa)!=2) {
					stop("Please provide a numeric length two as argument for 'aqg.fsa' to manually provide a fix scale for the aquagrams.", call.=FALSE)
				}
			} else {
				pv <- pv_fsa_fss # ("both", "only")
				if (!any(fsa %in% pv)) {
		 			stop(paste("Please provide one of '", paste(pv, collapse="', '"), "' to the argument 'aqg.fsa'.", sep=""), call.=FALSE)
			 	}
			}
		}
		###
		fss <- a$fss
		if (all(!is.logical(fss)) & any(fss!=TRUE)) {
			if (!is.character(fss)) {
				if (!all(is.numeric(fss)) | length(fss)!=2) {
					stop("Please provide a numeric length two as argument for 'fss' to manually provide a fix scale for the subtraction spectra.", call.=FALSE)
				}
			} else {
				pv <- pv_fsa_fss # ("both", "only")
				if (!any(fss %in% pv)) {
		 			stop(paste("Please provide one of '", paste(pv, collapse="', '"), "' to the argument 'aqg.fss'.", sep=""), call.=FALSE)
				}
			}
		}
		###	
		# the custom color aqg.ccol remains unchecked for now
		###
		clt <- a$clt
		if (all(clt=="def")) {
			clt <- .ap2$stn$aqg_linetypes
		}
		msg <- "Please provide an integer vector to the argument 'aqg.clt'."
		if ( !all(is.numeric(clt)) ) {
			stop(msg, call.=FALSE)			
		}
		if (any(!is.wholenumber(clt)) ) {
			stop(msg, call.=FALSE)
		}
		ap$aquagr$clt <- clt
		###
		pplot <- a$pplot
		if (all(pplot=="def")) {
			pplot <- .ap2$stn$aqg_adPeakPlot
		}
		if (!all(is.logical(pplot)) | length(pplot) != 1) {
			stop("Please provide either 'def', or 'TRUE' or 'FALSE' to the argument 'aqg.pplot'", call.=FALSE)
		}
		ap$aquagr$pplot <- pplot
		###
		plines <- a$plines
		if (all(plines=="def")) {
			plines <- .ap2$stn$aqg_AdLines
		}
		msg <- "Please provide an integer vector to the argument 'aqg.plines'"
		if (!all(is.logical(plines))) {
			if (!all(is.numeric(plines)) )  {
				stop(msg, call.=FALSE)
			}
			if (!all(is.wholenumber(plines))) {
				stop(msg, call.=FALSE)
			}
		}
		ap$aquagr$plines <- plines
		###
		discr <- a$discr
		if (all(discr=="def")) {
			discr <- .ap2$stn$aqg_discrim
		}
		if (!all(is.logical(discr)) | length(discr) != 1) {
			stop("Please provide either 'def' or TRUE or FALSE to the argument 'aqg.discr'", call.=FALSE)
		}
		ap$aquagr$discr <- discr		
		###
	} # end if !is.null(ap$aquagr)
	return(ap)
} # EOF

ap_check_pca_defaults <- function(ap, header) {
	if (!is.null(ap$pca)) {
		a <- ap$pca
		###
		what <- a$what
		possVal <- pv_pca_what
		if (!any(what %in% possVal) | length(what) !=1) {
			stop(paste("Please provide one of \n'", paste(possVal, collapse="', '"), "'\n to the argument 'pca.what'.", sep=""), call.=FALSE)
		}
		###
		pcs <- a$pcs
		if (!all(is.numeric(pcs)) | length(pcs) !=2) {
			stop(paste("Please provide a length two numeric to the argument 'pca.sc'."), call.=FALSE)
		}
		###
		pcSc <- a$pcSc
		if (!is.null(pcSc)) {
			if (!all(is.numeric(pcSc))) {
				stop(paste("Please provide a numeric vector to the argument 'pca.sc.pairs'."), call.=FALSE)
			}
		}
		###
		pcLo <- a$pcLo
		if (!all(is.numeric(pcLo))) {
			stop(paste("Please provide a numeric vector to the argument 'pca.lo'."), call.=FALSE)
		}
		###
		elci <- a$elci
		if (!is.null(elci)) {
			if (all(elci=="def")) {
				elci <- .ap2$stn$pca_CI_ellipse_level
			}
			if (!all(is.numeric(elci)) | length(elci) != 1) {
				stop("Please provide a length one numeric to the argument 'pca.elci'.", call.=FALSE)
			}
			if (elci <= 0 | elci >= 1) {
				stop("Please provide a number between 0 and 1 to the argument 'pca.elci'.", call.=FALSE)
			}
		} # end !is.null(elci)
		ap$pca$elci <- elci
		###
		co <- a$colorBy
		co2 <- a$elcolorBy
		if (!is.null(co2)) {
			if (length(co2) != 1) {
				if (length(co2) != length(co)) {
					stop("Please provide a vector with the same length as in 'pca.colorBy' to the argument 'pca.elcolorBy'.", call.=FALSE)
				}
			}
		}
		###
	} # end if !is.null(ap$pca) 
	return(ap)
} # EOF

ap_check_gp_generalPlottingDefaults <- function(ap) {
	if (!is.null(ap$genPlot)) {
		pg <- ap$genPlot
		###
		checkForLengthOneChar <- function(char, argName) {
			if (!all(is.character(char)) | length(char) !=1) {
				stop(paste("Please provide a length one character to the argument '", argName, "'.", sep=""), call.=FALSE)
			}	
		} # EOIF
		###
		checkForLengthOneChar(pg$where, "pg.where")
		checkForLengthOneChar(pg$onMain, "pg.main")
		checkForLengthOneChar(pg$onSub, "pg.sub")
		checkForLengthOneChar(pg$fns, "pg.fns")
	} # end if !is.null(ap$genPlot)
	return(ap)
} # EOF

ap_checExistence_Defaults <- function(ap, dataset) {
	cPref <- .ap2$stn$p_ClassVarPref
	yPref <- .ap2$stn$p_yVarPref
	cns <- colnames(dataset$header)
	checkEx <- function(charVec, where, what) {
		a <- which(!charVec %in% cns)
		if (length(a) != 0) {
			vars <- paste(charVec[a], collapse=", ")
			stop(paste("Sorry, the variable \"", vars, "\" appears not to exist in your dataset. \nPlease check the ", where, " part of the analysis procedure / your input.", sep=""), call.=FALSE)
		}
		a <- substr(charVec, 1, nchar(what))
		if (any(a != what)) {
			stop(paste("Not all of the provided variables are of the the required type \"", what, "\".\nPlease check the ", where, " part of the analysis procedure / your input.", sep=""), call.=FALSE)
		}
	} # EOIF
	checkEx(ap$ucl$splitClasses, "variable split", cPref)
	checkEx(ap$dpt$excludeOutliers$exOutVar, "exclude outliers (variable)", cPref)
	checkEx(ap$pca$colorBy, "PCA", cPref)
	el2c <- ap$pca$elcolorBy
	if (!is.null(el2c)) {
		checkEx(el2c, "PCA (elcolorBy)", cPref)	
	}
	checkEx(ap$simca$simcOn, "SIMCA", cPref)
	checkEx(ap$plsr$regressOn, "PLSR (regress on)", yPref)
	checkEx(ap$plsr$colorBy, "PLSR (color by)", cPref)
	if (!is.null(ap$aquagr)) {
		if (is.null(ap$aquagr$vars)) {
			stop(paste("Sorry, you have to provide one or more values for \"aqg.vars\". Please check the Aquagram part of the analysis procedure / your input."), call.=FALSE)
		} else {
			checkEx(ap$aquagr$vars, "Aquagram", cPref)		
		}
	}
	wls <- getWavelengths(dataset)
	splitWl <- ap$ucl$splitWl
	for (i in 1: length(splitWl)) {
		options(warn=-1)
		nums <- as.numeric(strsplit(splitWl[i], "-to-")[[1]])
		options(warn=0)
		if (any(is.na(nums))) {
			stop(paste("Please check the analysis procedure / your input at the wavelength-split; provide the wavelength split in the format like e.g. '1300-to-1600'."), call.=FALSE)
		}
		if (min(nums) < min(wls) | max(nums) > max(wls)) {
			stop(paste("Sorry, the specified wavelengths \"", splitWl[i], "\" are out of the available range.", sep=""), call.=FALSE)
		}
	}
	####
	ap <- ap_checkAquagramDefaults(ap, dataset$header)
	ap <- ap_check_pca_defaults(ap, dataset$header)
	ap <- ap_check_gp_generalPlottingDefaults(ap)
	# add more default checking for other statistics here
	return(ap)
} # EOF

ap_cleanZeroValuesCheckExistenceDefaults <- function(ap, dataset) {
	ap <- ap_cleanOutZeroValues(ap, dataset)
	ap <- ap_checExistence_Defaults(ap, dataset)
	return(ap)
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
#' @family Data pre-treatment functions
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
		nr <- nrow(dataset$NIR)
		nc <- ncol(dataset$NIR)
		noise <- matrix(rnorm(nr*nc, mean=1, sd=noiseLevel), nr, nc)
		dataset$NIR <- dataset$NIR * noise
		if (!.ap2$stn$allSilent) {cat(" ok\n")}
	}
	return(dataset)
} # EOF

makeIdString <- function(siClass, siValue, siWlSplit, siSmooth, siNoise) {
	# siClass and siValue come in as dataframes with one row and one or more columns
	varString <- NULL
	for (i in 1: ncol(siClass)) {
#		varString <- paste(varString, siClass[1,i], ":", siValue[1,i], ", ", sep="")
		varString <- paste(siValue[1,i], ", ", sep="")
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

checkForStats <- function(ap) {
	cnt <- 0
	char <- NULL
	if (!is.null(ap$pca)) { 
		cnt <- cnt + 1 
		char <- c(char, "pca")
	}
	if (!is.null(ap$simca)) { 
		cnt <- cnt + 1
		char <- c(char, "simca")
	}
	if (!is.null(ap$plsr)) { 
		cnt <- cnt + 1
		char <- c(char, "plsr")
	}
	if (!is.null(ap$aquagr)) {
		cnt <- cnt + 1
		char <- c(char, "Aquagram")
	}
	return(list(cnt=cnt, char=char))
} # EOF

#' @title *** Generate Datasets and make Models *** 
#' @description Generate several datasets by splitting up the original dataset 
#' according to the variables and values as specified in the analysis procedure 
#' in the 'split dataset' section and then calculate the models as specified 
#' in the 'statistics' section on all of the  datasets. By providing additional 
#' arguments to the function \code{\link{getap}} (what is the default way to get 
#' the analysis procedure 'ap') you can override any value in the analysis 
#' procedure. Please see examples and \code{\link{getap}} and  
#' \code{\link{anproc_file}} for further information.
#' @details Split-combinations that yield no result will automatically be omitted.
#' It is recommended to first  make the analysis procedure file as complete and 
#' accurate as possible, and then to override only a few parameters if necessary.
#' @param dataset An object of class 'aquap_data' as generated by \code{\link{gfd}}
#' @param md The metadata, an object of class 'aquap_md'
#' @param ap The analysis procedure, an object of class 'aquap_ap'
#' @return An object of class \code{\link{aquap_cube}} containing all the 
#' statistical models / calculations that were performed on the split-variations 
#' of the dataset.
#' @seealso \code{\link{getap}}, \code{\link{getmd}}
#' @examples
#' \dontrun{
#' dataset <- gfd() # will load or import data
#' cube <- gdmm(dataset) # split up the dataset and make models, execute the 
#' # analysis procedure as specified in its .r file
#' cube <- gdmm(dataset, getap(spl.var="C_Group")) # split the dataset by "C_Group"
#' cube <- gdmm(dataset, getap(spl.var=c("C_Group", "C_Temp"))) # split the dataset 
#' # by "C_Group", then by "C_Temp"
#' cube <- gdmm(dataset, getap(spl.wl="1300-to-1600")) # override 'spl.wl' in the 
#' # analysis procedure
#' cube <- gdmm(dataset, getap(aqg.bootCI=FALSE)) # override the value in 
#' # 'aqg.bootCI' of the analysis procedure with 'FALSE'. 
#' cube <- gdmm(dataset, getap(do.sim=FALSE, pls.regOn="Y_Temp"))
#' }
#' @family Core functions
#' @export
gdmm <- function(dataset, ap=getap(), md=getmd() ) {
	autoUpS(); ap; md ;
	if (class(dataset) != "aquap_data") {
		stop("Please provide an object of class 'aquap_data' to the argument 'dataset'.", call.=FALSE)
	}
	ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset)
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
	stat <- checkForStats(ap) 	#  check the ap if and where we calculate a model
	if (!.ap2$stn$allSilent & (stat$cnt != 0)) {cat("\nCalculating models...\n")}
	for (i in 1: cpt@len) {
		if (!.ap2$stn$allSilent & (stat$cnt != 0)) {cat(paste("   Working on dataset #", i, " of ", cpt@len, " (", getIdString(cubeList[[i]]), ") \n", sep=""))}
		cubeList[[i]] <- makeAllModels(cubeList[[i]], md, ap)
	} # end for i
	# collect the ranges for aquagram (if any)
	rangesColl <- NULL
	if ( (any(ap$aquagr$fsa!=FALSE) | any(ap$aquagr$fss!=FALSE)) & !is.null(ap$aquagr) ) {
		rangesColl <- collectRanges(cubeList, length(ap$aquagr$vars)) # returns a list with each element representing the range through all sets in a classVariable, so, one list-element for each class-variable
	}
	if (!.ap2$stn$allSilent & (stat$cnt != 0)) {cat("Done.\n")}
	if (!.ap2$stn$allSilent & (stat$cnt == 0)) {cat("No models were calculated.\n")}
	###
	
	##
	cube <- new("aquap_cube", cubeList)
	cube@metadata <- md
	cube@anproc <- ap
	cube@cp <- cp
	cube@cpt <- cpt
	cube@aqgRan <- rangesColl
	return(cube)
} # EOF


collectRanges <- function(cubeList, lengthClasses) {
	aquCalcRes  <- 111
	ranAvg <- ranBootRes <- ranSubtrSpec <- NULL
	ranColl <- list() # the range collection
	length(ranColl) <- lengthClasses
	###	
	iwo2 <- function(x, xslot, varn=NULL, classIndex) {
		aqgCalc <- getAqgResList(x)[[classIndex]]
		if (!is.null(slot(aqgCalc, xslot))) {
			if (is.null(varn)) {
				return(range(slot(aqgCalc, xslot)))
			} else {
				a <- slot(aqgCalc, xslot)
				return(range(a[varn]))
			}
		} else {
			return(1212)
		}
	} # EOIF
	owo2 <- function(cubeList, xslot, varn=NULL, classIndex) {
		a <- range(sapply(cubeList, iwo2, xslot, varn, classIndex))
		if (a[1] == 1212) {
			return(NULL)
		} else {
			return(a)
		}
	} # EOIF
	###
	for (i in 1: lengthClasses) {
			ranAvg <- owo2(cubeList, "avg", classIndex=i)
			ranBootRes <-  owo2(cubeList, "bootRes", classIndex=i)
			ranSubtrSpec <-  owo2(cubeList, "subtrSpec", varn="NIR", classIndex=i)
			ranColl[[i]] <- list(ranAvg=ranAvg, ranBootRes=ranBootRes, ranSubtrSpec=ranSubtrSpec)
	} # end for i
	return(ranColl)
} # EOF


#' @title Split Dataset
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{getap}}, also within function \code{\link{gdmm}}, to 
#' override the values in the analysis procedure file and so to modify the 
#' split-dataset process - see examples.
#' 
#' \code{getap(...)}
#' 
#' \code{gdmm(dataset, ap=getap(...))}
#' 
#' @template mr_details_allParams
#' @template mr_spl_split_param
#' @examples
#' \dontrun{
#' dataset <- gfd() # will load or import data
#' cube <- gdmm(dataset, getap(spl.var="C_Group")) # split the dataset by "C_Group"
#' cube <- gdmm(dataset, getap(spl.var=c("C_Group", "C_Temp"))) # split the dataset 
#' # by "C_Group", then by "C_Temp"
#' cube <- gdmm(dataset, getap(spl.wl="1300-to-1600")) # override 'spl.wl' in the 
#' # analysis procedure 
#' }
#' @family Calc. arguments
#' @name split_dataset
NULL

