
makeSingleBlock <- function(ClassVar, header, metaData) {
	stn <- getstn()
	ind <- which(colnames(header)==ClassVar)
	levelsChar <- levels(header[,ind])
	classPrefix <- stn$p_ClassVarPref
	strippedClassVar <- unlist(strsplit(ClassVar, classPrefix))[2]
	if (is.na(strippedClassVar)) {
		classPrefix <- stn$p_yVarPref
		strippedClassVar <- unlist(strsplit(ClassVar, classPrefix))[2]
	}
	EClabel <- metaData$postProc$ECRMLabel[1]
#	if (get("stngs")$autoRemoveECs & (strippedClassVar != get("stngs")$ECRMCol))
	if (TRUE & (strippedClassVar != stn$p_ECRMCol)) { 
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
	# now resort if the value is all numeric
	options(warn=-1)
	nums <- as.numeric(out[,2])
	options(warn=-0)
	if (all(is.numeric(nums))) {
		out[,2] <- out[,2][order(nums)]
	}
	return(out)
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
					if (colname != "exOut") { # because this is an exception
						stop("Check settings for the logical input in the spl.x please. One has to be TRUE.", call.=FALSE)
					}
					if (colname == "exOut") {
						chars <- rep("no", nrow(frame))
						logic <- rep(FALSE, nrow(frame))
						multFrame <- frame
					}
				}
			}
		}
	}
	dfChars <- data.frame(X=chars)
	colnames(dfChars) <- colname
	dfLogic <- data.frame(X=logic)
	colnames(dfLogic) <- paste(colname, "Logic", sep="")
	out <- cbind(multFrame, dfChars, dfLogic)
	return(out)
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
	aquapCPT@csAvg <- patt[, which(colnames(patt) == "csAvgLogic")]	
	aquapCPT@noise <- patt[, which(colnames(patt) == "NoiseLogic")]
	aquapCPT@exOut <- patt[, which(colnames(patt) == "exOutLogic")]
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
	useRaw  <- ap$dpt$csAvg$useRaw
	useConSAvg <- ap$dpt$csAvg$doAvg
	patt <- multiplyByBinary(patt, useRaw, useConSAvg, "csAvg")
	#
	useRaw  <- ap$dpt$noise$useRaw
	useNoise <- ap$dpt$noise$useNoise
	patt <- multiplyByBinary(patt, useRaw, useNoise, "Noise")
	#
	excludeOutliers <- ap$dpt$excludeOutliers$exOut
	useRaw <- ap$dpt$excludeOutliers$exOutRaw
	patt <- multiplyByBinary(patt, useRaw, excludeOutliers, "exOut")
	##
	cpt <- makeCPT(patt)
	cp <- cleanCPPattern(patt)
	return(list(cp=cp, cpt=cpt))
} # EOF

ap_cleanOutZeroValues <- function(ap, dataset) {
	stn <- getstn()
	cPref <- stn$p_ClassVarPref
	yPref <- stn$p_yVarPref
	noSplitCol <- stn$p_commonNoSplitCol
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

ap_reCheckAqgBootR <- function(ap, dataset) {
	stn <- getstn()
	###
#	R <- ap$aquagr$R
	R <- ap$aquagr$ROrig # because we have to re-evaluate, as, due to subsplitting in groups, the nrow very probably did change!!! 
	if (all(R=="def")) {
		R <- stn$aqg_bootR
	}
	if (!grepl("nrow@", R)) {
		if (!is.numeric(R) | (length(R)!=1)) {
			stop("Please provide either 'def', 'nrow@x' (with x being a number),  or a length 1 numeric for the argument 'aqg.R'.", call.=FALSE)			
		}
	}
	if (!is.numeric(R)) {
		num <- as.numeric(unlist(strsplit(R, "@"))[2])
		R <- round(nrow(dataset) * num, 0)
	}
	ap$aquagr$R <- R # possibly new value
	###
	return(ap)
} # EOF

ap_checkAquagramDefaults <- function(ap, header) {
	stn <- getstn()
	if (!is.null(ap$aquagr)) {
		a <- ap$aquagr
		###
		nrCorr <- a$nrCorr
		if (all(nrCorr == "def")) {
			nrCorr <- stn$aqg_correctNrOfObs
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
			mod <- stn$aqg_defaultMod
		}
		if (!any(mod %in% possibleValues) | length(mod) !=1 ) {
			stop(paste("Please provide one of \n", paste(possibleValues, collapse=", "), "; \nor 'def' for reading in the default from the settings.r file to the argument 'aqg.mod'.", sep=""), call.=FALSE)
		}		
		ap$aquagr$mod <- mod
		###
		minus <- a$minus
		groupingVars <- a$vars
		if (!is.null(minus)) {
			if (!all(is.character(minus)) ) {
				stop("Please provide only characters for the argument 'aqg.minus'", call.=FALSE)
			}
			if (length(minus) > length(groupingVars)) {
				stop("Please provide a character vector not longer as the character vector in 'aqg.vars' for the argument 'aqg.minus'", call.=FALSE)
			}
			if (length(minus) == 1 & length(groupingVars) > 1 ) {
				minus <- rep(minus, length(groupingVars))
			}
			for (i in 1: length(groupingVars)) {
				ind <- which(colnames(header) == groupingVars[i])
				levelsChar <- levels(header[,ind])
				if ((!minus[i] %in% levelsChar) & grepl("diff", mod) ) {
					stop(paste("Sorry, it appears that the provided value \"", minus[i], "\" for the argument 'aqg.minus' is not present in the Aquagram grouping variable \"", groupingVars[i], "\". \nPlease check your input at 'aqg.minus' and 'aqg.vars'", sep=""), call.=FALSE)
				}
				if ((!minus[i] %in% levelsChar) & any(spectra %in% pvSubSpectra)) {
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
		if (!is.null(TCalib)) {
			if (all(TCalib == "def")) {
				TCalib <- stn$aqg_calibTRange
			}
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
			Texp <- stn$aqg_Texp
		}
		if (!is.numeric(Texp) | (length(Texp) != 1) ) {
			stop("Please provide either 'def', or a length 1 numeric for the argument 'aqg.Texp'.", call.=FALSE)			
		}
		ap$aquagr$Texp <- Texp
		###
		bootCI <- a$bootCI
		if (all(bootCI=="def")) {
			bootCI <- stn$aqg_bootCI
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
		R <- ROrig <- a$R
		if (all(R=="def")) {
			R <- stn$aqg_bootR
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
		ap$aquagr$ROrig <- ROrig
		ap$aquagr$R <- R
		###
		if (!is.numeric(a$smoothN) | length(a$smoothN)!=1) {
			stop("Please provide an odd length 1 numeric to the argument 'aqg.smoothN'", call.=FALSE)
		}
		###
		selWls <- a$selWls
		if (all(selWls=="def")) {
			selWls <- stn$aqg_wlsAquagram
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
			clt <- stn$aqg_linetypes
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
			pplot <- stn$aqg_adPeakPlot
		}
		if (!all(is.logical(pplot)) | length(pplot) != 1) {
			stop("Please provide either 'def', or 'TRUE' or 'FALSE' to the argument 'aqg.pplot'", call.=FALSE)
		}
		ap$aquagr$pplot <- pplot
		###
		plines <- a$plines
		if (all(plines=="def")) {
			plines <- stn$aqg_AdLines
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
			discr <- stn$aqg_discrim
		}
		if (!all(is.logical(discr)) | length(discr) != 1) {
	#		stop("Please provide either 'def' or TRUE or FALSE to the argument 'aqg.discr'", call.=FALSE)
		}
		ap$aquagr$discr <- discr		
		###
	} # end if !is.null(ap$aquagr)
	return(ap)
} # EOF

ap_check_pca_defaults <- function(ap, header) {
	stn <- getstn()
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
				elci <- stn$pca_CI_ellipse_level
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
	stn <- getstn()
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
		if (pg$where == "def") {
			aa <- stn$gen_plot_pgWhereDefault
		} else {
			aa <- pg$where
		}
		if (aa == "pdf" | aa == "PDF") {
			aa <- "pdf"
		} # now we should either have 'pdf' or anything else one character
		ap$genPlot$where <- aa
		checkForLengthOneChar(pg$onMain, "pg.main")
		checkForLengthOneChar(pg$onSub, "pg.sub")
		checkForLengthOneChar(pg$fns, "pg.fns")
	} # end if !is.null(ap$genPlot)
	return(ap)
} # EOF

ap_check_dptModules <- function(ap) {
	a <- ap$dpt$dptModules
	prePost <- c(a$dptPre, a$dptPost)
	pv <- pv_dptModules
	if (!is.null(prePost)) {
		prePostFirst <- unlist(lapply(strsplit(prePost, "@"), function(x) x[1])) # get only the characters before an eventual '@'
	} else {
		prePostFirst <- NULL
	}
	a <- which(!prePostFirst %in% pv) # gives back the indices of the elements of dptMods that are NOT within the possible values
	if (length(a) != 0) { 
		vars <- paste(prePost[a], collapse=", ")
		stop(paste("Sorry, the dpt module \"", vars, "\" can not be recognized. Please check the 'dpt.pre' and 'dpt.post' parts of the analysis procedure / your input.", sep=""), call.=FALSE)
	}
} # EOF

ap_check_plsr_Input <- function(ap, header) {
	stn <- getstn()
	if (!is.null(ap$plsr)) {
		cPref <- stn$p_ClassVarPref
		##
		e <- ap$plsr
		## ncomp
		if (!is.null(e$ncomp)) {
			if (!all(is.numeric(e$ncomp)) | length(e$ncomp) != 1) {
				stop("Please provide a length one numeric to the argument 'pls.ncomp' in the analysis procedure / your input.", call.=FALSE)
			}
			if (!is.wholenumber(e$ncomp)) {
				stop("Please provide an integer to the argument 'pls.ncomp' in the analysis procedure / your input.", call.=FALSE)
			}
		}
		############ valid  pv_plsr_crossvalidation <- c("CV", "LOO")
		## the validity of the regressOn has already been checked, so we can rely here on the fact that all the regressOn is correct
		regrOn <- e$regressOn # can be a vector 1..n
		if (all(e$valid ==  "def")) {
			e$valid <- stn$plsr_calc_typeOfCrossvalid
		}
		if (all(e$valid == "LOO")) {
			e$valid <- "LOO"
		}
		if ( (length(e$valid) > 1) & (length(e$valid) != length(regrOn)) ) {
			stop(paste0("Please provide either an input of length '1', or of equal length as the input in 'pls.regOn' in the argument 'pls.valid'. Please check your input resp. the analysis procedure."), call.=FALSE)
		}
		if (length(e$valid) == 1) {
			e$valid <- rep(e$valid, length(regrOn))
		}
		cns <- colnames(header)
		cns <- cns[grep(cPref, cns)]
		for (i in 1: length(e$valid)) {
			options(warn=-1)
			nr <- as.numeric(e$valid[i])
			options(warn=0)
			if (!is.na(nr)) { # so it really was a number
				e$valid[i] <- as.character(round(nr, 0))
			} else { # so we had a character
				if (e$valid[i] != "LOO" ) {
					if (!e$valid[i] %in% cns) {
						stop(paste0("Sorry, the class-variable '", e$valid[i], "' for grouping scans for crossvalidation seems not to exist in the provided dataset.\nPlease check your input at the argument 'pls.valid' or in the analysis procedure. \nPossible values are: '", paste(cns, collapse="', '"), "."), call.=FALSE)
					}
					if (nlevels(header[, e$valid[i]]) < 2) {
						stop(paste0("Sorry, you need to have at least two distinct groups for crossvalidation. The selected variable '", e$valid[i], "' contains only one group in the current dataset."), call.=FALSE)
					}
				} # end is.character			
			} # end else
		} # end for i	
		ap$plsr$valid <- e$valid
		###############
		# exclude outliers
		regrOn <- e$regressOn # can be a vector 1..n
		exOut <- e$exOut
		if (all(exOut ==  "def")) {
			exOut <- stn$plsr_calc_excludePlsrOutliers
		}
		if ( (length(exOut) > 1) & (length(exOut) != length(regrOn)) ) {
			stop(paste0("Please provide either an input of length '1', or of equal length as the input in 'pls.regOn' in the argument 'pls.exOut'. Please check your input resp. the analysis procedure."), call.=FALSE)
		}
		if (length(exOut) == 1) {
			exOut <- rep(exOut, length(regrOn))
		}
		if (!all(is.logical(exOut))) {
			stop(paste0("Please provide either logical TRUE or logical FALSE (not as character) for the argument 'pls.exOut'. Please check your input resp. the analysis procedure."), call.=FALSE)
		}
		ap$plsr$exOut <- exOut
		###############
		## what pv_plsr_what <- c("both", "errors", "regression")
		if (!all(is.character(e$what)) | length(e$what) !=1) {
			stop(paste("Please provide a length one character to the argument 'pls.what' in the analysis procedure / your input.\nPossible values are: '", paste(pv_plsr_what, collapse="', '"), "'.", sep=""), call.=FALSE)
		}
		if (!e$what %in% pv_plsr_what) {
			stop(paste("Please provide one of: '", paste(pv_plsr_what, collapse="', '"), "' to the argument 'pls.what' in the analysis procedure / your input.", sep=""), call.=FALSE)
		}
		## RDP
		if (!all(is.logical(e$inRdp)) | length(e$inRdp) !=1) {
			stop("Please provide either 'TRUE' or 'FALSE' to the argument 'pls.rdp' in the analysis procedure / your input", call.=FALSE)
		}
		##
		if (!is.null(e$colorBy)) {
			cns <- colnames(header)
			cns <- cns[grep(cPref, cns)]
			if (length(e$colorBy) != 1) {
				stop(paste0("Please provide only a single class variable for coloring in the plsr-plot.\nPlease check your input at the argument 'pls.colorBy' or in the analysis procedure. \nPossible values are: '", paste(cns, collapse="', '"), "."), call.=FALSE)
			}
			if (!e$colorBy %in% cns) {
				stop(paste0("Sorry, the class-variable '", e$colorBy, "' for coloring in the plsr-plot seems not to exist in the provided dataset.\nPlease check your input at the argument 'pls.colorBy' or in the analysis procedure. \nPossible values are: '", paste(cns, collapse="', '"), "."), call.=FALSE)
			}		

		} # end !is.null e$colorBy
		#
	} # end if !is.null
	return(ap)
} # EOF

ap_check_classifier_Input <- function(ap, header) {
	stn <- getstn()
	if (!is.null(ap$classif)) {
		pvAllXDA <- pv_classificationFuncs_XDA
		pvNonDA <- pv_nonDAClassifiers
		pvAllClassif <- pv_allClassificationFuncs
		cns <- colnames(header)
		cPref <- stn$p_ClassVarPref
		cns <- cns[grep(cPref, cns)]
		###
		checkClassExistence <- function(X, char, ppv=FALSE) {
			if (!all(is.character(X))) {
				stop(paste0("Please only provide characters as input for the argument '", char, "'. Please check your input resp. the analysis procedure."), call.=FALSE)
			}
			for (i in 1: length(X)) {
				if (!X[i] %in% cns) {
					msg1 <- paste0("Sorry, it seems that the class-variable `", X[i], "` as target variable for classification (", char, ") does not exist in the provided dataset.")
					msg2 <- "";  if (ppv) { msg2 <- paste0("\nPossible values are: '", paste(cns, collapse="', '"), ".") }
					stop(paste0(msg1, msg2), call.=FALSE)
				}
				if (nlevels(header[, X[i]]) < 2) {
					stop(paste0("Sorry, you need to have at least two distinct groups for classification (", char, "). The selected variable '", X[i], "' contains only one group in the current dataset."), call.=FALSE)
				}
			} # end for i
		} # EOIF
		checkTestCV <- function(x, char) {
			if (!all(is.logical(x)) | length(x) != 1) {
				stop(paste0("Please provide either `TRUE` `FALSE` to the argument `", char, "`."), call.=FALSE)
			}
		} # EOIF
		checkPercTest <- function(x, char, lim=50) {
			if (!all(is.numeric(x)) | length(x) != 1) {
				stop(paste0("Please provide a numeric length one between ", lim, " and 100 to the argument `", char, "`."), call.=FALSE)
			}
			if (x < 0 | x > lim) {
				stop(paste0("Please provide a numeric between 0 and ", lim, " to the argument `", char, "`."), call.=FALSE)
			}
		} # EOIF
		checkForNumericLengthOne <- function(x, char) {
			if (!all(is.numeric(x)) | length(x) != 1) {
				stop(paste0("Please provide a numeric length one to the argument `", char, "`."), call.=FALSE)
			}
		} # EOIF
		checkPCAred <- function(mo, prLog, prNC) {
			if (!all(is.logical(prLog)) | length(prLog) != 1) {
				stop(paste0("Please provide either TRUE or FALSE to the argument `", mo, "pcaRed` in the analysis procedure / your input."), call.=FALSE)
			}
			if (any(is.character(prNC))) {
				if (length(prNC) != 1 | !all(prNC == "max")) {
					stop(paste0("For using the maximum number of components resp. their scores for subsequent classification, please provide `max` to the argument `", mo, "pcaNComp`in the analysis procedure / your input."), call.=FALSE)
				}
			} else { # end if any character
				if (!all(is.wholenumber(prNC))) {
					stop(paste0("In order to specify the components resp. their scores for subsequent classification, please provide only integers to the argument `", mo, "pcaNComp` in the analysis procedure / your input."), call.=FALSE)
				}
			} # end else
		} # EOIF
		checkThings <- function(mo, testCV, percTest, bootCutoff, BootFactor, valid) {
			checkTestCV(testCV, paste0(mo, "testCV"))
			checkPercTest(percTest, paste0(mo, "percTest"))
			checkForNumericLengthOne(bootCutoff, paste0(mo, "cvBootCutoff"))
			checkForNumericLengthOne(BootFactor, paste0(mo, "cvBootFactor"))
			checkForNumericLengthOne(valid, paste0(mo, "valid"))
		} # EOIF
		###
		# XDA
		da <- ap$classif$da
		if (!is.null(da)) {
			if (!all(is.character(da$type))) {
				stop(paste0("Please only provide characters as input for the argument 'da.type'. Please check your input resp. the analysis procedure."), call.=FALSE)
			}
			if (!all(da$type %in% pvAllXDA)) {
				stop(paste0("Please provide one or more of '", paste(pvAllXDA, collapse="`, `"), "' to the argument 'da.type' in the analysis procedure / your input."), call.=FALSE)
			}
			checkClassExistence(da$classOn, "da.classOn")
			checkThings("da.", da$testCV, da$percTest, da$bootCutoff, da$bootFactor, da$valid)
			checkPCAred("da.", da$pcaRed, da$pcaNComp)
		} # end !is null
		#
		rnf <- ap$classif$rnf
		if (!is.null(rnf)) {
			checkClassExistence(rnf$classOn, "rnf.classOn")
			checkThings("rnf.", rnf$testCV, rnf$percTest, rnf$bootCutoff, rnf$bootFactor, rnf$valid)
			checkPCAred("rnf.", rnf$pcaRed, rnf$pcaNComp)
		} # end !is null
		#
		svm <- ap$classif$svm
		if (!is.null(svm)) {
			checkClassExistence(svm$classOn, "svm.classOn")
			checkThings("svm.", svm$testCV, svm$percTest, svm$bootCutoff, svm$bootFactor, svm$valid)
			checkPCAred("svm.", svm$pcaRed, svm$pcaNComp)
		} # end !is null
		#
		ann <- ap$classif$nnet
		if (!is.null(ann)) {
			checkClassExistence(ann$classOn, "nnet.classOn")
			checkThings("nnet.", ann$testCV, ann$percTest, ann$bootCutoff, ann$bootFactor, ann$valid)
			checkPCAred("nnet.", ann$pcaRed, ann$pcaNComp)			
		} # end !is null
		#
	} # end not null
	return(ap)
} # EOF

ap_checExistence_Defaults <- function(ap, dataset, haveExc, checkWlsRange) {
	stn <- getstn()
	cPref <- stn$p_ClassVarPref
	yPref <- stn$p_yVarPref
	if (haveExc) {	
		outlierChar <- stn$p_outlierCol # we have to define an exception for the checking the existence
	} else {
		outlierChar <- NULL
	}
	cns <- colnames(dataset$header)
	#
	checkEx <- function(charVec, where, what, exc=outlierChar) {
		aa <- which(!charVec %in% cns) # gives back the indices of the charVec that is not in the colnames
		if (!is.null(exc)) { # so if we want to allow an exception
			allNotHere <- charVec[aa]
			onlyExceptions <- allNotHere[grep(exc, allNotHere)] # now isolate the exceptions from all not here
			if (length(onlyExceptions) == 0) {
				excOK <- FALSE
			} else {
				excOK <- all(grepl(exc, onlyExceptions)) # exc has length 1 !!
			}
			if (excOK) { # 
				remainingChar <- setdiff(allNotHere, onlyExceptions)
			} else {
				remainingChar <- allNotHere
			}
			aa <- which(charVec %in% remainingChar)
		} # end !is.null(exc)
		if (length(aa) != 0) {
			vars <- paste(charVec[aa], collapse=", ")
			stop(paste("Sorry, the variable \"", vars, "\" appears not to exist in your dataset. \nPlease check the ", where, " part of the analysis procedure / your input.", sep=""), call.=FALSE)
		}
		a <- substr(charVec, 1, nchar(what))
		if (any(aa != what)) {
			stop(paste("Not all of the provided variables are of the the required type \"", what, "\".\nPlease check the ", where, " part of the analysis procedure / your input.", sep=""), call.=FALSE)
		}
	} # EOIF
	checkEx(ap$ucl$splitClasses, "variable split", cPref)
	exOut <-  ap$dpt$excludeOutliers$exOut
	exOutRaw <-  ap$dpt$excludeOutliers$exOutRaw
	if (exOut | exOutRaw) {
			checkEx(ap$dpt$excludeOutliers$exOutVar, "exclude outliers (variable)", cPref)
	}
	checkEx(ap$pca$colorBy, "PCA (colorBy)", cPref)
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
	if (checkWlsRange) {
		wls <- getWavelengths(dataset)
		wlsTolerance <- 10  # XXX improve this !!
		splitWl <- ap$ucl$splitWl
		for (i in 1: length(splitWl)) {
			options(warn=-1)
			nums <- as.numeric(strsplit(splitWl[i], "-to-")[[1]])
			options(warn=0)
			if (any(is.na(nums))) {
				stop(paste("Please check the analysis procedure / your input at the wavelength-split; provide the wavelength split in the format like e.g. '1300-to-1600'."), call.=FALSE)
			}
			if (min(nums) < (min(wls)-wlsTolerance) | max(nums) > (max(wls)+wlsTolerance) ) {
				stop(paste("Sorry, the specified wavelengths \"", splitWl[i], "\" are out of the available range.", sep=""), call.=FALSE)
			}
		} # end for i
	} # end if checkWlsRange
	####
	ap <- ap_checkAquagramDefaults(ap, dataset$header)
	ap <- ap_check_pca_defaults(ap, dataset$header)
	ap <- ap_check_gp_generalPlottingDefaults(ap)
	ap_check_dptModules(ap)
	ap <- ap_check_plsr_Input(ap, dataset$header)
	ap <- ap_check_classifier_Input(ap, dataset$header)
	# add more default checking for other statistics here
	return(ap)
} # EOF

ap_cleanZeroValuesCheckExistenceDefaults <- function(ap, dataset, haveExc, checkWlsRange=FALSE) {
	ap <- ap_cleanOutZeroValues(ap, dataset)
	ap <- ap_checExistence_Defaults(ap, dataset, haveExc, checkWlsRange)
	return(ap)
} # EOF

#' @title Select Wavelengths
#' @description Select wavelengths from a dataset.
#' @details It is not necessary to exactly know the numbers of the wavelengths 
#' present in the dataset, as for the lower limit the same or the next highest 
#' wavelength as \code{from}, and for the upper limit the same or the next 
#' lowest wavelength as \code{to} will be selected.
#' @param dataset An object of class 'aquap_data'
#' @param from Numeric length one. The lower limit of the wavelengths to be 
#' selected.
#' @param to Numeric lengtho one. The upper limit of the wavelengths to be 
#' selected.
#' @seealso \code{\link{siWl}}, \code{\link{siWlg}}
#' @return The standard dataset (class 'aquap_data') containing only the 
#' selected wavelengths.
#' @family Data pre-treatment functions
#' @export
selectWls <- function(dataset, from, to) {
	wls <- getWavelengths(dataset)
	lowInd <- min(which(wls >= from)) # so we do not have to know the exact wavelength
	highInd <- max(which(wls <= to))
	NIRsel <- dataset$NIR[,lowInd:highInd]
	dataset$NIR <- NIRsel
	minWls <- min(wls)
	maxWls <- max(wls)
	if (from < minWls) {
		message(paste("The wavelength ", from, "nm is not available in the provided dataset.\nThe next highest available wavelength, ", minWls, "nm has been selected.", sep=""))
	}
	if (to > maxWls) {
		message(paste("The wavelength ", to, "nm is not available in the provided dataset.\nThe next lowest available wavelength, ", maxWls, "nm has been selected.", sep=""))		
	}
	return(dataset)
} # EOF

performConSAvg <- function(dataset, siCsAvg, numbers=NULL) {
	stn <- getstn()
	if (siCsAvg) {
		yVarPref <- stn$p_yVarPref
		classVarPref <- stn$p_ClassVarPref
		usePar <- stn$gen_useParallel
		md <- getMetadata(dataset)
		if (!stn$allSilent) {cat("      averaging consecutive scans...")}
		if (is.null(numbers)) {
			numbers <- seq(1: md$postProc$nrConScans) # this is selecting *all* the consecutive scans
		}
		header <- getHeader(dataset)
		NIR <- getNIR(dataset)
		originalNIRColnames <- colnames(NIR)
		colnames(NIR) <- paste("NIRwlx.", seq(1,ncol(NIR)), sep="")
		datasetTX <- cbind(header, NIR)
		csName <- paste(yVarPref, stn$p_conSNrCol, sep="")
		snName <- paste(yVarPref, stn$p_sampleNrCol, sep="")		
		#
		dfSelector <- data.frame(numbers) 	## XXX numbers could be user defined in a future version
		colnames(dfSelector) <- csName		## so first get only those cons. scans of interest in ALL the sample numbers, then via plyr::ddply grouped by sample number calculate their mean
		datasetTXSelection <- plyr::match_df(datasetTX, dfSelector, on=csName) 	## isolate only those observations that have our numbers in consec scans
		#
		ind <- which(colnames(dataset$header) == csName) # the index of the column with the consecutive scans
		datasetRed <- dataset[which(dataset$header[,ind] == 1)]	 # keeps only the first of the consecutive scans in the dataset; we only want one row for each sample in the header file, as averaging over the cons. scans gives us just one row
		#
		ind1 <- which(colnames(datasetTXSelection) == snName)
		ind2 <- which(colnames(datasetTXSelection) == csName)
		nirInd <- grep("NIRwlx.", colnames(datasetTXSelection))
		NIRtemp <- datasetTXSelection[, c(ind1, ind2, nirInd)]
		if (usePar) {
			registerParallelBackend()
		}
		NIR <- plyr::ddply(NIRtemp, snName, plyr::colwise(mean), .parallel=usePar)[, -(1:2)]	# Perform averaging grouped by sample Number, then leave out the first two columns
		NIR <- as.matrix(NIR)
		colnames(NIR) <- originalNIRColnames
		rownames(NIR) <- rownames(datasetRed)
		datasetRed$NIR <- I(NIR)
		dataset <- reFactor(datasetRed)		## to adjust for the changed factor-level situation in the class for the cons scans
		if (!stn$allSilent) {cat(" ok\n")}
	} # end if (siCsAvg) 
	return(dataset)
}# EOF

performNoise <- function(dataset, siNoise, noiseFile) {
	stn <- getstn()
	if (siNoise) {
		noiMode <- stn$noi_addMode 
		if (!stn$allSilent) {cat(paste0("      adding ", noiMode, " noise..."))}
		dataset <- noi_performNoise(dataset, noiseFile)	
		if (!stn$allSilent) {cat(" ok\n")}
	}
	return(dataset)
} # EOF

performExcludeOutliers <- function(dataset, siExOut, ap) {
	stn <- getstn()
	eov <- ap$dpt$excludeOutliers$exOutVar # the grouping variables from the analysis procedure by which grouping the outliers should be detected
	doExOut <- ap$dpt$excludeOutliers$exOut
	keepRaw <- ap$dpt$excludeOutliers$exOutRaw
	if (doExOut & keepRaw) {oneBlock <- FALSE; twoBlocks <- TRUE} else {oneBlock <- TRUE; twoBlocks <- FALSE}
	cnOutlier <- stn$p_outlierCol
	cPref <- stn$p_ClassVarPref
	if (!is.null(eov)) { 
		eovTitleChar <- paste(unlist(lapply(strsplit(eov, cPref), function(x) x[2])), collapse=".")
	}
	if (keepRaw | siExOut) {
		kmax <- stn$simca_kMax
		tol <- stn$simca_tolerance
		header <- getHeader(dataset)
		indOut <- NULL
		if ((twoBlocks & !siExOut) | oneBlock) {
			for (i in 1:length(eov)) {
				ind <- which(colnames(header) == eov[i])
				indOut <- c(indOut, ind)
			} # end for i
			fusionFac <- as.factor(apply(header[indOut], 1, function(x) paste(x, collapse="_"))) # extract the selected columns from the header and paste each row together
			flatDf <- makeFlatDataFrame(dataset, fusionGroupBy=fusionFac)
			if (!stn$allSilent) {cat("      detecting & flagging outliers... ")}
			simcaMod <- rrcovHD::RSimca(grouping ~ ., data=flatDf, kmax=kmax, tol=tol)  ## k=0 does not work ??, but still calculating k
			singleFlags <- paste(unlist(lapply(simcaMod@pcaobj, function(x) length(which(x@flag == FALSE)))), collapse="|")
			flags <- as.logical(simcaMod@flag) #  having FALSE for outliers
			nrOutliers <- length(which(flags == FALSE)) 
			usedK <- paste(simcaMod@k, collapse="|")
			finderMsg <- "found"
		} else { # end if ((twoBlocks & siExOut) | oneBlock)
			if (!stn$allSilent) {cat("      flagging outliers:")}
			finderMsg <- ""
		}
		if (twoBlocks & siExOut) {
			flags <- .ap2$.outlierFlags$allFlags[[.ap2$.ocnt]] # use the counter established in function createOutlierFlaglist in gdmm to subscript the list containing all the flag-data
			nrOutliers <- .ap2$.outlierFlags$nrOutliers[[.ap2$.ocnt]]
			singleFlags <- .ap2$.outlierFlags$singleFlagCounter[[.ap2$.ocnt]]
			usedK <- .ap2$.outlierFlags$usedK[[.ap2$.ocnt]]
			if(is.null(nrOutliers) | is.null(singleFlags) | is.null(usedK)) {stop("This should not be NULL!!!")}
			.ap2$.ocnt <- .ap2$.ocnt +1
		}
		if (nrOutliers == 0) {
			msg <- "none. "
			remMsg <- "\n"
		} else {
			msg <- paste(finderMsg," *", singleFlags, "*. [", usedK, " comp.]", sep="")
			if (twoBlocks & siExOut) {
				remMsg <- ", removing outliers. \n"} 
			else  {
				if (oneBlock & siExOut) {remMsg <- ""} else {remMsg <- "\n"}
			}
		}
#		if (siExOut) {crMsg <- ""} else {crMsg <- "\n"}
		if (!stn$allSilent) {cat(paste(msg, remMsg, sep=""))}
		if (twoBlocks & !siExOut) {
			.ap2$.outlierFlags$allFlags <- c(.ap2$.outlierFlags$allFlags, list(flags))
			.ap2$.outlierFlags$nrOutliers <- c(.ap2$.outlierFlags$nrOutliers, list(nrOutliers))
			.ap2$.outlierFlags$singleFlagCounter <- c(.ap2$.outlierFlags$singleFlagCounter, list(singleFlags))
			.ap2$.outlierFlags$usedK <- c(.ap2$.outlierFlags$usedK, list(usedK))
		}
		## now flag the outliers
		flagsFacDf <- data.frame(as.factor(!flags)) # having TRUE for outlier
		colnames(flagsFacDf) <- paste(cPref, cnOutlier, "_", eovTitleChar, sep="")
		ind <- max(grep(cnOutlier, colnames(header)))
		if (length(ind) == 0) { # XXX possible bug 
			ind <- ncol(header)-1
		}
		headerPre <- header[, 1:ind]
		# ! check for a possible double column that can appear if the user decides to do the SAME outlier-checking as has been done already automatically in the beginning
		inDo <- which(colnames(headerPre) == colnames(flagsFacDf)) # the index of a possible double column in the header, that can happen if the user chooses to exclude outliers in all-scope and that has already been done at the time of the import !!
		if (length(inDo) > 0 ) {
			headerPre <- headerPre[, -inDo]
		}
		headerPost <- header[, (ind+1):ncol(header)]
		headerNew <- cbind(headerPre, flagsFacDf, headerPost)
		colRepNew <- extractClassesForColRep(headerNew)		## the color representation of the factors
		rownames(headerNew) <- rownames(colRepNew) <- rownames(header) # just to be sure
		dataset$header <- headerNew
		dataset$colRep <- colRepNew
	} # end if keepRaw | siExOut
	if (siExOut) {
		if (!stn$allSilent & oneBlock) {cat(", removing outliers.\n")}
		dataset <- dataset[flags] # as the outliers are FALSE, we are so removing the outliers
	}
	return(dataset)
} # EOF

performDPT_Core <- function(dataset, dptSeq, extraModelList=NULL, allExportModels=TRUE) {
# dptSeq is the pre or post charcter string coming from ap$dpt$dptModules
# pv_dptModules <- c("sgol", "snv", "msc", "emsc", "osc", "deTr", "gapDe")
# the modules itself are already checked
#print(dptSeq); 
	stn <- getstn()
	pvMod <- pv_dptModules	
	if (!is.null(dptSeq)) {
	first <- unlist(lapply(strsplit(dptSeq, "@"), function(x) x[1])) # get only the characters before an eventual '@'
		if (!stn$allSilent) {
			cat(paste("      Data treatment: ", paste(dptSeq, collapse=", "), "\n", sep=""))
		}
		for (i in 1: length(first)) {
		###########
			if (first[i] == pvMod[1]) { # sgolay
				## to have also the option to NOT add additional parameters to sgol
				if (!grepl("@", dptSeq[i])) {
					dataset <- do_sgolay(dataset,  exportModel=allExportModels)
				} else { # so, yes, we have an '@' present	
#		stop("You must have the parameters of the Savitzky-Golay operation defined via the character 'sgol@p-n-m', with p, n, m being integers; n has to be odd.\nDefault values could be e.g. 2-21-0 for a mild smoothing. Please check the analysis procedure / your input.", call.=FALSE)
					infoChar <- strsplit(dptSeq[i], "@")[[1]][[2]] # get only the second part of the module containing the numbers (still as character!)
					options(warn=-1)
					nums <- as.numeric(unlist(strsplit(infoChar, "-")))
					options(warn=0)
					if (any(is.na(nums)) | !all(is.wholenumber(nums))) {
						stop("Please provide only integers as arguments to the Savitzky-Golay operation, e.g. ", pvMod[1], "@2-21-0.\n Please check the analysis procedure / your input.", call.=FALSE)
					}
					if (length(nums) != 3) {
						stop("Please provide exactly three integers as arguments to the Savitzky-Golay operation, e.g. ", pvMod[1], "@2-21-0. \n Please check the analysis procedure / your input.", call.=FALSE)
					}
					dataset <- do_sgolay(dataset, p=nums[1], n=nums[2], m=nums[3], exportModel=allExportModels)  # arguments: p=2, n=21, m=0)
				}  # end else
			} # end sgolay
		###########
			if (first[i] == pvMod[2]) { # snv
				dataset <- do_snv(dataset, exportModel=allExportModels)  # no additional arguments here
			} # end snv
		###########
			if (first[i] == pvMod[3]) { # msc
				vec <- NULL
				if (grepl("@", dptSeq[i])) {
					ords <- unlist(strsplit(dptSeq[i], "@"))[2] # 'one row dataset', get only the element after the '@', what should be an existing object in the workspace with a dataset containing only one row.
					if (!exists(ords)) {
						stop(paste(pvMod[3], "@", ords, ":Sorry, the object with the name \"", ords, "\" does not seem to exist.\nPlease check the analysis procedure / your input (part data pre / post treatment)", sep=""), call.=FALSE)
					}
					if (!is(eval(parse(text=ords)), "aquap_data")) {
						stop(paste("Please make sure that the specified object \"", ords, "\" is of class 'aquap_data'", sep=""), call.=FALSE)
					}
					if (nrow(eval(parse(text=ords))) != 1) {
						stop(paste("For successful ", pvMod[3], ", the provided dataset has to have only one row, while the dataset in object \"", ords, "\" is containing ", nrow(ords), " rows. \nPlease check the analysis procedure / your input (part data pre / post treatment)", sep=""), call.=FALSE)
					}
					vec <- eval(parse(text=ords)) # now get the provided object from workspace into 'vec'
				} # end if grepl @ element
				if (!is.null(extraModelList)) {
					if (!is.null(extraModelList[[i]][[1]])) { # we need the second bracket [[1]] because we have lists having NULL in the empty places!
						eMo <- extraModelList[[i]]
					} else {
						eMo <- NULL
					}
				} else {
					eMo <- NULL
				}
				dataset <- do_msc(dataset, ref=vec, extMscModel=eMo, exportModel=allExportModels) # one reference (a one-lined dataset)
			} # end msc
		###########
			if (first[i] == pvMod[4]) { # emsc
				lvObj <- NULL
				if (!grepl("@", dptSeq[i])) {
					stop(paste("Sorry, you have to provide the name of an object containing a data frame with one or two loadings or with one regression vector for successful '", pvMod[4], "'.\nDo this in the format \"", pvMod[4], "@myObj\", with 'myObj' being the name of an existing data frame with max. 2 columns containing two loadings or one regression vector.\nPlease see ?dpt_modules for further details.", sep=""), call.=FALSE) 
				}
				loadvecObjChar <- unlist(strsplit(dptSeq[i], "@"))[2] # the object containing the data frame with max 2 rows (up to two loading vectors or one regression vector).
				if (!exists(loadvecObjChar)) {
						stop(paste(pvMod[4], "@", loadvecObjChar, ": Sorry, the object with the name \"", loadvecObjChar, "\" does not seem to exist.\nPlease check the analysis procedure / your input (part data pre / post treatment); see ?dpt_modules for further details.", sep=""), call.=FALSE)					
				} 
				lvObj <- eval(parse(text=loadvecObjChar)) # here get from the character only the real object
				if ( !is(lvObj, "matrix") & !is(lvObj, "data.frame") ) {
					stop(paste(pvMod[4], "@", loadvecObjChar, ": Please make sure that the specified object \"", loadvecObjChar, "\" is of class 'matrix' or 'data.frame'.\nPlease see ?dpt_modules for further details", sep=""), call.=FALSE)
				} # end if		
				if (ncol(lvObj) > 2) {
					stop(paste("For successful ", pvMod[4], "@", loadvecObjChar, ", the provided dataframe/matrix can have max. 2 columns. \nPlease see ?dpt_modules for further details.", sep=""), call.=FALSE)
				} # end if
				if (nrow(lvObj) != ncol(dataset$NIR) ) {
					stop(paste(pvMod[4], "@", loadvecObjChar, ":The length (nrow) of the provided dataframe/matrix is ", nrow(lvObj), ", while the current dataset has ", ncol(dataset$NIR), " wavelengths.\nThe length of the loading/regression vector and the number of wavelengths in the NIR-data must be the same.\nPlease provide a loading / regression vector with the right length fitting *all* the datasets in your 'cube', or re-think your (possible) data-splitting procedure when generating the 'cube' object. \nPlease see ?dpt_modules and ?do_emsc for further details.", sep=""), call.=FALSE)
				} # end if	
				dataset <- do_emsc(dataset, vecLoad = lvObj, exportModel=allExportModels) # a data frame with max 2 columns (loadings or a regression vector)
			} # end emsc
		###########
			if (first[i] == pvMod[5]) { # osc
					## have osc here soon 
					message("Sorry, no osc yet!")
			} # end osc
		###########
			if (first[i] == pvMod[6]) { # deTr
				if (!grepl("@", dptSeq[i])) {
					dataset <- do_detrend(dataset, exportModel=allExportModels) # takes the full range as source and target
				} else {
					trg <- "src"
					#
					infoChar <- strsplit(dptSeq[i], "@")[[1]][[2]] # get only the second part of the module containing the numbers (still as character!)
					charSep <- unlist(strsplit(infoChar, "-")) # the single elements
					options(warn=-1)
					src <- as.numeric(charSep)
					options(warn=0) 
					indNa <- which(is.na(src))
					if (length(indNa) != 0 ) { # so we have a character present
						trg <- charSep[indNa]
						src <- src[-indNa] # clean out the characters
					} else { # so we have no character present
						if (length(src) == 4) {
							trg <- src[3:4]
							src <- src[1:2]
						} else {
							if (length(src) == 3) {
								trg <- src[3]
								src <- src[1:2]
							}
						} # end else
					} # end else
					dataset <- do_detrend(dataset, src, trg, exportModel=allExportModels) # checking is inside here
				} # end else
			} # end deTrend
		###########
			if (first[i] == pvMod[7]) { # gap Derivative
				if (!grepl("@", dptSeq[i])) {
					dataset <- do_gapDer(dataset, exportModel=allExportModels)
				} else { # so, yes, we have an '@' present	meaning we are providing values
					infoChar <- strsplit(dptSeq[i], "@")[[1]][[2]] # get only the second part of the module containing the numbers (still as character!)
					options(warn=-1)
					nums <- as.numeric(unlist(strsplit(infoChar, "-")))
					options(warn=0)
					if (any(is.na(nums)) | !all(is.wholenumber(nums))) {
						stop("Please provide only integers as arguments to the gap-derivative operation, e.g. ", pvMod[7],"@1-11-13-1.\n Please check the analysis procedure / your input.", call.=FALSE)
					}
					if (length(nums) != 4) {
						stop("Please provide exactly four integers as arguments to the gap-derivative operation, e.g. ", pvMod[7], "@1-11-13-1. \n Please check the analysis procedure / your input.", call.=FALSE)
					}
					dataset <- do_gapDer(dataset, m=nums[1], w=nums[2], s=nums[3], deltaW=nums[4], exportModel=allExportModels)  # arguments: p=2, n=21, m=0)
				}  # end else
			} # end gap Derivative	
		###########			
		} # end for i
	} # end if
	return(dataset)	
} # EOF

performDpt_Pre <- function(dataset, ap) {
	dptPre <- ap$dpt$dptModules$dptPre
	dataset <- performDPT_Core(dataset, dptPre)
	return(dataset)
} # EOF

performDpt_Post <- function(dataset, ap) {
	dptPost <- ap$dpt$dptModules$dptPost
	dataset <- performDPT_Core(dataset, dptPost)	
	return(dataset)
} # EOF

makeIdString <- function(siClass, siValue, siWlSplit, siCsAvg, siNoise, siExOut, ap) {
	dpt <- ap$dpt$dptModules
	pre <- dpt$dptPre
	pos <- dpt$dptPost
	if (is.null(pre) & is.null(pos)) { dpt <- NULL } else { dpt <- 1 }
	# siClass and siValue come in as dataframes with one row and one or more columns
	varString <- NULL
	for (i in 1: ncol(siClass)) {
#		varString <- paste(varString, siClass[1,i], ":", siValue[1,i], ", ", sep="")
		varString <- paste(varString, siValue[1,i], ", ", sep="")
	}
	varString <- substr(varString, 1, (nchar(varString)-2)) # to get rid of the last ","
	varString <- paste(varString, " ", sep="")
	more <- FALSE
	if (siCsAvg) {
		more <- TRUE
		csAvg <- "csAvg. "
	} else {
		csAvg <- ""
	}
	if (siNoise) {
		more <- TRUE
		noise <- "noise "
	} else {
		noise <- ""
	}
	if (siExOut) {
		more <- TRUE
		exOut <- "exOut "
	} else {
		exOut <- ""
	}
#	if (!is.null(dpt)) {
#		more <- TRUE
#		dptOut <- "dpt "
#	} else {
#		dptOut <- ""
#	}
	if (more) {
		moreAdd <- ", "
	} else {
		moreAdd <- ""
	}
#	easy <- paste(varString, "@", siWlSplit[1], "-to-", siWlSplit[2], moreAdd, csAvg, noise, exOut, dptOut, sep="")
	easy <- paste(varString, "@", siWlSplit[1], "-to-", siWlSplit[2], moreAdd, csAvg, noise, exOut, sep="")
	return(easy)
} # EOF

initializeExtraModelsList <- function() {
	aa <- list(type=NULL, mod=NULL)
	assign(pv_extraMods, aa, pos=.ap2)
} # EOF

processSingleRow_CPT <- function(dataset, siClass, siValue, siWlSplit, siCsAvg, siNoise, siExOut, ap, noiseFile) { # si for single
	stn <- getstn()
	keepEC <- stn$gd_keepECs
	newDataset <- ssc_s(dataset, siClass, siValue, keepEC) 
	if (is.null(newDataset)) { # character "nixnox" is returned if a variable combination yields no data. That is because returning NULL introduced a bug if it was on the end of the list... probably...
#		return("nixnox")
		return(NULL)
	}
	initializeExtraModelsList() ### upper end
	newDataset <- selectWls(newDataset, siWlSplit[1], siWlSplit[2])
	newDataset <- performDpt_Pre(newDataset, ap)
	newDataset <- performConSAvg(newDataset, siCsAvg)
	newDataset <- performNoise(newDataset, siNoise, noiseFile)
	newDataset <- performExcludeOutliers(newDataset, siExOut, ap)
	newDataset <- performDpt_Post(newDataset, ap)
	##
	idString <- makeIdString(siClass, siValue, siWlSplit, siCsAvg, siNoise, siExOut, ap)
	newDataset@anproc <- ap
	exMod <- get(pv_extraMods, pos=.ap2)
#	exMod <- new("aquap_extMod", type=exMod$type, mod=exMod$mod)
	exMod <- list(type=exMod$type, mod=exMod$mod) # this should not be necessary !
	out <- new("aquap_set", dataset=newDataset, idString=idString, extraModels=exMod) 
	rm(list=(pv_extraMods), pos=.ap2) ### lower end (happens every time we are done with a single cube-element)
#	print(str(out@extraModels)); wait()
	return(out)
} # EOF

correct_cpt <- function(cpt, nullInd) {
	n <- nullInd
	#
	classes <- cpt@splitVars$classes[-n,] 
	rownames(classes) <- 1:nrow(classes)
	cpt@splitVars$classes <- classes
	#
	values <- cpt@splitVars$values[-n,] 
	rownames(values) <- 1:nrow(values)
	cpt@splitVars$values <- values
	#
	cpt@splitVars$values <- cpt@splitVars$values[-n,]
	cpt@wlSplit <- cpt@wlSplit[-n]
	cpt@csAvg <- cpt@csAvg[-n]
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
	if (!is.null(ap$classif)) {
		cnt <- cnt + 1
		char <- c(char, "classification")
	}
	return(list(cnt=cnt, char=char))
} # EOF

checkCubeForRealStats <- function(cube) {
	cle <- function(sName, obj=cube) { # cle: check list element
		return(any(unlist(lapply(obj, function(x) !is.null(slot(x, sName)))))) # returns TRUE if at last one model of kind "sName" is available
	} # EOIF
	cnt <- 0
	char <- NULL
	if (cle("pca")) { 
		cnt <- cnt + 1 
		char <- c(char, "pca")
	}
	if (cle("simca")) { 
		cnt <- cnt + 1
		char <- c(char, "simca")
	}
	if (cle("plsr")) { 
		cnt <- cnt + 1
		char <- c(char, "plsr")
	}
	if (cle("aquagr")) {
		cnt <- cnt + 1
		char <- c(char, "Aquagram")
	}
	if (cle("xda")) {
		cnt <- cnt + 1
		char <- c(char, "X.DA")
	}
	if (cle("rnf")) {
		cnt <- cnt + 1
		char <- c(char, "RNF")
	}
	if (cle("svm")) {
		cnt <- cnt + 1
		char <- c(char, "SVM")
	}
	if (cle("ann")) {
		cnt <- cnt + 1
		char <- c(char, "NNET")
	}
	return(list(cnt=cnt, char=char))
#	return(checkForStats(cube@anproc))
} # EOF

createOutlierFlagList <- function() {
	stn <- getstn()
	.ap2$.ocnt <- 1 # a counter
	.ap2$.outlierFlags <- list()
		.ap2$.outlierFlags$allFlags <- list()
		.ap2$.outlierFlags$nrOutliers <- list()
		.ap2$.outlierFlags$singleFlagCounter <- list()
		.ap2$.outlierFlags$usedK <- list()
} # EOF

checkLoadNoiseFile <- function(header, maxNir, ap, md, noiseFile) {
	stn <- getstn()
	useNoise <- ap$dpt$noise$useNoise
	noiMode <- stn$noi_addMode 
	pvNM <- pv_noiseAddModes
	if (useNoise & noiMode != pvNM[4]) { # also do not do this if we have the static mode
		if (all(noiseFile == "def")) { # the incoming 'noiseFile' is from the gdmm function. If any other than 'def' we do not even look at the metadata
			noiseFile <- stn$noi_noiseDataFilename
			# If a value other than "def" is provided in the argument "noiseFile" in "gdmm", this is overriding the value of "noiseFileName" in the metadata file.
			if (md$meta$noiseFile != noiseFile) { # we only look at the value for 'noiseFileName' in the metadata if 'noiseFile' in gdmm is "def"
				noiseFile <- md$meta$noiseFile
			}			
		} # end if all is def	
		if (!all(is.character(noiseFile)) | length(noiseFile) != 1) {
			stop("Please provide a character length one to the argument 'noiseFile' resp. in the settings.r file (parameter 'noi_noiseDataFilename').", call.=FALSE)
		} 
		##
		pathSH <- Sys.getenv("AQUAP2SH")
		addInfo <- "Please see the help for ?noise_procedures for more information.\n"
		noisePath <- paste(pathSH, noiseFile, sep="/")
		if (!file.exists(noisePath)) {
			stop(paste0("The R-data file \"", noiseFile, "\" that should contain noise-data does not seem to exist in \n\"", pathSH, "\".\nProvide either 'def' to read in the default value from the settings (parameter 'noi_noiseDataFilename'), or the name of an existing R-data file. ", addInfo), call.=FALSE)
		}
		noiDataset <- eval(parse(text=load(noisePath)))
		if (!is(noiDataset, "aquap_data")) {
			stop(paste0("Please make sure that the noise-data file '", noiseFile, "' is of class 'aquap_data' (as generated by the function 'gfd').\n", addInfo), call.=FALSE)
		}
		##
		checkDatasetVersion(noiDataset, noiseFile) # stops if the version of dataset is too old
		## check outliers
		if (stn$noi_forceExclusionOfOutliers) {
			clPrev <- stn$p_ClassVarPref
			cnOl <- stn$p_outlierCol
			allSuff <- stn$p_outlierCol_allSuffix
			cnOutlier <- paste0(clPrev, cnOl, allSuff)
			cns <- colnames(noiDataset$header)
			if (!cnOutlier %in% cns) {
				stop(paste0("You need to have outliers detected in the whole noise-dataset. Please re-import the noise data (function 'gfd') and make sure to have the argument 'dol' or its default in the settings.r file (parameter 'imp_flagOutliers') set to TRUE.\nPlease refer to ?gfd for more information.\n(You can deactivate the forced exclusion of outliers of the noise-data file in the settings at the argument 'noi_forceExclusionOfOutliers'.)"), call.=FALSE)
			}
			 noiDataset <- ssc_s(noiDataset, cnOutlier, FALSE) # only keep the non-outliers
		} 
		## check age-difference		
		tsData <- header[1,]$Timestamp
		tsNoise <- noiDataset$header[1,]$Timestamp # both get returned as NULL if the column is not available !!
		if (stn$noi_forceTimeDifferenceCheck & !is.null(tsData) & !is.null(tsNoise)) {
			diffT <- as.numeric(difftime(tsData, tsNoise, units="days"))
			accDiff <-  stn$noi_acceptTimeDiffDays
			if (diffT > accDiff) {
				stop(paste0("Sorry, the time difference between the first spectrum of the noise file and the first spectrum of the dataset is more than ", accDiff, " days (parameter 'noi_acceptTimeDiffDays' in the settings file).\n(You can deactivate the checking of time differences between noise-data file and actual dataset in the settings at the argument 'noi_forceTimeDifferenceCheck'.)"), call.=FALSE)
			}
		}
		## check noise plausibility
		if (stn$noi_forceNoisePlausibility) {
			accPer <- stn$noi_plausPercentage ;  	setCheck_NumericLengthOne(accPer, "noi_plausPercentage")
			maxAccept <- (maxNir/100)* accPer # maxNir coming from the actual dataset
			if (max(noiDataset$NIR) > maxAccept) {
				stop(paste0("It seems to be unlikely that the data in the R-data file '", noiseFile, "' in your AQUAP2SH-folder actually are noise-data, as their spectral maximum is exceeding ", accPer, "% of the spectral maximum in the actual dataset. \n(You can deactivate the checking of the plausibility of the noise-data file in the settings at the argument 'noi_forceNoisePlausibility'.)"), call.=FALSE)
			}
		}
		## check noise mode
		noiMode <- stn$noi_addMode
		if (!all(noiMode %in% pv_noiseAddModes) | length(noiMode) != 1) { # c("sd", "extrema")
			stop(paste0("Please provide one of '", paste(pv_noiseAddModes, collapse="', '"), "' to the argument 'noi_addMode' in the settings file."), call.=FALSE)
		}
		## check sample size
		sampSize <- stn$noi_sampleSize
		if (!all(is.numeric(sampSize)) | length(sampSize) != 1 | !all(is.wholenumber(sampSize)) | !all(sampSize > 0) ) {
			stop(paste0("Please provide a positive integer length one to the argument 'noi_sampleSize' in the settings.r file."), call.=FALSE)
		}
		##
		assign("noiseFile", noiseFile, pos=parent.frame(n=1)) # important for handing over the correct name of the noise file
		return(noiDataset)
	} else { #
		return(NULL)
	} # end if useNoise	
} # EOF

haveClassicAqg <- function(ap) {
	classMod <- pv_AquagramModes[1] # the first two are the classic ones
	mod <- ap$aquagr$mod
	if (grepl(classMod, mod)) {
		return(TRUE)
	} else {
		return(FALSE)
	}
} # EOF

checkTempCalibFile <- function(tempFile, ap, md) {
	stn <- getstn()
	if (!is.null(ap$aquagr)) { 
		if (!haveClassicAqg(ap)) { 
			clPrev <- stn$p_ClassVarPref
			yPrev <- stn$p_yVarPref
			#
			if (all(tempFile == "def")) { # the incoming 'tempFile' is from the gdmm function. If any other than 'def' we do not even look at the metadata
				tempFile <- stn$aqg_tempCalib_Filename
				# If a value other than "def" is provided in the argument "tempFile" in "gdmm", this is overriding the value of "tempCalibFileName" in the metadata file.
				if (md$meta$tempFile != tempFile) { # we only look at the value for 'noiseFileName' in the metadata if 'noiseFile' in gdmm is "def"
					tempFile <- md$meta$tempFile
				}			
			} # end if all is def	
			if (!all(is.character(tempFile)) | length(tempFile) != 1) {
				stop("Please provide a character length one to the argument 'tempFile' resp. in the settings.r file (parameter 'aqg_tempCalib_Filename').", call.=FALSE)
			} 
			##
			pathSH <- Sys.getenv("AQUAP2SH")
			addInfo <- "Please see the help for ?tempCalib_procedures for more information.\n"
			tempPath <- paste(pathSH, tempFile, sep="/")
			if (!file.exists(tempPath)) {
				stop(paste0("The R-data file \"", tempFile, "\" that should contain temperature calibration data does not seem to exist in \n\"", pathSH, "\".\nProvide either 'def' to read in the default value from the settings (parameter 'aqg_tempCalib_Filename'), or the name of an existing R-data file. ", addInfo), call.=FALSE)
			}
			tempDataset <- eval(parse(text=load(tempPath)))
			if (!is(tempDataset, "aquap_data")) {
				stop(paste0("Please make sure that the temperature calibration data file '", tempFile, "' is of class 'aquap_data' (as generated by the function 'gfd').\n", addInfo), call.=FALSE)
			}
			##
			checkDatasetVersion(tempDataset, tempFile) # stops if the version of dataset is too old
			## check outliers
			if (stn$aqg_tempCalib_forceOutlExcl) {
				cnOl <- stn$p_outlierCol
				allSuff <- stn$p_outlierCol_allSuffix
				cnOutlier <- paste0(clPrev, cnOl, allSuff)
				cns <- colnames(tempDataset$header)
				if (!cnOutlier %in% cns) {
					stop(paste0("You need to have outliers detected in the whole temperature calibration dataset. Please re-import the data (function 'gfd') and make sure to have the argument 'dol' or its default in the settings.r file (parameter 'imp_flagOutliers') set to TRUE.\nPlease refer to ?gfd for more information.\n(You can deactivate the forced exclusion of outliers of the temperature calibration data file in the settings at the argument 'aqg_tempCalib_forceOutlExcl'.)"), call.=FALSE)
				}
				 tempDataset <- ssc_s(tempDataset, cnOutlier, FALSE) # only keep the non-outliers
			} # if force 
	#		## check age-difference		
	#		tsData <- header[1,]$Timestamp
	#		tsNoise <- noiDataset$header[1,]$Timestamp # both get returned as NULL if the column is not available !!
	#		if (stn$noi_forceTimeDifferenceCheck & !is.null(tsData) & !is.null(tsNoise)) {
	#			diffT <- as.numeric(difftime(tsData, tsNoise, units="days"))
	#			accDiff <-  stn$noi_acceptTimeDiffDays
	#			if (diffT > accDiff) {
	#				stop(paste0("Sorry, the time difference between the first spectrum of the noise file and the first spectrum of the dataset is more than ", accDiff, " days (parameter 'noi_acceptTimeDiffDays' in the settings file).\n(You can deactivate the checking of time differences between noise-data file and actual dataset in the settings at the argument 'noi_forceTimeDifferenceCheck'.)"), call.=FALSE)
	#			}
	#		}
			## check temperature plausibility
			if (TRUE) {
				cns <- colnames(tempDataset$header)
				waterCn <- paste0(yPrev, pv_YcolumnNameSampleTemp)
				if (! waterCn %in% cns) {
					stop(paste0("It seems to be unlikely that the data in the R-data file '", tempFile, "' in your AQUAP2SH-folder actually are temperature calibration data, as the column '", waterCn, "' containing the sample temperature is missing. \nPlease check the temperature calibration dataset resp. experiment, possibly re-import the raw data using 'gfd' and move the resulting R-data file to your AQUAP2SH folder."), call.=FALSE)
				}
			}
			assign("tempFile", tempFile, pos=parent.frame(n=1)) # important for handing over the correct name of the tempFile 
			return(tempDataset)
		} else {
			return(NULL)
		}
	}  else { # end if !is.null(ap$aquag)
		return(NULL)
	}
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
#' @param ap The analysis procedure, an object of class 'aquap_ap'
#' @param noiseFile Character length one. The name of the corresponding noise data 
#' file residing in the AQUAP2SH folder. Leave at the default 'def' to read in the 
#' value from the settings.r file (parameter \code{noi_noiseDataFilename}), or 
#' provide the name of an existing R-data file in your specified AQUAP2SH folder 
#' to use this as source for the noise data. If a value other than "def" is 
#' provided, this is overriding the value of "noiseFileName" in the metadata file.
#' Please see \code{\link{noise_procedures}} for more information.
#' @param tempFile Character length one. The name of the corresponding temperature 
#' calibration data file residing in the AQUAP2SH folder. Leave at the default 'def' 
#' to read in the value from the settings.r file (parameter 
#' \code{aqg_tempCalib_Filename}), or provide the name of an existing R-data file 
#' in your specified AQUAP2SH folder to use this as source for the temperature 
#' calibration data. If a value other than "def" is provided, this is overriding 
#' the value of "tempCalibFileName" in the metadata file. Please see 
#' \code{\link{tempCalib_procedures}} for more information.
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
#' cube <- gdmm(dataset, getap(spl.do.noise=TRUE), noiseFile="FooBarNoise") # to
#' # use the noise-data file 'FooBarNoise' instead of the default noise data file
#' }
#' @family Core functions
#' @export
gdmm <- function(dataset, ap=getap(), noiseFile="def", tempFile="def") {
	stn <- autoUpS(); ap;
	if (!is(dataset, "aquap_data")) {
		stop("Please provide an object of class 'aquap_data' to the argument 'dataset'.", call.=FALSE)
	}
	checkDatasetVersion(dataset)
	createOutlierFlagList() # needed to collect the flags and flag-data from the first block to the second
	ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset, haveExc=TRUE, checkWlsRange=TRUE)
	md <- getMetadata(dataset)
	mdF <- getmd() # the name of the noise file could be different(?)
	if (md$meta$noiseFile != mdF$meta$noiseFile) {
		md$meta$noiseFile <- mdF$meta$noiseFile # if different, take the value from the file
	}
	if (md$meta$tempFile != mdF$meta$tempFile) {
		md$meta$tempFile <- mdF$meta$tempFile # if different, take the value from the file
	}
	##
	noiseDataset <- checkLoadNoiseFile(dataset$header, max(dataset$NIR), ap, md, noiseFile) # only if noise is added; if not returns NULL; is assigning noiseFile !!
	noi_calculateNoiseDistribution(noiseDataset, noiseFile) # only if noise: calculate noise distribution, save as global variable in .ap2
	##
	tempCalibDataset <- checkTempCalibFile(tempFile, ap, md) # only if we are calculating an aquagram; if not returns NULL; is assigning tempFile
	aq_loadGlobalAquagramCalibData(tempCalibDataset, tempFile) # only if aquagram: calculate AUC etc., save as global variable in .ap2
	ap <- aq_checkTCalibRange(ap, tempFile) # only if Aquagram: is stopping if the temperatuer range is out of reach
	##
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
	csAvg <- cpt@csAvg # is a vector
	noise <- cpt@noise # is a vector
	exOut <- cpt@exOut # is a vector
	### generate datasets
	if (!stn$allSilent) {cat("Generating Datasets...\n")}
	for (i in 1:len) {
		if (!stn$allSilent) {cat(paste("   Working on #", i, " of ", len, "\n", sep=""))}
		cubeList[[i]] <- processSingleRow_CPT(dataset, as.data.frame(classes[i,,drop=F]), as.data.frame(values[i,,drop=F]), wlSplit[[i]], csAvg[i], noise[i], exOut[i], ap, noiseFile) # the "as.data.frame" is necessary if we only have one column
	} # end for i
	if (!stn$allSilent) {cat("Done.\n")}
	###
	### clean out the NULL-datasets
#	nullInd <- which(unlist(lapply(cubeList, is.null))) # which one of the split by variable resulted in zero rows (has been returned as NULL in ssc_s)
#	nullInd <- which(cubeList == "nixnox")
	nullInd <- which(lapply(cubeList, is.null) == TRUE)
	if (length(nullInd) > 0) {
		cubeList <- cubeList[-nullInd]
		cp <- cp[-nullInd,]
		rownames(cp) <- 1:nrow(cp)
		cpt <- correct_cpt(cpt, nullInd)
		message(paste("  *", length(nullInd), "* of the split-by-variable combinations resulted in no data. Those datasets have been omitted.", sep=""))
	}
	###
	
	### now make the models (so far, we have only the dataset and the id-string in the set)
	stat <- checkForStats(ap) 	#  check the ap if and where we calculate a model
	if (!stn$allSilent & (stat$cnt != 0)) {cat("\nCalculating models...\n")}
	maybeGeneratePlsrCluster(ap)
	for (i in 1: cpt@len) {
		if (!stn$allSilent & (stat$cnt != 0)) {cat(paste("   Working on dataset #", i, " of ", cpt@len, " (", getIdString(cubeList[[i]]), ") \n", sep=""))}

		cubeList[[i]] <- makeAllModels(cubeList[[i]], md, ap, tempFile) ###### CORE #########  CORE ############ CORE ##############
	} # end for i
	maybeStopPlsrCluster()
	# collect the ranges for aquagram (if any)
	rangesColl <- NULL
	if ( (any(ap$aquagr$fsa!=FALSE) | any(ap$aquagr$fss!=FALSE)) & !is.null(ap$aquagr) ) {
		rangesColl <- collectRanges(cubeList, length(ap$aquagr$vars)) # returns a list with each element representing the range through all sets in a classVariable, so, one list-element for each class-variable
	}
	if (!stn$allSilent & (stat$cnt != 0)) {cat("Done.\n")}
	if (!stn$allSilent & (stat$cnt == 0)) {cat("No models were calculated.\n")}
	###
	
	##
	cube <- new("aquap_cube", cubeList)
	cube@metadata <- md
	cube@anproc <- ap
	cube@cp <- cp
	cube@cpt <- cpt
	cube@aqgRan <- rangesColl
	cube@timestamp <- as.character(Sys.time())
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
#' @seealso \code{\link{dpt_modules}}
#' @family Calc. arguments
#' @name split_dataset
NULL

