# Metadata --------------------------------------------
check_mdDefaults <- function(fn) {
	stn <- getstn()
	path <- stn$fn_metadata
	if (all(fn == "def")) {
		fn <- stn$fn_mDataDefFile
	}
	if (!all(is.character(fn)) | length(fn) != 1) {
		stop("Please provide a length one character to the argument 'fn', thank you.", call.=FALSE)
	} 
	ok <- file.exists(paste(path, fn, sep="/"))
	if (!ok) {
		stop(paste("The metadata-file \"", fn, "\" does not seem to exist. Please check your input.", sep=""), call.=FALSE)
	} else {
		assign("fn", fn, pos=parent.frame(n=1))
	}	
} # EOF

check_mdDefaultValues <- function(localEnv, tePaSH=NULL) {
	# realMeasurementLabel  
	stn <- getstn()
	le <- localEnv
	#
	noSplitVal <- le$commonValue
	if (all(noSplitVal == "def")) {
		noSplitVal <- stn$p_commonNoSplit
	}
	if (length(noSplitVal) != 1 | !all(is.character(noSplitVal))) {
		stop("Please provide a character length one to the variable 'commonValue' in the metadata file.", call.=FALSE)
	}
	assign("noSplitLabel", noSplitVal, pos=parent.frame(n=1))
	#
	ecl <- le$envControlLabel 
	if (all(ecl == "def")) {
		ecl <- stn$p_envControlLabel
	}
	if (length(ecl) != 1 | !all(is.character(ecl))) {
		stop("Please provide a character length one to the variable 'envControlLabel' in the metadata file.", call.=FALSE)
	}
	assign("ECLabel", ecl, pos=parent.frame(n=1))	
	#
	rml <- le$realMeasurementLabel
	if (all(rml == "def")) {
		rml <- stn$p_realMeasurementLabel
	}
	if (length(rml) != 1 | !all(is.character(rml))) {
		stop("Please provide a character length one to the variable 'realMeasurementLabel' in the metadata file.", call.=FALSE)
	}
	assign("RMLabel", rml, pos=parent.frame(n=1))
	# 
	ft <- le$filetype
	if (all(ft == "def")) {
		ft <- stn$imp_specFileType
	}
	if (length(ft) != 1 | !all(is.character(ft))) {
		stop("Please provide a character length one to the variable 'filetype' in the metadata file.", call.=FALSE)
	}
	if (grepl("custom@", ft)) {
		cfn <- strsplit(ft, "custom@")[[1]][2] # isolate the filename
		if (checkOnTest()) {
			pathSH <- tePaSH
		} else {
			pathSH <- Sys.getenv("AQUAP2SH")
		} # end else
		path <- paste(pathSH,  cfn, sep="/")
		if (!file.exists(path)) {
			stop(paste("The custom data-import file with the name '", cfn, "' does not seem to exist in \n\"", pathSH, "\".", sep=""), call.=FALSE)
		}
	}
	assign("filetype", ft, pos=parent.frame(n=1))	
	#
	nfn <- le$noiseFileName
	if (all(nfn == "def")) {
		nfn <- stn$noi_noiseDataFilename
	}
	if (length(nfn) != 1 | !all(is.character(nfn))) {
		stop("Please provide a character length one to the variable 'noiseFileName' in the metadata file resp. the argument 'noi_noiseDataFilename' in the settings file.", call.=FALSE)
	}
	assign("noiseFileName", nfn, pos=parent.frame(n=1))
	# 
	tcfn <- le$tempCalibFileName
	if (all(tcfn == "def")) {
		tcfn <- stn$aqg_tempCalib_Filename
	}
	if (length(tcfn) != 1 | !all(is.character(tcfn))) {
		stop("Please provide a character length one to the variable 'tempCalibFileName' in the metadata file resp. the argument 'aqg_tempCalib_Filename' in the settings file.", call.=FALSE)
	}
	assign("tempCalibFileName", tcfn, pos=parent.frame(n=1))
	#
	spac <- le$spacing
	msg <- "Please provide either FALSE or a length one numeric to the variable 'spacing' in the metadata file."
	if ( length(spac) != 1  | (!all(is.logical(spac)) & !all(is.numeric(spac))) ) {
		stop(msg, call.=FALSE)
	}
	if (is.logical(spac) & spac == TRUE) {
		stop(msg, call.=FALSE)		
	}
	#
	slClasses <- le$sl_classes
	if (all(slClasses == "def")) {
		slClasses <- stn$fn_class_structure
	}
	if (length(slClasses) != 1 | !all(is.character(slClasses))) {
		stop("Please provide a character length one to the variable 'sl_classes' in the metadata file.", call.=FALSE)
	}
	foldLoc <- stn$fn_metadata
	if (!file.exists(paste0(foldLoc, "/", slClasses, ".xlsx"))) {
 		stop(paste("The class-structure file \"", slClasses, ".xlsx\" does not seem to exist in the folder \"", foldLoc, "\". \nPlease check your input.", sep=""), call.=FALSE)
	}
	assign("slClasses", slClasses, pos=parent.frame(n=1))
	#
	#
	slType <- le$sampleListType
	if (all(slType == "def")) {
		slType <- stn$imp_sampleListType
	}
	if (!is.null(slType)) {
		if (length(slType) != 1 | !all(is.character(slType))) {
			stop("Please provide a character length one to the variable 'sampleListType' in the metadata file.", call.=FALSE)
		}
	}
	assign("slType", slType, pos=parent.frame(n=1))
	#
	trhLog <- le$tempHumLog
	if (all(trhLog == "def")) {
		trhLog <- stn$imp_use_TRH_logfile
	}
	if (trhLog == TRUE) {
		stop("Please provide either 'FALSE' or a character length one to the variable 'tempHumLog' in the metadata file.", call.=FALSE)
	}
	if (trhLog != FALSE) {
		if (length(trhLog) != 1 | !all(is.character(trhLog))) {
			stop("Please provide a character length one to the variable 'tempHumLog' in the metadata file.", call.=FALSE)
		}	
	}
	assign("trhLog", trhLog, pos=parent.frame(n=1))
	#
	multRows <- le$multiplyRows
	if (all(multRows == "def")) {
		multRows <- stn$imp_multiplyRows
	}
	if (length(multRows) != 1 | !all(is.logical(multRows))) {
		stop("Please provide either 'TRUE' or 'FALSE' to the variable 'multiplyRows' in the metadata file.", call.=FALSE)
	}
	assign("multRows", multRows, pos=parent.frame(n=1))

} # EOF

check_mdVersion <- function(folderLocal, nameLocal) {
	fiEl <- gl_firstElement_metadata
	tmpl <- gl_templateSuffix_mdAp
	manMax <- gl_maxEmptyMd # the max empty lines after deleting and adding keys
	#
	aa <- paste(path.package("aquap2"), "/templates/metadata.r", sep="")
	if (!file.exists(aa)) {
		aa <- paste(path.package("aquap2"), "/templates/metadata.r", sep="/inst")	# required in the case of devtools::load_all			
	} # end if	
#	pathToLocal <- paste(folderLocal, nameLocal, sep="/")
	out <- checkFileVersionPossiblyModify(pathToPack=aa, folderLocal, nameLocal, pm=fiEl, tmpl, manMax) # pm: the first key in the file
	if (out == FALSE) {
	#	stop("Please update the metadata files in your experiments according to the latest template.", call.=FALSE)
		stop(call.=FALSE)
	}
} # EOF

getClassStructureFromXlsx <- function(slClasses) {
	stn <- getstn()
	# is checked, everything coming in here should be a real existing xlsx file
	foldLoc <-  stn$fn_metadata
	delCol <- stn$p_deleteCol
	slClassFilePath <- paste0(foldLoc, "/", slClasses, ".xlsx") # make it the complete path
	#	
	xTxt <- openxlsx::read.xlsx(xlsxFile = slClassFilePath, skipEmptyRows = FALSE, colNames = FALSE)	
	#
	clPref <- stn$p_ClassVarPref
	yPref <- stn$p_yVarPref
	
	indNA <- sort(c(grep(clPref, xTxt[,1]), grep(yPref, xTxt[,1])))[-1]-1 # one row above each of the column names there MUST be an empty row, i.e. NA
	if (!all(is.na(xTxt[,1])[indNA])) {
		stop(paste0("There must be at least one empty row above each column name in the file \"", slClasses, ".xlsx\""), call.=FALSE)
	} # end if

	# get the column names
	indL1 <- sort(c(grep(clPref, xTxt[,1]), grep(yPref, xTxt[,1])))
	if (length(indL1) == 0) {
		stop(paste0("The column names for class- and numerical variables in the xlsx-file \n'", slClasses, "' must be preceded by '", clPref, "' resp. '", yPref, "'."), call.=FALSE)
	} # 
	
	colNamesL1 <- xTxt[,1][indL1]
	if (ncol(xTxt) > 1) {
		colNamesL2 <- xTxt[,2][indL1]
		colNamesL2[which(is.na(colNamesL2))] <- paste0(clPref, delCol) # is inserting "C_DELETE" into empty Level2 column names
	} else {
		colNamesL2 <- paste0(clPref, delCol) 
	} # end else

	cleanOutNAs <- function(vals) {
		# the indices of vals that are NAs XXX But are we sure that in every locale in xlsx empty cells come back as "NA" ?
		# when vals was only characters, empty cells get back as real NA
		# when vals was numeric, then as.character, then empty cells get back as "NA" as character
		naInd <- grep("NA", vals) # first look for "NA" as character
		vals[naInd] <- NA			# make it a real NA
		vnas <- which(is.na(vals)) #
		if (length(vnas) > 0) {
			if (length(vnas) == length(vals)) { #  so we have only NAs, we need to keep only one
				return("x")
			} # end if
			vals <- vals[-vnas]
		} # end if
		return(vals)
	} # EOIF

	#### now break up into elements and extract values on L1 and L2
	aa <- c(indL1, nrow(xTxt)+1)
	## L1
	valL1 <- list(NULL)
	length(valL1) <- length(indL1)
	for (i in seq_along(indL1)) {
		fr <- aa[i]+1
		to <- aa[i+1]-1
		vals <- as.character(xTxt[fr:to, 1])
		vals <- cleanOutNAs(vals)
		valL1[[i]] <- vals
	} # end for i

	## L2
	valL2 <- list(NULL)
	length(valL2) <- length(indL1)
	if (ncol(xTxt) == 1) {
		valL2 <- valL1
	} else {
		cols <- 2: ncol(xTxt) # the colnumns, ranging from 2 to ncol
		for (i in seq_along(indL1)) {
			rnFr <- aa[i]+1
			nrRows <- length(valL1[[i]])
			L2inner <- list(NULL)
			length(L2inner) <- nrRows
			for (k in 0 : (nrRows-1)) {
				vals <- as.character(xTxt[rnFr+k, cols])
				vals <- cleanOutNAs(vals)
				L2inner[[k+1]] <- vals
			} # end for k
			valL2[[i]] <- L2inner
		} # end for i
	} # end else
	#	
	outList <- list(cnsL1=colNamesL1, cnsL2=colNamesL2, valL1=valL1, valL2=valL2)
	return(outList)
} # EOF

getmd_core <- function(fn="def", stn) {
	check_mdDefaults(fn) # is assigning
	checkTransitTo_SLxlsx_MaybeMakeMetadataBackup(fn, stn)
	#
	clPref <- stn$p_ClassVarPref
	yPref <- stn$p_yVarPref
	timeCol <- paste(clPref, stn$p_timeCol, sep="")
	ecrmCol <- paste(clPref, stn$p_ECRMCol, sep="")
	replCol <- paste(clPref, stn$p_replicateCol, sep="")
	groupCol <- paste(clPref, stn$p_groupCol, sep="")
	commCol <- paste(clPref, stn$p_commonNoSplitCol, sep="")
	tempCol <- paste(yPref, stn$p_tempCol, sep="")
	rhCol <- paste(yPref, stn$p_RHCol, sep="")
	# defaults:
	defReplPref <- stn$p_replicatePrefix
	defNoTime <- stn$p_noTimePointsLabel
	##
#	expNameCol <- paste(clPref, stn$p_expNameCol, sep="")
#	sampleNrCol <- paste(yPref, stn$p_sampleNrCol, sep="")
#	conSNrCol <- paste(yPref, stn$p_conSNrCol, sep="")
#	deleteCol <- paste(clPref, stn$p_deleteCol, sep="")
	### the above 4 are not used here
	foldLoc <-  stn$fn_metadata
	check_mdVersion(folderLocal=foldLoc, nameLocal=fn) 	### Version check here !!
	e <- new.env()
	sys.source(paste(foldLoc, fn, sep="/"), envir=e)
	noSplitLabel <- ECLabel <- RMLabel <- filetype <- noiseFileName <- tempCalibFileName <- slClasses <- slType <- trhLog <- multRows <- NULL # gets assigned below
	check_mdDefaultValues(localEnv=e) ### checking and assigning
	##
	Repls <- (paste(defReplPref, seq(1, e$Repls), sep=""))
	Group <- e$Group
	if (all(e$TimePoints == FALSE)) {
		TimePoints <- defNoTime
	} else {
		TimePoints <- e$TimePoints
	}
	##
	# now get the class structure from the xlsx file
	classStructure <- getClassStructureFromXlsx(slClasses=slClasses) # "slClasses" got assigned above in check_mdDefaultValues
	##
	userCols <- NULL
	for (i in 1: length(classStructure$cnsL1)) {
		userCols <- c(userCols, classStructure$cnsL1[i], classStructure$cnsL2[i])
	} # end for i
	coluNames <- c(timeCol, ecrmCol, userCols, replCol, groupCol, tempCol, rhCol)
	###
	## put together
 	expClasses <- list(L1=classStructure$valL1, L2=classStructure$valL2, Repls=Repls, Group=Group, timeLabels=TimePoints)
 	postProc <- list(spacing=e$spacing, ECRMLabel=c(ECLabel, RMLabel), noSplitLabel=noSplitLabel, nrConScans=e$nrConScans)
 	meta <- list(expName=e$expName, coluNames=coluNames, filetype=filetype, noiseFile=noiseFileName, tempFile=tempCalibFileName, slType=slType, trhLog=trhLog, multRows=multRows, xaxDenom=e$xaxDenominator, yaxDenom=e$yaxDenominator)
	expMetaData <- list(expClasses = expClasses, postProc = postProc, meta = meta)
	return(new("aquap_md", expMetaData))
} # EOF

#' @title Get Metadata
#' @description Read in the metadata from the default or a custom metadata file 
#' located in the metadata-folder. By providing the argument 'expName' to the 
#' function you can override the experiment name as specified in the metadata.r 
#' file. This can be useful e.g. if you want to load a different file using 
#' \code{\link{gfd}}.
#' @details The name of the default metadata-file can be specified in the settings.
#' Only the experiment name can be changed via the \code{...} argument by providing 
#' the argument 'expName'. Other arguments will be ignored.
#' @param fn Character length one.  The filename of the metadata file to load. 
#' If left at 'def', the default filename for 
#' a metadata-file as specified in the settings (what is "metadata.r") is read 
#' in. Provide any other valid name of a metadata-file to load it. (Do not forget 
#' the '.r' at the end.)
#' @param ... Provide the argument 'expName' to override the experiment name 
#' specified in the metadata.r file.
#' @return An (invisible) list with all the metadata of the experiment.
#' @seealso \code{\link{metadata_file}}, \code{\link{getap}}, \code{\link{gdmm}}
#' @examples
#' \dontrun{
#' md <- getmd(); str(md); names(md)
#' md <- getmd("myFile.r")
#' md <- getmd(expName="OtherName") # to override the experiment name as specified 
#' # in the metadata.r file.
#' dataset <- gfd(getmd(expName="OtherName"))
#' }
#' @export
getmd <- function(fn="def", ...) {
	stn <- autoUpS()
	md <- getmd_core(fn, stn)
	old_expName <- md$meta$expName
	modifyExpName <- function(expName=old_expName, ...) {
		return(expName)
	} # EOIF
	md$meta$expName <- modifyExpName(...)
	return(invisible(md))
} # EOF


# Analysis Procedure --------------------------------------

check_apDefaults <- function(fn) {
	stn <- getstn()
	path <- stn$fn_metadata
	if (all(fn == "def")) {
		fn <- stn$fn_anProcDefFile
	}
	if (!all(is.character(fn)) | length(fn) != 1) {
		stop("Please provide a length one character to the argument 'fn', thank you.", call.=FALSE)
	} 
	ok <- file.exists(paste(path, fn, sep="/"))
	if (!ok) {
		stop(paste("The analysis procedure file \"", fn, "\" does not seem to exist. Please check your input.", sep=""), call.=FALSE)
	} else {
		assign("fn", fn, pos=parent.frame(n=1))
	}	
} # EOF

check_apVersion <- function(folderLocal, nameLocal) {
	fiEl <- gl_firstElement_anProc
	tmpl <- gl_templateSuffix_mdAp
	manMax <- gl_maxEmptyAp # the max empty lines after deleting and adding keys	
	#
	aa <- paste(path.package("aquap2"), "/templates/anproc.r", sep="")
	if (!file.exists(aa)) {
		aa <- paste(path.package("aquap2"), "/templates/anproc.r", sep="/inst")	# required in the case of devtools::load_all			
	} # end if	
	pathToLocal <- paste(folderLocal, nameLocal, sep="/")
	out <- checkFileVersionPossiblyModify(pathToPack=aa, folderLocal, nameLocal, pm=fiEl, tmpl, manMax) # pm: the first key in the file	
	if (out == FALSE) {
	#	stop("Please update the analysis procedure files in your experiments according to the latest template.", call.=FALSE)
		stop(call.=FALSE)
	}
} # EOF

getap_core_file <- function(fn="def") {
	check_apDefaults(fn) # is assigning
	stn <- getstn()
	##
	foldLoc <- stn$fn_metadata
	check_apVersion(folderLocal=foldLoc, nameLocal=fn) 	### Version check here !!
	e <- new.env()
	sys.source(paste(foldLoc, fn, sep="/"), envir=e)
	##
	ucl <- list(splitClasses=e$spl.var, splitWl=e$spl.wl)
	if (e$spl.do.csAvg == FALSE) {e$spl.csAvg.raw <- TRUE} # just to be sure that one is true
	csAvg <- list(doAvg=e$spl.do.csAvg, useRaw=e$spl.csAvg.raw)
	if (e$spl.do.noise == FALSE) {e$spl.noise.raw <- TRUE} # just to be sure that one is true
	noise <- list(useNoise=e$spl.do.noise, useRaw=e$spl.noise.raw)
#	if (e$spl.do.exOut == FALSE) {e$spl.exOut.raw <- TRUE} # just to be sure that one is true
	exOut <- list(exOut=e$spl.do.exOut, exOutRaw=e$spl.exOut.raw, exOutVar=e$spl.exOut.var)
	dptModules <- list(dptPre=e$dpt.pre, dptPost=e$dpt.post)
	dpt <- list(csAvg=csAvg, noise=noise, excludeOutliers=exOut, dptModules=dptModules)
	## 
	pca <- list(doPCA=e$do.pca, colorBy=e$pca.colorBy, elci=e$pca.elci, elcolorBy=e$pca.elcolorBy, what=e$pca.what, pcs=e$pca.sc, pcSc=e$pca.sc.pairs, pcLo=e$pca.lo)
	simca <- list(doSIMCA=e$do.sim, simcOn=e$sim.vars, simcK=e$sim.K)
	plsr <- list(doPLSR=e$do.pls, regressOn=e$pls.regOn, ncomp=e$pls.ncomp, valid=e$pls.valid, exOut=e$pls.exOut, colorBy=e$pls.colorBy, what=e$pls.what, inRdp=e$pls.rdp)	
	aquagr <- list(doAqg=e$do.aqg, vars=e$aqg.vars, nrCorr=e$aqg.nrCorr, spectra=e$aqg.spectra, minus=e$aqg.minus, mod=e$aqg.mod, TCalib=e$aqg.TCalib, Texp=e$aqg.Texp, bootCI=e$aqg.bootCI, R=e$aqg.R, smoothN=e$aqg.smoothN, selWls=e$aqg.selWls, msc=e$aqg.msc, reference=e$aqg.reference, fsa=e$aqg.fsa, fss=e$aqg.fss, ccol=e$aqg.ccol, clt=e$aqg.clt, pplot=e$aqg.pplot, plines=e$aqg.plines, discr=e$aqg.discr)	
	da <- list(doDa=e$do.da, type=e$da.type, classOn=e$da.classOn, testCV=e$da.testCV, percTest=e$da.percTest, bootCutoff=e$da.cvBootCutoff, bootFactor=e$da.cvBootFactor, valid=e$da.valid, pcaRed=e$da.pcaRed, pcaNComp=e$da.pcaNComp)
	rnf <- list(doRnf=e$do.rnf, classOn=e$rnf.classOn, testCV=e$rnf.testCV, percTest=e$rnf.percTest, bootCutoff=e$rnf.cvBootCutoff, bootFactor=e$rnf.cvBootFactor, valid=e$rnf.valid, pcaRed=e$rnf.pcaRed, pcaNComp=e$rnf.pcaNComp)
	svm <- list(doSvm=e$do.svm, classOn=e$svm.classOn, testCV=e$svm.testCV, percTest=e$svm.percTest, bootCutoff=e$svm.cvBootCutoff, bootFactor=e$svm.cvBootFactor, valid=e$svm.valid, pcaRed=e$svm.pcaRed, pcaNComp=e$svm.pcaNComp)
	nnet <- list(doNnet=e$do.nnet, classOn=e$nnet.classOn, testCV=e$nnet.testCV, percTest=e$nnet.percTest, bootCutoff=e$nnet.cvBootCutoff, bootFactor=e$nnet.cvBootFactor, valid=e$nnet.valid, pcaRed=e$nnet.pcaRed, pcaNComp=e$nnet.pcaNComp)
	classif <- list(da=da, rnf=rnf, svm=svm, nnet=nnet)
	genPlot <- list(where=e$pg.where, onMain=e$pg.main, onSub=e$pg.sub, fns=e$pg.fns)
	##
	ap <- list(ucl=ucl, dpt=dpt, pca=pca, simca=simca, plsr=plsr, aquagr=aquagr, classif=classif, genPlot=genPlot)
	return(new("aquap_ap", ap))
} #EOF

getap_core <- function(fn, .lafw_fromWhere="load", cube=NULL, ...) {
	### check if any unknown argument is there
	##
	checkTdArgs <- function(pv, ...) {
		a <- substitute(c(...))
		chars <- names(eval(a))
		if (!is.null(chars)) {
			## begin nameX correction
			for (i in 2:20) { # to correct for the 'nameX'behaviour for the arguments that can have more than one value
				ind <- grep(i, chars)
				if (length(ind) != 0) {
					chars <- chars[-ind]
				} # end if
			} # end for i
			ind <- grep(1, chars)
			if (length(ind) != 0) {
				chars[ind] <- substr(chars[ind], 1, nchar(chars[ind])-1) # cut away the "1" from the remaining chars that have a "1" 
			} # end if
			## end nameX correction		
			out <- NULL
			for (i in 1: length(chars)) {
				ind <- which(pv == chars[i])
				if (length(ind) == 0) {
					out <- c(out, i)
				} # end if
			} # end for i
			if (!is.null(out)) {
				msg <- paste("Sorry, the provided arguments '", paste(chars[out], collapse="', '"), "' can not be recognized.", sep="")
				stop(msg, call.=FALSE)
			} # end if
		} # end if (!is.null(chars))
	} # EOIF
	##
	##
	checkTdArgs(pv_tripleDotsMod, ...)
	#####	
	if (.lafw_fromWhere == "load") {
		return(getap_core_file(fn))
	} else {
		if (.lafw_fromWhere == "cube") {
			return(cube@anproc)
		} else {
			stop("An error at obtaining the analysis procedure occured. Please stay calm.", call.=FALSE) # this is not likely to ever happen
		} # end else
	} # end else
} # EOF

modifyThisAp <- function(ap, ...) {
	apMod <- new("aquap_ap")
	###
	UCL <- ap$ucl
	modifyUCL <- function(spl.var=UCL$splitClasses, spl.wl=UCL$splitWl, ...) {
		return(list(splitClasses=spl.var, splitWl=spl.wl))
	} # EOIF
	apMod$ucl <- modifyUCL(...)
	###
	# 	csAvg <- list(doAvg=e$spl.do.csAvg, useRaw=e$spl.csAvg.raw)
	DP <- ap$dpt
	modifyDPT <- function(spl.do.csAvg=DP$csAvg$doAvg, spl.csAvg.raw=DP$csAvg$useRaw, spl.do.noise=DP$noise$useNoise, spl.noise.raw=DP$noise$useRaw, spl.do.exOut=DP$excludeOutliers$exOut, spl.exOut.raw=DP$excludeOutliers$exOutRaw, spl.exOut.var=DP$excludeOutliers$exOutVar, dpt.pre=DP$dptModules$dptPre, dpt.post=DP$dptModules$dptPost, ...) {
		if (spl.do.csAvg == FALSE) {spl.csAvg.raw <- TRUE} # just to be sure that one is true
		if (spl.do.noise == FALSE) {spl.noise.raw <- TRUE}
		csAvg <- list(doAvg=spl.do.csAvg, useRaw=spl.csAvg.raw)
		noise <- list(useNoise=spl.do.noise, useRaw=spl.noise.raw)
		exOut <- list(exOut=spl.do.exOut, exOutRaw=spl.exOut.raw, exOutVar=spl.exOut.var)
		dptModules <- list(dptPre=dpt.pre , dptPost=dpt.post )
		return(list(csAvg=csAvg, noise=noise, excludeOutliers=exOut, dptModules=dptModules))
	} # EOIF
	apMod$dpt <- modifyDPT(...)
	###
	checkDo <- function(root, do) { # provide do as character
		if (is.null(root)) {		# the whole root is not here
			return(FALSE)
		} else {
			if (is.null(root[do][[1]])) { # the root is here, but no "do.xxx" directive (coming from the cube)
				return(TRUE)
			}
			if (root[do][[1]]) {	 # so if the "do.xxx" is TRUE - root is here, "do.xxx" directive is here (coming from loading)
				return(TRUE)
			} else {
				return(FALSE) 		# root is here, do.xxx is FALSE - (coming from loading)
			}
		}
	} # EOIF
	###
	PC <- ap$pca
#	if (is.null(PC)) {doIt <- FALSE} else { if (PC$doPCA) {doIt <- TRUE} else {doIt <- FALSE}} # have to correct for the possibility that we get the core-ap from the cube: in this case we do not have the "do.XXX" argument
	doIt <- checkDo(PC, "doPCA")
	modifyPCA <- function(do.pca=doIt, pca.colorBy=PC$colorBy, pca.elci=PC$elci, pca.elcolorBy=PC$elcolorBy, pca.what=PC$what, pca.sc=PC$pcs, pca.sc.pairs=PC$pcSc, pca.lo=PC$pcLo, ...) { # define the default of the function as the value coming in in the analysis procedure ap (getap_core); need the ... to "ignore" all the other arguments that do not match
		if (!do.pca) {
			return(NULL)
		} else {
			return(list(colorBy=pca.colorBy, elci=pca.elci, elcolorBy=pca.elcolorBy, what=pca.what, pcs=pca.sc, pcSc=pca.sc.pairs, pcLo=pca.lo)) # return the same name as in the ap before
		}
	} # EOIF
	apMod$pca <- modifyPCA(...) # if no values are provided in ... , then the defaults (who are all the values from the .r file) are taken. If one or more values are provided, they replace the default.
	###
	SI <- ap$simca
	doIt <- checkDo(SI, "doSIMCA")
	modifySIMCA <- function(do.sim=doIt, sim.vars=SI$simcOn, sim.K=SI$simcK, ...) {
		if (!do.sim) {
			return(NULL)
		} else {
			return(list(simcOn=sim.vars, simcK=sim.K))
		}
	} # EOIF
	apMod$simca <- modifySIMCA(...)
	###
	PL <- ap$plsr
	doIt <- checkDo(PL, "doPLSR")
	modifyPLSR <- function(do.pls=doIt, pls.regOn=PL$regressOn, pls.ncomp=PL$ncomp, pls.valid=PL$valid, pls.exOut=PL$exOut, pls.colorBy=PL$colorBy, pls.what=PL$what, pls.rdp=PL$inRdp, ...) {
		if (!do.pls) { 
			return(NULL)
		} else {
			return(list(regressOn=pls.regOn, ncomp=pls.ncomp, valid=pls.valid, exOut=pls.exOut, colorBy=pls.colorBy, what=pls.what, inRdp=pls.rdp))
		}
	} # EOIF
	apMod$plsr <- modifyPLSR(...)	
	###
	AQ <- ap$aquagr
	doIt <- checkDo(AQ, "doAqg")
	modifyAquagram <- function(do.aqg=doIt, aqg.vars=AQ$vars, aqg.nrCorr=AQ$nrCorr, aqg.spectra=AQ$spectra, aqg.minus=AQ$minus, aqg.mod=AQ$mod, aqg.TCalib=AQ$TCalib, aqg.Texp=AQ$Texp, aqg.bootCI=AQ$bootCI, aqg.R=AQ$R, aqg.smoothN=AQ$smoothN, aqg.selWls=AQ$selWls, aqg.msc=AQ$msc, aqg.reference=AQ$reference, aqg.fsa=AQ$fsa, aqg.fss=AQ$fss, aqg.ccol=AQ$ccol, aqg.clt=AQ$clt, aqg.pplot=AQ$pplot, aqg.plines=AQ$plines, aqg.discr=AQ$discr, ...) {
		if (!do.aqg) {
			return(NULL)
		} else {
			return(list(vars=aqg.vars, nrCorr=aqg.nrCorr, spectra=aqg.spectra, minus=aqg.minus, mod=aqg.mod, TCalib=aqg.TCalib, Texp=aqg.Texp, bootCI=aqg.bootCI, R=aqg.R, smoothN=aqg.smoothN, selWls=aqg.selWls, msc=aqg.msc, reference=aqg.reference, fsa=aqg.fsa, fss=aqg.fss, ccol=aqg.ccol, clt=aqg.clt, pplot=aqg.pplot, plines=aqg.plines, discr=aqg.discr))
		}
	} # EOIF
	apMod$aquagr <- modifyAquagram(...)
	###
	DA <- ap$classif$da
	doIt <- checkDo(DA, "doDa")
	modifyDA <- function(do.da=doIt, da.type=DA$type, da.classOn=DA$classOn, da.testCV=DA$testCV, da.percTest=DA$percTest, da.cvBootCutoff=DA$bootCutoff, da.cvBootFactor=DA$bootFactor, da.valid=DA$valid, da.pcaRed=DA$pcaRed, da.pcaNComp=DA$pcaNComp, ...) {
		if (!do.da) { 
			return(NULL)
		} else {
			return(list(type=da.type, classOn=da.classOn, testCV=da.testCV, percTest=da.percTest, bootCutoff=da.cvBootCutoff, bootFactor=da.cvBootFactor, valid=da.valid, pcaRed=da.pcaRed, pcaNComp=da.pcaNComp))
		}
	} # EOIF
	apMod$classif$da <- modifyDA(...)
	###
	RNF <- ap$classif$rnf
	doIt <- checkDo(RNF, "doRnf")
	modifyRnf <- function(do.rnf=doIt, rnf.classOn=RNF$classOn, rnf.testCV=RNF$testCV, rnf.percTest=RNF$percTest, rnf.cvBootCutoff=RNF$bootCutoff, rnf.cvBootFactor=RNF$bootFactor, rnf.valid=RNF$valid,  rnf.pcaRed=RNF$pcaRed, rnf.pcaNComp=RNF$pcaNComp, ...) {
		if (!do.rnf) { 
			return(NULL)
		} else {
			return(list(classOn=rnf.classOn, testCV=rnf.testCV, percTest=rnf.percTest, bootCutoff=rnf.cvBootCutoff, bootFactor=rnf.cvBootFactor, valid=rnf.valid, pcaRed=rnf.pcaRed, pcaNComp=rnf.pcaNComp))
		}
	} # EOIF
	apMod$classif$rnf <- modifyRnf(...)
	###
	SVM <- ap$classif$svm
	doIt <- checkDo(SVM, "doSvm")
	modifySVM <- function(do.svm=doIt, svm.classOn=SVM$classOn, svm.testCV=SVM$testCV, svm.percTest=SVM$percTest, svm.cvBootCutoff=SVM$bootCutoff, svm.cvBootFactor=SVM$bootFactor, svm.valid=SVM$valid, svm.pcaRed=SVM$pcaRed, svm.pcaNComp=SVM$pcaNComp, ...) {
		if (!do.svm) { 
			return(NULL)
		} else {
			return(list(classOn=svm.classOn, testCV=svm.testCV, percTest=svm.percTest, bootCutoff=svm.cvBootCutoff, bootFactor=svm.cvBootFactor, valid=svm.valid, pcaRed=svm.pcaRed, pcaNComp=svm.pcaNComp))
		}
	} # EOIF
	apMod$classif$svm <- modifySVM(...)
	###
	NN <- ap$classif$nnet
	doIt <- checkDo(NN, "doNnet")
	modifyNnet <- function(do.nnet=doIt, nnet.classOn=NN$classOn, nnet.testCV=NN$testCV, nnet.percTest=NN$percTest, nnet.cvBootCutoff=NN$bootCutoff, nnet.cvBootFactor=NN$bootFactor, nnet.valid=NN$valid,  nnet.pcaRed=NN$pcaRed, nnet.pcaNComp=NN$pcaNComp, ...) {
		if (!do.nnet) { 
			return(NULL)
		} else {
			return(list(classOn=nnet.classOn, testCV=nnet.testCV, percTest=nnet.percTest, bootCutoff=nnet.cvBootCutoff, bootFactor=nnet.cvBootFactor, valid=nnet.valid, pcaRed=nnet.pcaRed, pcaNComp=nnet.pcaNComp))
		}
	} # EOIF
	apMod$classif$nnet <- modifyNnet(...)
	###
	cnt <- checkForStats(apMod)$cnt # returns 0 if not a single model has been calculated; we have do check at the modified ap !!
	GP <- ap$genPlot
	modifyGenPlot <- function(pg.where=GP$where, pg.main=GP$onMain, pg.sub=GP$onSub, pg.fns=GP$fns, ...) {
		if (cnt == 0) {
		#	return(NULL) # why this?
			return(list(where=pg.where, onMain=pg.main, onSub=pg.sub, fns=pg.fns)) # modify XXX, for being able to plot the spectra!
		} else {
			return(list(where=pg.where, onMain=pg.main, onSub=pg.sub, fns=pg.fns))
		}	
	} # EOIF
	apMod$genPlot <- modifyGenPlot(...)
	###	
	ap_check_dptModules(apMod)
	###
	return(apMod)
} # EOF

#' @title Get Analysis Procedure
#' @description Read in the analysis procedure from the default or a custom 
#' analysis procedure file located in the metadata-folder. By providing any of 
#' the arguments of the analysis procedure file (see \code{\link{anproc_file}}) 
#' to the function you can override the values in the file with the provided 
#' values.
#' @details The name of the default analysis procedure file can be specified in 
#' the settings. The provided value and defaults will be checked in 
#' \code{\link{gdmm}} and the resulting \code{\link{aquap_cube}} contains the 
#' final analysis procedure in its slot @@anproc.
#' @param fn Character length one. The filename of the analysis procedure file 
#' to load. If left at 'def', the default filename for an analysis procedure 
#' file as specified in the settings (factory default is "anproc.r") is read in. 
#' Provide any other valid name of an analysis procedure file to load it. (Do not 
#' forget the '.r' at the end.)
#' @param ... Any of the arguments of the analysis procedure - please see 
#' \code{\link{anproc_file}}. Any argument/value provided via \code{...} will 
#' override the value in the analysis procedure .r file.
#' @return An (invisible) list with the analysis procedure.
#' @seealso \code{\link{anproc_file}}, \code{\link{getmd}}, \code{\link{gdmm}}
#' @examples
#' \dontrun{
#' ap <- getap(); str(ap); names(ap)
#' ap <- getap("myFile.r")
#' ap <- getap(pca.colorBy="C_Group") # change the value of 'pca.colorBy'
#' from the .r file to 'C_Group'
#' ap <- getap(do.sim=FALSE) # switch off the calculation of SIMCA models
#' ap <- getap(spl.var="C_Group") # change the split variable to "C_Group"
#' }
#' @export
getap <- function(fn="def", ...) {
	autoUpS()
	ap <- getap_core(fn, ...) 	# first load the analysis procedure as defined in the .r file, then possibly modify it. If no additional arguments get supplied by the  user, the original values from the .r file get passed on.
								# depending on a possible additional argument in ... (.lafw_fromWhere), either the ap from the file is loaded, or,  in case of a call from a plotting function, the ap from the cube (what then is also present in the ... argument) is taken
	###
	apMod <- modifyThisAp(ap, ...)
	return(invisible(apMod))
} # EOF
