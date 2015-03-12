# Metadata --------------------------------------------
check_mdDefaults <- function(fn) {
	path <- .ap2$stn$fn_metadata
	if (all(fn == "def")) {
		fn <- .ap2$stn$fn_mDataDefFile
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

copyMetadataFile <- function(fromPath, toPath) {
	ok <- file.copy(fromPath, toPath, overwrite=TRUE)
	if (ok) { cat("A fresh template of the metadata file has been copied from the package.\n")}			
} # EOF

check_mdVersion_I <- function(localEnv) {
	le <- localEnv
	mdPacPath <- paste(path.package("aquap2"), "/templates/metadata.r", sep="")
	pathSH <- Sys.getenv("AQUAP2SH")
	pe <- new.env()
	sys.source(mdPacPath, envir=pe)
	locNames <- ls(le)
	pacNames <- ls(pe)
	if (!identical(locNames, pacNames)) {
		okInd <- which(pacNames %in% locNames)
		miss <- pacNames[-okInd]
		delInd <- which(locNames %in% pacNames)
		del <- locNames[-delInd]
		msgNew <- "The new variables are:"
		msgDel <- "The following variables have been deleted:"
		#
		message("There appears to be a newer version of the template for the metadata file in the package \"aquap2\".")
		if (length(miss) != 0 & length(del) == 0) {
			message(msgNew) ;   message(paste(miss, collapse=", "))
		} else {
			if (length(miss) == 0 & length(del) != 0) {
				message(msgDel); 	message(paste(del, collapse=", "))
			} else {
				message(msgNew) ;   message(paste(miss, collapse=", "))
				message(msgDel); 	message(paste(del, collapse=", "))
			}
		}
		message(paste("Do you want to copy it now into \n\"", pathSH, "\" \nas a template to modify the metadata-files in your experiment accordingly? \n( y / n )", sep=""))
		a <- readLines(n=1)
		if (a != "y" & a != "Y") {
			message("Please be aware that the package will not work properly if your metadata files are not up to date.")
			out <- FALSE
		} else {
			copyMetadataFile(mdPacPath, pathSH)
			out <- FALSE
		}
	} else { # so we are identical, everything ok
		out <- TRUE
	}
	if (out == FALSE) {
		stop("Please update the metadata files in your experiments according to the latest template.", call.=FALSE)
	}
} # EOF

check_mdVersion <- function(localEnv) {
	if (is.null(.ap2$.devMode)) {
		check_mdVersion_I(localEnv)
	}
} # EOF


getmd_core <- function(fn="def") {
	check_mdDefaults(fn)
	#
	clPref <- .ap2$stn$p_ClassVarPref
	yPref <- .ap2$stn$p_yVarPref
	timeCol <- paste(clPref, .ap2$stn$p_timeCol, sep="")
	ecrmCol <- paste(clPref, .ap2$stn$p_ECRMCol, sep="")
	replCol <- paste(clPref, .ap2$stn$p_replicateCol, sep="")
	groupCol <- paste(clPref, .ap2$stn$p_groupCol, sep="")
	commCol <- paste(clPref, .ap2$stn$p_commonNoSplitCol, sep="")
	tempCol <- paste(yPref, .ap2$stn$p_tempCol, sep="")
	rhCol <- paste(yPref, .ap2$stn$p_RHCol, sep="")
	# defaults:
	defNoSplit <- .ap2$stn$p_commonNoSplit
	defECLabel <- .ap2$stn$p_envControlLabel
	defRMLabel <- .ap2$stn$p_realMeasurementLabel
	defReplPref <- .ap2$stn$p_replicatePrefix
	defNoTime <- .ap2$stn$p_noTimePointsLabel
	##
	expNameCol <- paste(clPref, .ap2$stn$p_expNameCol, sep="")
	sampleNrCol <- paste(yPref, .ap2$stn$p_sampleNrCol, sep="")
	conSNrCol <- paste(yPref, .ap2$stn$p_conSNrCol, sep="")
	deleteCol <- paste(clPref, .ap2$stn$p_deleteCol, sep="")
	### the above 4 are not used here
	path <- .ap2$stn$fn_metadata
	path <- paste(path, fn, sep="/")
	e <- new.env()
	sys.source(path, envir=e)
	check_mdVersion(localEnv=e) 	### Version check here !!
	##
#	Repls <- list(paste(defReplPref, seq(1, e$Repls), sep="")) 		
	Repls <- (paste(defReplPref, seq(1, e$Repls), sep=""))
#	Group <- list(e$Group)										
	Group <- e$Group
#	ECRMlabel <- list(defECLabel, defRMLabel)						
	ECRMlabel <- c(defECLabel, defRMLabel)
#	noSplitLabel <- list(defNoSplit)
	noSplitLabel <- defNoSplit
	if (all(e$TimePoints == FALSE)) {
#		TimePoints <- list(defNoTime)
		TimePoints <- defNoTime
	} else {
#		TimePoints <- list(e$TimePoints)							# 	TimePoints=list("TN")
		TimePoints <- e$TimePoints
	}
	if (length(e$columnNamesL1) != length(e$columnNamesL2)) {
		stop("Please provide two equally long inputs for the column names in L1 and L2", call.=FALSE)
	}
	userCols <- NULL
	for (i in 1: length(e$columnNamesL1)) {
		userCols <- c(userCols, e$columnNamesL1[i], e$columnNamesL2[i])
	}
	coluNames <- c(timeCol, ecrmCol, userCols, replCol, groupCol, tempCol, rhCol)
	###
	## put together
 	expClasses <- list(L1=e$L1, L2=e$L2, Repls=Repls, Group=Group, timeLabels=TimePoints)
 	postProc <- list(spacing=e$spacing, ECRMLabel=ECRMlabel, noSplitLabel=noSplitLabel, nrConScans=e$nrConScans)
 	meta <- list(expName=e$expName, coluNames=coluNames)
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
#' @param fn Character length one. If left at 'def', the default filename for 
#' a metadata-file as specified in the settings (what is "metadata.r") is read 
#' in. Provide any other valid name of a metadata-file to load it. (Do not forget 
#' the '.r' at the end.)
#' @param ... Provide the argument 'expName' to override the experiment name 
#' specified in the metadata.r file.
#' @return A list with all the metadata of the experiment.
#' @seealso \code{\link{metadata_file}}, \code{\link{getap}}, \code{\link{gdmm}}
#' @examples
#' \dontrun{
#' md <- getmd(); str(md); names(md)
#' md <- getmd("myFile.r")
#' md <- getmd(expName="OtherName") # to override the experiment name as specified 
#' # in the metadata.r file.
#' }
#' @export
getmd <- function(fn="def", ...) {
	autoUpS()
	md <- getmd_core(fn)
	old_expName <- md$meta$expName
	modifyExpName <- function(expName=old_expName, ...) {
		return(expName)
	} # EOIF
	md$meta$expName <- modifyExpName(...)
	return(md)
} # EOF


# Analysis Procedure --------------------------------------

check_apDefaults <- function(fn) {
	path <- .ap2$stn$fn_metadata
	if (all(fn == "def")) {
		fn <- .ap2$stn$fn_anProcDefFile
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

copyAnProcFile <- function(fromPath, toPath) {
	ok <- file.copy(fromPath, toPath, overwrite=TRUE)
	if (ok) { cat("A fresh template of the analysis procedure file has been copied from the package.\n")}			
} # EOF

check_apVersion_I <- function(localEnv) {
	le <- localEnv
	apPacPath <- paste(path.package("aquap2"), "/templates/anproc.r", sep="")
	pathSH <- Sys.getenv("AQUAP2SH")
	pe <- new.env()
	sys.source(apPacPath, envir=pe)
	locNames <- ls(le)
	pacNames <- ls(pe)
	if (!identical(locNames, pacNames)) {
		okInd <- which(pacNames %in% locNames)
		miss <- pacNames[-okInd]
		delInd <- which(locNames %in% pacNames)
		del <- locNames[-delInd]
		msgNew <- "The new variables are:"
		msgDel <- "The following variables have been deleted:"
		#
		message("There appears to be a newer version of the template for the analysis procedure file in the package \"aquap2\".")
		if (length(miss) != 0 & length(del) == 0) {
			message(msgNew) ;   message(paste(miss, collapse=", "))
		} else {
			if (length(miss) == 0 & length(del) != 0) {
				message(msgDel); 	message(paste(del, collapse=", "))
			} else {
				message(msgNew) ;   message(paste(miss, collapse=", "))
				message(msgDel); 	message(paste(del, collapse=", "))
			}
		}
		message(paste("Do you want to copy it now into \n\"", pathSH, "\" \nas a template to modify the analysis procedure files in your experiment accordingly? \n( y / n )", sep=""))
		a <- readLines(n=1)
		if (a != "y" & a != "Y") {
			message("Please be aware that the package will not work properly if your analysis procedure files are not up to date.")
			out <- FALSE
		} else {
			copyAnProcFile(apPacPath, pathSH)
			out <- FALSE
		}
	} else { # so we are identical, everything ok
		out <- TRUE
	}
	if (out == FALSE) {
		stop("Please update the analysis procedure files in your experiments according to the latest template.", call.=FALSE)
	}
} # EOF

check_apVersion <- function(localEnv) {
	if (is.null(.ap2$.devMode)) {
		check_apVersion_I(localEnv)
	}
} # EOF

getap_core <- function(fn="def") {
	check_apDefaults(fn)
	##
	path <- .ap2$stn$fn_metadata
	path <- paste(path, fn, sep="/")
	e <- new.env()
	sys.source(path, envir=e)
	check_apVersion(localEnv=e) 	### Version check here !!
	##
	ucl <- list(splitClasses=e$splitByVariable, splitWl=e$splitByWavelength)
	if (e$do_smooth == FALSE) {e$smooth_useRaw <- TRUE} # just to be sure that one is true
	smoothing <- list(useSmooth=e$do_smooth, useRaw=e$smooth_useRaw)
	if (e$do_noiseTest == FALSE) {e$noiseTest_useRaw <- TRUE} # just to be sure that one is true
	noise <- list(useNoise=e$do_noiseTest, useRaw=e$noiseTest_useRaw)
	dpt <- list(smoothing=smoothing, noise=noise)
	##
	pca <- list(doPCA=e$do.pca, colorBy=e$pca.colorBy)
	simca <- list(doSIMCA=e$do.sim, simcOn=e$sim.vars, simcK=e$sim.k)
	plsr <- list(doPLSR=e$do.pls, regressOn=e$pls.regOn, ncomp=e$pls.ncomp, valid=e$pls.valid, colorBy=e$pls.colorBy)	
	aquagr <- list(doAqg=e$do.aqg, vars=e$aqg.vars, nrCorr=e$aqg.nrCorr, spectra=e$aqg.spectra, minus=e$aqg.minus, TCalib=e$aqg.TCalib, Texp=e$aqg.Texp, bootCI=e$aqg.bootCI, R=e$aqg.R, smoothN=e$aqg.smoothN, selWls=e$aqg.selWls, msc=e$aqg.msc, reference=e$aqg.reference)	
	##
	ap <- list(ucl=ucl, dpt=dpt, pca=pca, simca=simca, plsr=plsr, aquagr=aquagr)
	return(new("aquap_ap", ap))
} #EOF

#' @title Get Analysis Procedure
#' @description Read in the analysis procedure from the default or a custom 
#' analysis procedure file located in the metadata-folder. By providing any of 
#' the arguments of the statistics section of the analysis procedure file 
#' (see \code{\link{anproc_file}} to the function you can override the values 
#' in the file with the provided values.
#' @details The name of the default analysis procedure file can be specified in 
#' the settings. Other arguments than precise matches to the available arguments 
#' will be ignored. Arguments in the 'split dataset' section of the analysis 
#' procedure get exclusively read in from file.
#' @param fn Character length one. If left at 'def', the default filename for an 
#' analysis procedure file as specified in the settings (factory default is 
#' "anproc.r") is read in. Provide any other valid name of an analysis procedure 
#' file to load it. (Do not forget the '.r' at the end.)
#' @param ... Any of the arguments of the 'statistics' section as defined in the 
#' analysis procedure - please  see \code{\link{anproc_file}}. 
#' Any argument/value provided via \code{...} will  override the value in the 
#' analysis procedure .r file.
#' @return A list with the analysis procedure.
#' @seealso \code{\link{anproc_file}}, \code{\link{getmd}}, \code{\link{gdmm}}
#' @examples
#' \dontrun{
#' ap <- getap(); str(ap); names(ap)
#' ap <- getap("myFile.r")
#' ap <- getap(pca.colorBy="C_Group") # change the value of 'pca.colorBy'
#' from the .r file to 'C_Group'
#' ap <- getap(do.sim=FALSE) # switch off the calculation of SIMCA models
#' }
#' @export
getap <- function(fn="def", ...) {
	autoUpS()
	ap <- getap_core(fn) # first load the analysis procedure as defined in the .r file, then possibly modify it. If no additional arguments get supplied by the  user, the original values from the .r file get passed on.
	apMod <- new("aquap_ap")
	apMod$ucl <- ap$ucl # copy unchanged
	apMod$dpt <- ap$dpt # copy unchanged
	###
	PC <- ap$pca
	modifyPCA <- function(do.pca=PC$doPCA, pca.colorBy=PC$colorBy, ...) { # define the default of the function as the value coming in in the analysis procedure ap (getap_core); need the ... to "ignore" all the other arguments that do not match
		if (!do.pca) {
			return(NULL)
		} else {
			return(list(colorBy=pca.colorBy)) # return the same name as in the ap before
		}
	
	} # EOIF
	apMod$pca <- modifyPCA(...) # if no values are provided in ... , then the defaults (who are all the values from the .r file) are taken. If one or more values are provided, they replace the default.
	###
	SI <- ap$simca
	modifySIMCA <- function(do.sim=SI$doSIMCA, sim.vars=SI$simcOn, sim.K=SI$simcK, ...) {
		if (!do.sim) {
			return(NULL)
		} else {
			return(list(simcOn=sim.vars, simcK=sim.K))
		}
	} # EOIF
	apMod$simca <- modifySIMCA(...)
	###
	PL <- ap$plsr
	modifyPLSR <- function(do.pls=PL$doPLSR, pls.regOn=PL$regressOn, pls.ncomp=PL$ncomp, pls.valid=PL$valid, pls.colorBy=PL$colorBy, ...) {
		if (!do.pls) {
			return(NULL)
		} else {
			return(list(regressOn=pls.regOn, ncomp=pls.ncomp, valid=pls.valid, colorBy=pls.colorBy))
		}
	} # EOIF
	apMod$plsr <- modifyPLSR(...)	
	###
	AQ <- ap$aquagr
	modifyAquagram <- function(do.aqg=AQ$doAqg, aqg.vars=AQ$vars, aqg.nrCorr=AQ$nrCorr, aqg.spectra=AQ$spectra, aqg.minus=AQ$minus, aqg.TCalib=AQ$TCalib, aqg.Texp=AQ$Texp, aqg.bootCI=AQ$bootCI, aqg.R=AQ$R, aqg.smoothN=AQ$smoothN, aqg.selWls=AQ$selWls, aqg.msc=AQ$msc, aqg.reference=AQ$reference, ...) {
		if (!do.aqg) {
			return(NULL)
		} else {
			return(list(vars=aqg.vars, nrCorr=aqg.nrCorr, spectra=aqg.spectra, minus=aqg.minus, TCalib=aqg.TCalib, Texp=aqg.Texp, bootCI=aqg.bootCI, R=aqg.R, smoothN=aqg.smoothN, selWls=aqg.selWls, msc=aqg.msc, reference=aqg.reference))
		}
	} # EOIF
	apMod$aquagr <- modifyAquagram(...)
	###
#	print(str(apMod)); wait()
	return(apMod)
} # EOF
