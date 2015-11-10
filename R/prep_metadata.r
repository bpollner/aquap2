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

check_mdDefaultValues <- function(localEnv) {
	# realMeasurementLabel  
	le <- localEnv
	#
	noSplitVal <- le$commonValue
	if (all(noSplitVal == "def")) {
		noSplitVal <- .ap2$stn$p_commonNoSplit
	}
	if (length(noSplitVal) != 1 | !all(is.character(noSplitVal))) {
		stop("Please provide a character length one to the variable 'commonValue' in the metadata file.", call.=FALSE)
	}
	assign("noSplitLabel", noSplitVal, pos=parent.frame(n=1))
	#
	ecl <- le$envControlLabel 
	if (all(ecl == "def")) {
		ecl <- .ap2$stn$p_envControlLabel
	}
	if (length(ecl) != 1 | !all(is.character(ecl))) {
		stop("Please provide a character length one to the variable 'envControlLabel' in the metadata file.", call.=FALSE)
	}
	assign("ECLabel", ecl, pos=parent.frame(n=1))	
	#
	rml <- le$realMeasurementLabel
	if (all(rml == "def")) {
		rml <- .ap2$stn$p_realMeasurementLabel
	}
	if (length(rml) != 1 | !all(is.character(rml))) {
		stop("Please provide a character length one to the variable 'realMeasurementLabel' in the metadata file.", call.=FALSE)
	}
	assign("RMLabel", rml, pos=parent.frame(n=1))	
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
	defReplPref <- .ap2$stn$p_replicatePrefix
	defNoTime <- .ap2$stn$p_noTimePointsLabel
	##
#	expNameCol <- paste(clPref, .ap2$stn$p_expNameCol, sep="")
#	sampleNrCol <- paste(yPref, .ap2$stn$p_sampleNrCol, sep="")
#	conSNrCol <- paste(yPref, .ap2$stn$p_conSNrCol, sep="")
#	deleteCol <- paste(clPref, .ap2$stn$p_deleteCol, sep="")
	### the above 4 are not used here
	path <- .ap2$stn$fn_metadata
	path <- paste(path, fn, sep="/")
	e <- new.env()
	sys.source(path, envir=e)
	check_mdVersion(localEnv=e) 	### Version check here !!
	noSplitLabel <- ECLabel <- RMLabel <- NULL # gets assigned below
	check_mdDefaultValues(localEnv=e) ### checking and assigning
	##
	Repls <- (paste(defReplPref, seq(1, e$Repls), sep=""))
	Group <- e$Group
	if (all(e$TimePoints == FALSE)) {
		TimePoints <- defNoTime
	} else {
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
 	postProc <- list(spacing=e$spacing, ECRMLabel=c(ECLabel, RMLabel), noSplitLabel=noSplitLabel, nrConScans=e$nrConScans)
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
#' @param fn Character length one.  The filename of the metadata file to load. 
#' If left at 'def', the default filename for 
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
#' dataset <- gfd(getmd(expName="OtherName"))
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

getap_core_file <- function(fn="def") {
	check_apDefaults(fn)
	##
	path <- .ap2$stn$fn_metadata
	path <- paste(path, fn, sep="/")
	e <- new.env()
	sys.source(path, envir=e)
	check_apVersion(localEnv=e) 	### Version check here !!
	##
	ucl <- list(splitClasses=e$spl.var, splitWl=e$spl.wl)
	if (e$spl.do.smo == FALSE) {e$spl.smo.raw <- TRUE} # just to be sure that one is true
	smoothing <- list(useSmooth=e$spl.do.smo, useRaw=e$spl.smo.raw)
	if (e$spl.do.noise == FALSE) {e$spl.noise.raw <- TRUE} # just to be sure that one is true
	noise <- list(useNoise=e$spl.do.noise, useRaw=e$spl.noise.raw)
	dpt <- list(smoothing=smoothing, noise=noise)
	##
	pca.elci <- 0.95						## the confidence interval for the ellipse to draw around groups in score plots; set to NULL for not drawing ellipses at all
	pca.el2colorBy <- NULL					## which variables to use for plotting additional confidence intervall ellipses. Set to NULL for using the same as in pca.colorBy. Provide one variable (gets recycled) or a vector with equal length as pca.colorBy to have the additional ci-ellipses along these variables.

	pca <- list(doPCA=e$do.pca, colorBy=e$pca.colorBy, elci=e$pca.elci, elcolorBy=e$pca.elcolorBy, what=e$pca.what, pcs=e$pca.sc, pcSc=e$pca.sc.pairs, pcLo=e$pca.lo)
	simca <- list(doSIMCA=e$do.sim, simcOn=e$sim.vars, simcK=e$sim.K)
	plsr <- list(doPLSR=e$do.pls, regressOn=e$pls.regOn, ncomp=e$pls.ncomp, valid=e$pls.valid, colorBy=e$pls.colorBy)	
	aquagr <- list(doAqg=e$do.aqg, vars=e$aqg.vars, nrCorr=e$aqg.nrCorr, spectra=e$aqg.spectra, minus=e$aqg.minus, mod=e$aqg.mod, TCalib=e$aqg.TCalib, Texp=e$aqg.Texp, bootCI=e$aqg.bootCI, R=e$aqg.R, smoothN=e$aqg.smoothN, selWls=e$aqg.selWls, msc=e$aqg.msc, reference=e$aqg.reference, fsa=e$aqg.fsa, fss=e$aqg.fss, ccol=e$aqg.ccol, clt=e$aqg.clt, pplot=e$aqg.pplot, plines=e$aqg.plines, discr=e$aqg.discr)	
	genPlot <- list(where=e$pg.where, onMain=e$pg.main, onSub=e$pg.sub, fns=e$pg.fns)
	##
	ap <- list(ucl=ucl, dpt=dpt, pca=pca, simca=simca, plsr=plsr, aquagr=aquagr, genPlot=genPlot)
	return(new("aquap_ap", ap))
} #EOF

getap_core <- function(fn, .lafw_fromWhere="load", cube=NULL, ...) {
	### check if any unknown argument is there
	checkTdArgs <- function(pv, ...) {
		a <- substitute(c(...))
		chars <- names(eval(a))
		if (!is.null(chars)) {
		## begin nameX correction
		for (i in 2:20) { # to correct for the 'nameX'behaviour for the arguments that can have more than one value
			ind <- grep(i, chars)
			if (length(ind) != 0) {
				chars <- chars[-ind]
			}
		} # end for i
		ind <- grep(1, chars)
		if (length(ind) != 0) {
				chars[ind] <- substr(chars[ind], 1, nchar(chars[ind])-1) # cut away the "1" from the remaining chars that have a "1" 
		}
		## end nameX correction		
			out <- NULL
			for (i in 1: length(chars)) {
				ind <- which(pv == chars[i])
				if (length(ind) == 0) {
					out <- c(out, i)
				}
			}
			if (!is.null(out)) {
				msg <- paste("Sorry, the provided arguments '", paste(chars[out], collapse="', '"), "' can not be recognized.", sep="")
				stop(msg, call.=FALSE)
			}
		}
	} # EOIF
	checkTdArgs(pv_tripleDotsMod, ...)
	#####	
	if (.lafw_fromWhere == "load") {
		return(getap_core_file(fn))
	} else {
		if (.lafw_fromWhere == "cube") {
			return(cube@anproc)
		} else {
			stop("An error at obtaining the analysis procedure occured. Please stay calm.", call.=FALSE) # this is not likely to ever happen
		}
	}
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
#' @return A list with the analysis procedure.
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
	apMod <- new("aquap_ap")
	###
	UCL <- ap$ucl
	modifyUCL <- function(spl.var=UCL$splitClasses, spl.wl=UCL$splitWl, ...) {
		return(list(splitClasses=spl.var, splitWl=spl.wl))
	} # EOIF
	apMod$ucl <- modifyUCL(...)
	###
	DP <- ap$dpt
	modifyDPT <- function(spl.do.smo=DP$smoothing$useSmooth, spl.smo.raw=DP$smoothing$useRaw, spl.do.noise=DP$noise$useNoise, spl.noise.raw=DP$noise$useRaw, ...) {
		if (spl.do.smo == FALSE) {spl.smo.raw <- TRUE} # just to be sure that one is true
		if (spl.do.noise == FALSE) {spl.noise.raw <- TRUE}
		smoothing <- list(useSmooth=spl.do.smo, useRaw=spl.smo.raw)
		noise <- list(useNoise=spl.do.noise, useRaw=spl.noise.raw)
		return(list(smoothing=smoothing, noise=noise))
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
	modifyPLSR <- function(do.pls=doIt, pls.regOn=PL$regressOn, pls.ncomp=PL$ncomp, pls.valid=PL$valid, pls.colorBy=PL$colorBy, ...) {
		if (!do.pls) {
			return(NULL)
		} else {
			return(list(regressOn=pls.regOn, ncomp=pls.ncomp, valid=pls.valid, colorBy=pls.colorBy))
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
	cnt <- checkForStats(apMod)$cnt # returns 0 if not a single model has been calculated; we have do check at the modified ap !!
	GP <- ap$genPlot
	modifyGenPlot <- function(pg.where=GP$where, pg.main=GP$onMain, pg.sub=GP$onSub, pg.fns=GP$fns, ...) {
		if (cnt == 0) {
			return(NULL)
		} else {
			return(list(where=pg.where, onMain=pg.main, onSub=pg.sub, fns=pg.fns))
		}	
	} # EOIF
	apMod$genPlot <- modifyGenPlot(...)
	###	
	return(apMod)
} # EOF
