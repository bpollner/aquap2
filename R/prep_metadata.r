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

#' @title Get Metadata
#' @description Read in the metadata from the default or a custom metadata file 
#' located in the metadata-folder.
#' @details The name of the default metadata-file can be specified in the settings..
#' @param fn Character length one. If left at 'def', the default filename for 
#' a metadata-file as specified in the settings (what is "metadata.r") is read 
#' in. Provide any other valid name of a metadata-file to load it. (Do not forget 
#' the '.r' at the end.)
#' @return A list with all the metadata of the experiment.
#' @seealso \code{\link{metadata_file}}
#' @examples
#' \dontrun{
#' md <- getmd()
#' md <- getmd("myFile.r")
#' }
#' @export
getmd <- function(fn="def") {
	autoUpS()
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
	#####
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
	#####
	## put together
 	expClasses <- list(L1=e$L1, L2=e$L2, Repls=Repls, Group=Group, timeLabels=TimePoints)
 	postProc <- list(spacing=e$spacing, ECRMLabel=ECRMlabel, noSplitLabel=noSplitLabel, nrConScans=e$nrConScans)
 	meta <- list(expName=e$experimentName, coluNames=coluNames)
	expMetaData <- list(expClasses = expClasses, postProc = postProc, meta = meta)
	return(expMetaData)	
} # EOF

##################################################






getap <- function(fn="def") {
	autoUpS()
	
	
} #EOF

