checkForExperimentFolderStructure <- function(where=getwd(), stopIfBad = TRUE) {
	stn <- getstn()
	ad <- stn$fn_analysisData
	aa <- stn$fn_exports
	cc <- stn$fn_metadata
	aaa  <- stn$fn_rcode
	bb <- stn$fn_rawdata
	bbb  <- stn$fn_rdata
	ccc <- stn$fn_results
	dd  <- stn$fn_sampleList
	folderNames <- c(ad, aa, aaa, bb, bbb, cc, ccc, dd)
	#
	if(!all(folderNames %in% list.files(where)) ) {
		if (stopIfBad) {
			stop(paste0("Sorry, it appears the directory \n'", where, "' \nis not within the required standard folder structure.\nPlease consider using 'genFolderStr()' to create an appropriate folder structure in this directory."), call.=FALSE)
		} else {
			return(FALSE)
		} # end else
	} else { # so all is good
		return(TRUE)
	} # end else
} # EOF

################################################################
################################################################


#' @title Generate Folder Structure
#' @description Generate the required folder structure for a new experiment in 
#' the current working directory.
#' @details \code{genFolderStr} will generate all the required folders in the 
#' directory specified at \code{where} that 'aquap2' needs to work properly. 
#' Templates for metadata and analysis procedure will be copied into the 
#' metadata-folder.
#' @param where Character length one, the root folder of a single experiment.
#' Defaults to \code{getwd()}.
#' @return Used for its side effects, i.e. folders get created.
#' @family Helper Functions
#' @seealso \code{\link{settings_file}}, \code{\link{noise_procedures}}, 
#' \code{\link{genNoiseRecExp}} 
#' @export
genFolderStr <- function(where=getwd()) {
	stn <- autoUpS(cfs=FALSE) # no checking of folder structure here!
	fn_analysisData <- stn$fn_analysisData 
	fn_exports <- stn$fn_exports
	fn_rcode <- stn$fn_rcode 
	fn_rawdata <- stn$fn_rawdata
	fn_rdata <- stn$fn_rdata 
	fn_metadata <- stn$fn_metadata
	fn_results <- stn$fn_results 
	fn_sampleLists <- stn$fn_sampleLists
	fn_sampleListOut <- stn$fn_sampleListOut
	fn_sampleListIn <- stn$fn_sampleListIn
	pAdd <- ""
	#
	fn_mDataDefFile <- stn$fn_mDataDefFile
	fn_anProcDefFile <- stn$fn_anProcDefFile
	fn_slClassesFile <- stn$fn_class_structure
	#
	pp <- c(fn_analysisData, fn_exports, fn_rcode, fn_rawdata, fn_rdata, fn_metadata, fn_results, fn_sampleLists)
	pp <- paste0(where, "/", pp) # here all folder names get prepended with a full path (coming from where)
	dirOk <- NULL
	for (p in pp) {
		dirOk <- c(dirOk, dir.create(p, showWarnings=FALSE))
	}
	slin <- paste(where, fn_sampleLists, fn_sampleListIn, sep="/")
	slout <- paste(where, fn_sampleLists, fn_sampleListOut, sep="/")
	shW <- FALSE
	dirOk <- c(dirOk, dir.create(slin, showWarnings=shW))
	dirOk <- c(dirOk, dir.create(slout, showWarnings=shW))
	a <- path.package("aquap2")
	if (dir.exists(paste0(a, "/inst"))) { # needed for local tests
		pAdd <- "/inst"
	} # end if
	pathFrom <- paste(a, pAdd, "/templates/", sep="")
	# adapt to maybe locally different default names
	file.copy( paste(pathFrom, "metadata.r", sep=""), paste0(where, "/", fn_metadata, "/", fn_mDataDefFile), overwrite=FALSE) # they have the .R in the name
	file.copy( paste(pathFrom, "anproc.r", sep=""), paste0(where, "/", fn_metadata, "/", fn_anProcDefFile), overwrite=FALSE) # they have the .R in the name
	file.copy( paste(pathFrom, "sl_classes.xlsx", sep=""),  paste0(where, "/", fn_metadata, "/", fn_slClassesFile, ".xlsx"), overwrite=FALSE)
	if (all(dirOk)) {
		if (!stn$allSilent) {	cat("Folder structure created.\n")}
	} else {
		if (!stn$allSilent) {	cat("Some folders were created.\n")}
	} # end else
} # EOF

# only used when packing an example folder, it contains the instructions of which file to put where
# param ptFolder is the path to the folder that gets zipped later
# param dest A character vector with the destination for each element in the ptFolder
# returns a named list, with the names being the filenames in the ptFolder,
# and the values being the full paths (coming from ptFolder) to the destination folder
# possible values for destination are defined as globals
# Not required to be used by the end user, so no exporting this function
make_ap2dme_instructions <- function(ptFolder, dest = NULL) {
	rdsName <- gl_copyDestObj
	pvCdn <- pv_copyDestNames
	#
	allF <- sort(list.files(ptFolder))
	if (rdsName %in% allF) {
		allF <- allF[-(which(allF == rdsName))] # kick out the copyInstructions if here
	} # end if
	destList <- vector("list", length(allF))
	names(destList) <- allF
	if (is.null(dest)) {
		return(destList)
	} # end if
	if (length(destList) != length(dest)) {
		print(destList); print("---------");print(dest)
		stop("Length of provided destination does not match files", call.=FALSE)
	} # end if
	if (!all(dest %in% pvCdn)) {
		txt <- "Some of the provided destinations are non-standard"
		message(txt)
		cat(paste0("The standard destinations are:\n", paste(pvCdn, collapse="\n"), "\n") )
		stop(call.=FALSE)
	} # end if
	for (i in seq_along(destList)) {
		destList[[i]] <- dest[i]
	} # end for i
	# 
	# now destList is fine, save as RDS object in ptFolder
	saveRDS(destList, file=paste0(ptFolder, "/", rdsName))
	cat("Copy instructions saved.\n")
	return(invisible(destList))
} # EOF

#' @title Download and Make Example Experiment
#' @description Checks if a dataset is present. If not, it is downloading a 
#' single example experiment from a server, generating the required folder 
#' structure and inserting the downloaded files into their appropriate folder.
#' @details The experiment home folder, if already 
#' present, has to end in \code{@home} in order to be recognized correctly.
#' If the experiment home folder does not exist it will be created.
#' Currently, this function is primary intended to be used in testing 
#' and to help create executable examples.
#' The downloaded folder containing all available example datasets is stored in 
#' \code{tempdir()}. If already present, it will not be downloaded again.
#' @param where Character length one. The path where the experiment home folder
#' should be looked for. The experiment home folder, if already existing,  
#' has to end in \code{@home} in order to be recognized correctly.
#' @param expName Character length one, the name of the experiment.
#' @param ffs Logical. If the folder structure should be generated in all cases.
#' If set to \code{TRUE} and \code{where} is pointing to an already existing 
#' experiment home folder, the latter one will be deleted and a new folder 
#' structure will be created.
#' @param fdo Logical. If the download should always be enforced
#' @param sh Character length one. The path to settings home. Can and should 
#' be left at the default \code{NULL}.
#' @return Used for its side effect, i.e. do (possibly) download example data 
#' and to create a working experiment folder structure. 
#' Returns \code{NULL} if no data were downloaded, \code{FALSE} if data 
#' should have been downloaded but it failed, and \code{TRUE} if data were 
#' possibly downloaded, the experiment folder 
#' structure was created successfully, and the data files were copied.
#' @family Helper Functions
#' @export
ap2dme <- function(where, expName, ffs = FALSE, fdo = FALSE, sh=NULL) {
	stn <- getstn()
	#
#	gdExpRoot <- gl_LinkToExperiments # the server link to the complete rep aquap2_Data
	dataRepRoot <- gl_LinkToExperiments # the server link to the complete rep aquap2_Data
	expHomeSuffix <- gl_expHomeSuffix
	cpyObj <- gl_copyDestObj
	pvDest <- pv_copyDestNames
	td <- tempdir()
	settingsHome <- sh
	###
	fn_analysisData <- stn$fn_analysisData 
	fn_exports <- stn$fn_exports
	fn_rcode <- stn$fn_rcode 
	fn_rawdata <- stn$fn_rawdata
	fn_rdata <- stn$fn_rdata 
	fn_metadata <- stn$fn_metadata
	fn_results <- stn$fn_results 
	fn_sampleLists <- stn$fn_sampleLists
	fn_sampleListOut <- stn$fn_sampleListOut
	fn_sampleListIn <- stn$fn_sampleListIn
	fn_analysisData <- stn$fn_analysisData
	fn_exports <- stn$fn_exports
	###
	ap2dme <- FALSE
	remRepName <- "aquap2_Data-main"
	expFolder <- "experiments"
	#
	expHome <- paste0(expName, expHomeSuffix)
	if (!dir.exists(where)) {
		stop(paste0("Sorry, the folder '", where, "' \nthat should or will contain the experiment home folder \n'", expHome, "' does not seem to exist."), call.=FALSE)
	} # end if
	ptExpHome <- paste0(where, "/", expHome)
	if (dir.exists(ptExpHome) & ffs) {
		unlink(ptExpHome, recursive=TRUE)
	} # end if
	if (dir.exists(ptExpHome)) {
		fsok <- checkForExperimentFolderStructure(where = ptExpHome, stopIfBad = FALSE)
		if (!fsok) { # delete it all
			unlink(ptExpHome, recursive=TRUE)
		} else { # by now we have the correct folder structure in expHome,
			ap2dmeFile <- paste0(ptExpHome, "/ap2dme")
			if (file.exists(ap2dmeFile)) {
				ap2dme <- readRDS(ap2dmeFile)
			} # end if
			if (ap2dme) {
				return(NULL) # no data download, all is good			
			} else { # so we have an ap2dme, but is is FALSE
				unlink(ptExpHome, recursive=TRUE)
			} # end else
		} # end else
	} # end if
	#
	# if we are here ptExpHome does NOT exist (any more)
	ok <- dir.create(ptExpHome)
	if (!ok) {
		stop(paste0("Sorry, the folder '", expHome, "' could not be created in '", where, "' "), call.=FALSE)
	} # end if
	genFolderStr(ptExpHome) ######## here generate the folder structure #######
	#
	
	# now check if the main folder is already in tempdir: if yes, we do not have to download
	if (!dir.exists(paste0(td, "/", remRepName)) | fdo) {
		# now download data 
		repSource <- paste0(dataRepRoot, "") # already contains ".zip"
		targZip <- paste0(td, "/", remRepName, ".zip")	
		ok <- download.file(repSource, targZip, mode="wb") # is downloading the complete repository
		if (ok != 0) {
			message(paste0("Sorry, the download failed."))
			return(FALSE)
		} # end if ok != 0
		# unzip main folder
		aa <- try(utils::unzip(targZip, exdir = td)) # unzip the main folder
		if (is.null(aa)) {
			ap2dme <- FALSE
			saveRDS(ap2dme, paste0(ptExpHome, "/ap2dme"))
			return(FALSE)
		} # end if	
	} # end if not exist main data folder
	
	# now check if the single experiment is already unzipped 
	pathExper <- paste0(td, "/", remRepName, "/", expFolder, "/", expName)
    if (!dir.exists(pathExper)) {    
		# unzip single experiment
		targZip <- paste0(pathExper, ".zip")
		toDir <-  paste0(td, "/", remRepName, "/", expFolder)
		bb <- try(utils::unzip(targZip, exdir = toDir)) # now unzip the single experiment
		if (is.null(bb)) {
			ap2dme <- FALSE
			saveRDS(ap2dme, paste0(ptExpHome, "/ap2dme"))
			return(FALSE)   
		} # end if
    } # end if not is unzipped single experiment
	
	# read the copy instructions
	paCopyObj <- paste0(td, "/", remRepName, "/", expFolder, "/", expName, "/", cpyObj)
	if (!file.exists(paCopyObj)) {
		stop(paste0("Sorry, the object containing the required copy destinations for the experiment '", expName, "' \ndoes not seem to exist in \n'", pathExper, "' "), call.=FALSE)
	} # end if
	cpDest <- readRDS(file=paCopyObj)
	#
	# now insert the downloaded elements into the previously generated folder structure
	##
	mtfina <- function(folderName, tifona= tfn) {
		return(paste0(ptExpHome, "/", folderName, "/", tifona))
	} # EOIF
	##
	
	## organize settings home
	if (is.null(sh)) {
		shName <- aquap2_handover_to_uniset()$pkgUniset_RenvironSettingsHomeName
		settingsHome <- Sys.getenv(shName)
	} # end if
		
	## copy each file to the folder as instructed
	ok <- vector("list", length(cpDest))
	names(ok) <- names(cpDest)
	foFr <- paste0(td, "/", remRepName, "/", expFolder, "/", expName, "/") # folder from	
#	pvDest; c("root", "anData", "exports", "metadata", "R-code", "R-data", "rawdata", "results", "sl_in", "sl_out", "settingsHome")
	for (i in seq_along(cpDest)) {
		tfn <- names(cpDest)[i] # this file name
		fileFrom <- paste0(foFr, tfn) # fileFrom now contains the complete path to a file in the experiment folder
		toWhere <- cpDest[[i]]
		if (FALSE) {print("This file name: "); print(tfn); print("To Where: "); print(toWhere); print("---------")}
		if (toWhere == pvDest[1]) { # "root"
			fileTo <- paste0(ptExpHome, "/", tfn)
			ok[[i]] <- file.copy(fileFrom, fileTo, overwrite=TRUE)
		} else if (toWhere == pvDest[2]) { # "anData"
			fileTo <- mtfina(fn_analysisData)
			ok[[i]] <- file.copy(fileFrom, fileTo, overwrite=TRUE)
		} else if (toWhere == pvDest[3]) { # "exports"
			fileTo <- mtfina(fn_exports)
			ok[[i]] <- file.copy(fileFrom, fileTo, overwrite=TRUE)	
		} else if (toWhere == pvDest[4]) { # "metadata"
			fileTo <- mtfina(fn_metadata)
			ok[[i]] <- file.copy(fileFrom, fileTo, overwrite=TRUE)
		} else if (toWhere == pvDest[5]) { # "R-code"
			fileTo <- mtfina(fn_rcode)
			ok[[i]] <- file.copy(fileFrom, fileTo, overwrite=TRUE)
		} else if (toWhere == pvDest[6]) { # "R-data"
			fileTo <- mtfina(fn_rdata)
			ok[[i]] <- file.copy(fileFrom, fileTo, overwrite=TRUE)
		} else if (toWhere == pvDest[7]) { # "rawdata"
			fileTo <- mtfina(fn_rawdata)
			ok[[i]] <- file.copy(fileFrom, fileTo, overwrite=TRUE)
		} else if (toWhere == pvDest[8]) { # "results"
			fileTo <- mtfina(fn_results)
			ok[[i]] <- file.copy(fileFrom, fileTo, overwrite=TRUE)
		} else if (toWhere == pvDest[9]) { # "sl_in"
			fileTo <- mtfina(paste0(fn_sampleLists, "/", fn_sampleListIn))
			ok[[i]] <- file.copy(fileFrom, fileTo, overwrite=TRUE)
		} else if (toWhere == pvDest[10]) { # "sl_out"
			fileTo <- mtfina(paste0(fn_sampleLists, "/", fn_sampleListOut))
			ok[[i]] <- file.copy(fileFrom, fileTo, overwrite=TRUE)
		}  else if (toWhere == pvDest[11]) { # "settingsHome"
			fileTo <- settingsHome			
			ok[[i]] <- file.copy(fileFrom, fileTo, overwrite=TRUE)
		} # end else if
	} # end for i going through the elements of cpDest
	####
	if (all(unlist(ok))) {
		ap2dme <- TRUE
	} else {
		ap2dme <- FALSE
	} # end else
	saveRDS(ap2dme, paste0(ptExpHome, "/ap2dme"))
	return(ap2dme)
} # EOF

################################################################
################################################################

getStdColnames <- function() {
	stn <- getstn()
	yPref <- stn$p_yVarPref
	cPref <- stn$p_ClassVarPref
	sampleNrColn <- stn$p_sampleNrCol
	conSNrColn <- stn$p_conSNrCol
	timePointsColn <- stn$p_timeCol
	ecrmColn <- stn$p_ECRMCol
	replColn <- stn$p_replicateCol
	groupColn <- stn$p_groupCol
	tempColn <- stn$p_tempCol
	relHumColn <- stn$p_RHCol
	stdColsY <- c(paste(yPref, sampleNrColn, sep=""), paste(yPref, conSNrColn, sep=""), paste(yPref, tempColn, sep=""), paste(yPref, relHumColn, sep=""))
	stdColsC <- c( paste(cPref, timePointsColn, sep=""), paste(cPref, ecrmColn, sep=""), paste(cPref, replColn, sep=""), paste(cPref, groupColn, sep=""))
	return(list(stdColsY=stdColsY, stdColsC=stdColsC))
} # EOF

#' @title Print standard column names
#' @description Prints the standard column names as defined in the local 
#' settings.r file to stdout.
#' @family Helper Functions
#' @export
printStdColnames <- function() {
	autoUpS()
	cns <- getStdColnames()
	stdColsC <- cns$stdColsC
	stdColsY <- cns$stdColsY
	cat("The standard column names as defined in your settings.r file are: \n\n")
	cat("Class variables:\n")
	cat(paste(stdColsC, collapse=", ")); cat("\n\n")
	cat("Numeric variables:\n")
	cat(paste(stdColsY, collapse=", ")); cat("\n")
} # EOF


#' @title Select Observations
#' @description Create includes or excludes from the dataset by selecting 
#' from any variable in any logical combination, using the available logical 
#' operators like e.g. '|' and '&'.
#' @details The column names are provided as is, i.e. without quotes, while for 
#' characters the values have to be enclosed in quotation marks - see examples.
#' @param dataset An object of class 'aquap_data'
#' @param criteria The selection criteria in the format 
#' \code{variableName == value}, possibly joined by logical operators.
#' @param include Logical. If the observations matching the criteria should be 
#' included or excluded from the dataset.
#' @param keepEC If *all* the environmental control observations should be kept 
#' in the dataset. Only evaluated if 'include' is TRUE.
#' @return The standard dataset as described in \code{\link{getFullData}}
#' @examples
#'  \dontrun{
#'  ds <- ssc(dataset, C_Group=="Control")
#'  # keeps all the controls
#'  ds <- ssc(dataset, C_Group!="Control", include=FALSE)
#'  # the same as above
#'  
#'  
#'  ds <- ssc(dataset, C_Group=="Control" & C_Repl=="R1")
#'  # keeps only the first replicate of the controls
#'  ds <- ssc(dataset, C_Group=="Control" | C_Repl=="R1")
#'  # keeps all the first replicate and all the controls
#'  
#'  
#'  ds <- ssc(dataset, C_Group=="Control" & C_Repl=="R1", keepEC=TRUE)
#'  # keeps the first replicate of the controls and all the environmental controls
#'  ds <- ssc(dataset, C_Group=="Control" & C_Repl=="R1", include=FALSE)
#'  # keeps everything except the first replicate of the controls
#'  
#'  
#'  ds <- ssc(dataset, (C_Group=="Control" | C_Group=="Treatment") & Y_conSNr==1)
#'  # keeps the first consec. scan of the controls and the treatment group.
#'  ds <- ssc(dataset, (C_Group=="Control" | C_Group=="MQ") & C_conSNr=="1")
#'  # keeps the first consec. scan of the controls and the environmental controls
#'  
#'  
#'  ds <- ssc(dataset, Y_Temp==22.5)
#'  ds <- ssc(dataset, Y_Temp==22.5 & Y_conSNr==1)
#'  ds <- ssc(dataset, Y_conSNr==1) 
#'  # keeps only the first consecutive scan
#'  }
#' @family Data pre-treatment functions
#' @export
ssc <- function(dataset, criteria, include=TRUE, keepEC=FALSE) {
	stn <- autoUpS()
	cPref <- stn$p_ClassVarPref
	ecrmCol <- stn$p_ECRMCol
	ecLabel <- getMetadata(dataset)$postProc$ECRMLabel[1]
	string <- deparse(substitute(criteria))
	cns <- colnames(dataset$header)
#	aa <- lapply(cns, function(x) grep(x, string, fixed=TRUE))  	# core of the problem	
#	print(string); str(aa);
	cnsPres <- cns[which(lapply(cns, function(x) grep(pattern=x, x=string, fixed=TRUE)) > 0)] # gives back only those column names that appear in the string # XXX Problem !!
	# XXX problem here, when kind of ambiguous column names, e.g. one contained completely in the other	
	stri <- string
	for (i in 1: length(cnsPres)) {
		stri <- gsub(cnsPres[i], paste("dataset$header$", cnsPres[i], sep=""), stri, fixed=TRUE)
	} # end for i
	if (include) {
		if (keepEC) {
			stri <- paste("(", stri, ") |  dataset$header$", cPref, ecrmCol, " == \"", ecLabel, "\"", sep="")
		}
		d <- dataset[which(eval(parse(text=stri))),]
	} else {
		d <- dataset[-(which(eval(parse(text=stri)))),]
	}
	if (nrow(d) == 0) {
		stop(paste("Your selection criteria yielded no results. Please check your input."), call.=FALSE)
	}
#	return(new("aquap_data", reFactor(d)))
	return(d)
} # EOF

# to be called from the system
ssc_s <- function(dataset, variable, value, keepEC=TRUE) {
	stn <- getstn()
	# variable and value are always data frames with one row and 1 or *more* columns
	cPref <- stn$p_ClassVarPref
	ecrmCol <- stn$p_ECRMCol
	ecLabel <- getMetadata(dataset)$postProc$ECRMLabel[1]
	noSplitCol <- paste(cPref, stn$p_commonNoSplitCol, sep="")
	indEC <- which(colnames(dataset$header) == paste(cPref, ecrmCol, sep=""))
	selIndOut <-  NULL
	#
#	getECInd <- function(variable) { # because we must not add the ecs if they are already present in the case of the no-split column
#		nsc <- any(noSplitCol %in% variable)
#		if (keepEC & !nsc) {
#			return(which(dataset$header[,indEC] == ecLabel))
#		} else {
#			return(NULL)
#		}
#	} # EOIF
	###
	if (is(variable, "data.frame")) {
		for (i in 1: ncol(variable)) { # both variable and value have the same number of columns
			ind <- which(colnames(dataset$header) == variable[1,i])
			val <- as.character(value[1,i])
			selInd <-  which(dataset$header[,ind] == val)
			if (length(selInd) == 0) {
				return(NULL)	
			}
			dataset <- dataset[selInd] # !!! gives back the dataset in the loop, i.e. an logical "&" !!!
#			selIndOut <- c(selInd, selIndOut)
		} # end for i
	} else {
		ind <- which(colnames(dataset$header) == variable)
		val <- as.character(value)
		selInd <-  which(dataset$header[,ind] == val)
		if (length(selInd) == 0) {
			return(NULL)	
		}
		dataset <- dataset[selInd]
	}
	# now the dataset still has possibly the environmental controls in it
	if (!keepEC) {
		ind <- which(colnames(dataset$header) == paste(cPref, ecrmCol, sep="")) # where is the EC-column
		ECInds <- which(dataset$header[,ind] == ecLabel)
		if (length(ECInds) != 0) {
			dataset <- dataset[-(ECInds)]			
		}
	}
	return(dataset) # re-factoring is already included in the "[" operation
} # EOF

reFactor <- function(dataset) {
	for (i in 1: ncol(dataset$header)) {
		if (is.factor(dataset$header[,i])) {
			dataset$header[i] <- factor(dataset$header[,i])
		}
	}
	return(dataset)
} # EOF

manualDatasetSubscripting <- function(dataset, inds) { # is NOT returning a full dataset!!, is used in the windows-parallel bootstrap of the aquagram, because in "snow" the methods are available
	x <- dataset
	i <- inds
	#
	drop=FALSE
	header <- x$header[i, , drop=drop]
	colRep <- x$colRep[i, , drop=drop]
	if (!is.null(x$NIR)) {
		NIR <- x$NIR[i, , drop=drop]
		rownames(NIR) <- rownames(x$NIR)[i]
		colnames(NIR) <- colnames(x$NIR)
		fd <- reFactor(data.frame(I(header), I(colRep), I(NIR)))
	} else {
		fd <- reFactor(data.frame(I(header), I(colRep)))				
	}
	return(fd)
#	dataset@.Data <- fd # does not work on windows-parallel, as the class-definition is "invoked" when calling the "@.Data", and this class definition is not copied to the R-worker processes
#	return(dataset)
#	return(new("aquap_data", fd, ncpwl=x@ncpwl, metadata=x@metadata, anproc=x@anproc, version=x@version))
} # EOF

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
	 return(abs(x - round(x)) < tol)
} # EOF

makePchSingle<- function(PchToReFact, extra = FALSE) {  #PchToReFact: the factor what you want to display with different pch, extra: additional not nice pch
	nr <- length(unique(PchToReFact))
 	if (extra) {
 		nicePch<-c(0:20,35,127,134,135,164,169,171,174,182,187)
 	} else {
 		nicePch<-c(0:20)
 	}
 	if (nr > length(nicePch)){
   		nicePch <- rep(nicePch,ceiling(nr/length(nicePch)))
 	}
 	return(nicePch[PchToReFact])
} # EOF

makePchGroup <- function(PchToReFact, extra = FALSE) {
	nr <- length(unique(PchToReFact))
 	if (extra) {
 		nicePch<-c(0:20,35,127,134,135,164,169,171,174,182,187)
 	} else {
 		nicePch<-c(0:20)
 	}
 	if (nr > length(nicePch)){
   		nicePch <- rep(nicePch,ceiling(nr/length(nicePch)))
 	}
 	return(nicePch[unique(PchToReFact)])
} # EOF

getUniqLevelColor <- function(nrc) {
	if (all(is.numeric(nrc))) {
		return(as.numeric(levels(as.factor(nrc))))
	}
	if (all(is.character(nrc))) {
		return(levels(as.factor(nrc)))
	}
} # EOF

# color_data, color_unique, color_legend, txt, txtE, sumPart, dataGrouping, pch_data, pch_legend
extractColorLegendValues <- function(dataset, groupBy, minPart=NULL, minDistinctVals=NULL, ltyIn=NULL) { # returns a list
	stn <- getstn()
	legColLim <- stn$plt_lengthLegend_limToCols	# the number when we switch to columns
	maxLengLeg <- stn$plt_lengthLegend_truncate	# the maximum legend length
	legNrCols_more <- stn$plt_legendMoreCols			# the number of columns in the legend if more than 1
	legCex <- stn$plt_legend_standardCex			# the standard cex for the legend
	legCexDim <- stn$plt_legend_smallerCex			# the legend cex if smaller
	#
	legNrCols <- 1
	lty <- outIndsLeg <- NULL
	#
	colInd <- which(colnames(dataset$colRep) == groupBy)
	color_data <- dataset$colRep[, colInd]
	ind <- which(colnames(dataset$header) == groupBy)
	grouping <- dataset$header[, ind]
	legendText <- as.character(levels(grouping))
	options(warn=-1)
	nrs <- as.numeric(legendText)
	options(warn=0)
	if (any(is.na(nrs))) {
		lto <- order(legendText) # should be straight from 1 to n, because "level" already gives out characters in alphabetical order
	} else {
		lto <- order(nrs) # so if the legend text is coming from all numbers *and* they are higher than 9 we get the real order to sort later
	}		
	partN <- sapply(levels(grouping), function(x, grAll) length(which(grAll==x)), grAll=grouping)
	
	if (is(partN, "list") & length(partN) == 0) {
		stop(paste0("Sorry, a problem occurred when trying to count the levels in the class-variable '", groupBy, "'. \nProbably there are no data in this class-variable. A solution could be to remove '", groupBy, "' from your analysis / your input."), call.=FALSE)
	}
	sumPart <- sum(partN)
	legendText <- legendText[lto]
	legendTextExtended <- paste(legendText, "   N=", partN[lto], "", sep="") # have it in every line			
	color_unique <- getUniqLevelColor(color_data)  # here read out in levels !!!
#	color_legend <- color_unique[lto] # the old version, appears to be not always correct
	ind <- which(colnames(dataset$colRep) == groupBy) # get once the index of our grouping variable in the colRep
	color_legend <- sapply(legendText, function(x, cri, ds, grby) ssc_s(ds, grby, x)$colRep[1,cri], cri=ind, ds=dataset, grby=groupBy) # look through each of the elements of the legend text and extract the corresponding color from the colRep
	pch_data <- makePchSingle(grouping)
	pch_legend <- as.numeric(levels(as.factor(pch_data)))[lto]
	###
	####### have minimum participants
	if (!is.null(minPart)) {
		aa <- rle(sort(as.character(grouping)))
		ind <- which(aa$lengths < minPart)
		outIndsCol <- outIndLegCol <- NULL
		if (length(ind) > 0 ) {
			for (i in 1: length(ind)) { # because we could have more!!!!
				values <- aa$values[ind[i]]
		#		cat(paste0("kicked out: ", values, "\n"))
				outInds <- sapply(values, function(x) which(grouping == x))
				outIndsLeg <- sapply(values, function(x) which(legendText == x))
				outIndsCol <- c(outIndsCol, outInds)
				outIndLegCol <- c(outIndLegCol, outIndsLeg)
			} # end for i
			outInds <- outIndsCol
			outIndsLeg <- outIndLegCol
			grouping[outInds] <- NA
			grouping <- as.factor(as.character(grouping))
			color_data[outInds] <- NA
			pch_data[outInds] <- NA
			legendText[outIndsLeg] <- NA
			legendTextExtended[outIndsLeg] <- NA
			color_legend[outIndsLeg] <- NA
			pch_legend[outIndsLeg] <- NA
		}
		if (!is.null(ltyIn)) {
			lty <- rep(ltyIn, length(legendText))
			lty <- lty[1: length(legendText)]
			lty[outIndsLeg] <- NA
		}
	} # end !is.null(minPart)
	####
	##### cutting down (truncating) the legend
	le <- length(legendText)
	if (le > legColLim & le < maxLengLeg) {
		legCex <- legCexDim
		legNrCols <- legNrCols_more
	}
	if (le >= maxLengLeg) { # in this case the standared legCex and standard legNrCols stays in place
		half <- round(le/2, 0)
		inds <- c(1, 2, 3, 4, half-1, half, half+1, le-3, le-2, le-1, le) # gets the first, the middle and the last index of things
		color_legend <- color_legend[inds]
		aa <- legendText
		legendText <- c(aa[c(1,2,3)], "...", aa[c(half-1, half, half+1)], "...", aa[c(le-2, le-1, le)])
		aa <- legendTextExtended
		legendTextExtended <- c(aa[c(1,2,3)], "...", aa[c(half-1, half, half+1)], "...", aa[c(le-2, le-1, le)])
	}	
	####
	return(list(color_data=color_data,  color_unique=color_unique, color_legend=color_legend, txt=legendText, txtE=legendTextExtended, sumPart=sumPart, dataGrouping=grouping, pch_data=pch_data, pch_legend=pch_legend, legCex=legCex, legNrCols=legNrCols, ltyNA=lty))
} # EOF

countDecimals <- function(x, nrDec=25) {
	if (!all(is.numeric(x))) {stop()} 
	xRounded <- lapply(x, function(g) round(g, 0:nrDec))
	res <- mapply(function(xR,x) match(TRUE, xR==x), xRounded, x)
	res <- res -1 # to account for the first element what has zero commas
	res[is.na(res)] <- nrDec # as a precaution
	return(res)
} # EOF

readInSpecAreas <- function() {
	stn <- getstn()
	out <- as.data.frame(t(getOvertoneWls(getstn()$aqg_OT, stn)))  # getOvertoneWls() is in the file "calc_aqg.r"
return(out)
} # EOF

makeFlatDataFrame <- function(dataset, groupBy, fusionGroupBy=NULL) {
	if (is.null(fusionGroupBy)) {
		colInd <- which(colnames(dataset$header) == groupBy)
		grouping <- dataset$header[, colInd]
		class(grouping) <- "factor" # to get rid of the "AsIs" that, strangely, got smuggled in..
	} else {
		grouping <- fusionGroupBy
	}
	NIR <- as.data.frame(matrix(dataset$NIR, nrow=(nrow(dataset$NIR))))
	out <- cbind(grouping, NIR)
	colnames(out) <- c("grouping", colnames(dataset$NIR))
	rownames(out) <- rownames(dataset)
	return(out)
} # EOF

makeDataFrameForClassification <- function(dataset, groupBy, fusionGroupBy=NULL) { # ! is not flat !
	if (is.null(fusionGroupBy)) {
		colInd <- which(colnames(dataset$header) == groupBy)
		grouping <- dataset$header[, colInd]
		class(grouping) <- "factor" # to get rid of the "AsIs" that, strangely, got smuggled in..
	} else {
		grouping <- fusionGroupBy
	}
	NIR <- matrix(dataset$NIR, nrow=(nrow(dataset$NIR)))
	rownames(NIR) <- rownames(dataset$NIR)
	colnames(NIR) <- colnames(dataset$NIR)
	out <- data.frame(I(grouping), I(NIR))
	rownames(out) <- rownames(dataset$header)
	return(out)
} # EOF

makeFlatDataFrameMulti <- function(dataset, groupBy) {
	colInd <- which(colnames(dataset$header) %in% groupBy)
	grouping <- dataset$header[, colInd, drop=FALSE]
	class(grouping) <- "data.frame" # to get rid of the "AsIs" that, strangely, got smuggled in..
	NIR <- as.data.frame(matrix(dataset$NIR, nrow=(nrow(dataset$NIR))))
	out <- cbind(grouping, NIR)
	colnames(out) <- c(groupBy, colnames(dataset$NIR))
	rownames(out) <- rownames(dataset)
	return(out)
} # EOF

#' @title Get a single dataset from the 'cube'
#' @description Get a single dataset, referenced by its index, from the 'cube' 
#' object, i.e. the object generated by \code{\link{gdmm}}.
#' @details A valid index from the available range (see the row-names of the 
#' 'cube-object') has to be provided.
#' @param cube An object as created by \code{\link{gdmm}}.
#' @param index The index of the dataset to be obtained. See the leftmost
#' rowname of the 'cube' object.
#' @return A standard dataset as e.g. produced by the function \code{\link{gfd}}.
#' @examples 
#' \dontrun{
#' dataset <- gfd()
#' cube <- gdmm(dataset)
#' # assumes that in the analysis procedure we have a split variable defined.
#' dataset_3 <- getcd(cube, 3)
#' dataset_3
#' str(dataset_3)
#' }
#' @seealso \code{\link{do_msc}}, \code{\link{do_avg}}
#' @family Helper Functions
#' @family Extract Elements
#' @export
getcd <- function(cube, index) {
	return(invisible(cube[[index]]@dataset))
} # EOF

getbr <- function(cube, index) {
#	return(invisible(cube[[index]]))
} # EOF

#' @title Get a single model from the 'cube'
#' @description Get a single model, referenced by its index and specified by the
#' argument \code{what} from the 'cube' object, i.e. the object generated by 
#'  \code{\link{gdmm}}.
#' @inheritParams getcd
#' @template mr_getCubeModel
#' @param what Character length one, see details.
#' @section Extracting Vectors:
#' To extract e.g. two loading vectors from a pca-model, you could use a code like 
#' \code{loadingVectors <- pcaModel$loadings$[, c(1,2)]} to extract the first two 
#' loading vectors from the model - see examples.
#' @examples 
#' \dontrun{
#' fd <- gfd()
#' cube <- gdmm(fd)
#' # assumes that in the analysis procedure we have a split variable defined.
#' fd_3_pca <- getcm(cube, 3)
#' str(fd_3_pca)
#' ld12 <- fd_3_pca$model$loadings[, c(1,2)] # extract the first two loadings
#' ld24 <- fd_3_pca$model$loadings[, c(2,4)] # extract loadings 2 and 4
#' fd_2_pls <- getcm(cube, 2, "pls")
#' str(fd_2_pls)
#' }
#' @seealso \code{\link{do_emsc}}, \code{\link{dpt_modules}}
#' @family Helper Functions
#' @family Extract Elements
#' @export
getcm <- function(cube, index, what="pca") {
#	pv_what_models <- c("pca", "sim", "pls")
	pvWhat <- 	pv_what_models
	if (!all(class(what) == "character") | length(what) !=1) {
		stop(paste("Please provide a character of length one to the argument 'what'.\nPossible values are ", paste(pvWhat, collapse=", "), "\n", sep=""), call.=FALSE)
	}
	if (!what %in% pvWhat) {
		stop(paste("Please provide one of ", paste(pvWhat, collapse=", "), " to the argument 'what'.", sep=""), call.=FALSE)
	}
	if (!.hasSlot(cube[[index]], what)) {
		stop(paste("Sorry, the selected ", what, " model is not available.", sep=""), call.=FALSE)
	}
	out <- slot(cube[[index]], what)
	return(out)
} # EOF

# used in showCube
getCubeNrs <- function(cube) {
	nrRows <- nrWls <- NULL
	 for (i in 1: length(cube)) {
	 	ds <- getcd(cube, i)
	 	a <- nrow(ds)
	 	b <- ncol(ds$NIR)
	 	nrRows <- c(nrRows, a)
		nrWls <- c(nrWls, b)
	 }
	return(list(nrRows=nrRows, nrWls=nrWls))
} # EOF

adaptIdStringForDpt <- function(dptSource, prevIdString="") { # returns the new idString; dptSource is an analysis procedure
	stn <- getstn()
	limit <- stn$gen_plot_maxNrDptInfoOnMain
	##
	origAp <- dptSource #  we should get in the ap containing all the dpt information
	combChar <- msg <- char <- ""
	if (!is.null(origAp)) {
		apo <- origAp$dpt$dptModules
		dptPre <- apo$dptPre
		dptPost <- apo$dptPost
		if (!is.null(dptPost)) {
			dptPost[1] <- paste(";", dptPost[1], sep="") ## add an "|" in front of the first element in dptPost
		}
		combSingle <- c(dptPre, dptPost)
		if (length(combSingle) > limit) {
			restNr <- length(combSingle) - limit
			combSingle <- c(combSingle[1:limit], paste("+", restNr, sep=""))
		}
		combChar <- paste(combSingle, collapse=",")
	} # end if !is.null(origApp)
	if (combChar != "") {
		msg <- "  dpt:"
		char <- combChar
	}
	idStrAdd <- paste(msg, char, collapse="", sep="")
	idStrAdd <- gsub(",;", ";", idStrAdd) # some mistake above, take it out
	idStrNew <- paste(prevIdString, idStrAdd, collapse="", sep="")
	return(idStrNew)
} # EOF

getCheckLegendPosition <- function(xData, yData) {
	stn <- getstn()
	defPos <- stn$gen_plot_legendPosition
	pvPos <- pv_legendPosition # ("auto", "topleft", "topright", "bottomright", "bottomleft")
	if (!all(is.character(defPos)) | length(defPos) !=1) {
		stop(paste("Please provide a character length one for the default legend position.\n Possible values are ", paste(pvPos, collapse=", "), ".", sep=""), call.=FALSE)
	}
	if (!defPos %in% pvPos) {
		stop(paste("The legend-position '", defPos, "' can not be recognized. \nPlease provide one of '", paste(pvPos, collapse=", "), "' for the default legend position.", sep=""), call.=FALSE)
	}
	if (defPos == pvPos[1]) { # "auto"
		es <- list()
	#	es <- plotrix::emptyspace(xData, yData) # gives back a list with x and y value of the center of the "biggest empty rectangle"
		mer <- plotrix::maxEmptyRect(range(xData), range(yData), xData, yData)
		es$x <- mean(c(mer$rect[1], mer$rect[3]))
		es$y <- mean(c(mer$rect[2], mer$rect[4]))
		if (es$x >= mean(range(xData)) ) {
			xChar <- "right"
		} else {
			xChar <- "left"
		}
		if (es$y >= mean(range(yData)) ) {
			yChar <- "top"
		} else {
			yChar <- "bottom"
		}
		return(paste(yChar, xChar, sep=""))
	} else {
		return(defPos)
	}
} # EOF

checkApsChar <- function(aps) {
	stn <- getstn()
	path <- stn$fn_metadata
	if (all(aps == "def")) {
		aps <- stn$gen_plot_anprocSource
	}
	if (!all(is.character(aps)) | length(aps) != 1) {
		stop("Please provide a length one character to the argument 'aps' resp. the corresponding variable (gen_plot_anprocSource) in 'settings.r', thank you.", call.=FALSE)
	}
	if (aps == "cube") {
		return(aps)
	}
	if (aps == "defFile") {
		fn <- stn$fn_anProcDefFile
		ok <- file.exists(paste(path, fn, sep="/"))
		if (!ok) {
			stop(paste("The analysis procedure file \"", fn, "\" does not seem to exist. Please check your input.", sep=""), call.=FALSE)
		}
		return(fn)
	}
	return(aps) # so the only left option is a custom filename, that will be checked later	
} # EOF

#' @title Isolate single wavelength
#' @description Generate a dataset with a single wavelength.
#' @details Provide the wavelength that should remain in the dataset in the 
#' argument \code{wl}. It is not ncecessary to exactly know the desired 
#' wavelength -- if there is no direct match with the wavelength, the next best
#' hit will be taken.
#' @param dataset An object of class 'aquap_data' as produced e.g. by 
#' \code{\link{gfd}}.
#' @param wl Numeric length one. The
#' @param getMax Logical. Set to 'TRUE' to isolate the wavelength with the 
#' highest sum of absorbtion values.
#' @seealso \code{\link{aquap_data-methods}}, \code{\link{selectWls}}
#' @examples
#' \dontrun{
#' fd <- gfd()
#' dataset_single <- siWl(fd, 1300) # to only leave wavelength 1300
#' dataset_single2 <- siWl(fd, getMax=TRUE) # to look for the wavelength with 
#' # highest sum of absorbtion values
#' }
#' @family Extract Elements
#' @export
siWl <- function(dataset, wl, getMax=FALSE) {
	wls <- getWavelengths(dataset)
	if (getMax) {
		ind <- which.max(colSums(dataset$NIR))
	} else {
		ind <- match(wl, wls)		
	}
	if (is.na(ind)) { # so we do not have an exact match
		a <- which(wls > wl)[1]
		wlsBr <- wls[c(a-1, a)] # the two bordering values in the wavelengths
		hit <- wlsBr[ which.min(abs(wlsBr - wl)) ] # get the closer one
		ind <- match(hit, wls)
	}
	cns <- colnames(dataset$NIR)[ind]
	rns <- rownames(dataset$NIR)
	NIR <- dataset$NIR[,ind, drop=FALSE]
	colnames(NIR) <- cns
	rownames(NIR) <- rns
	dataset$NIR <- I(NIR)
	return(dataset)
} # EOF

#' @title Reduce Number of Wavelengths
#' @description Reduces the number of wavelengths in a provided dataset, either 
#' by simply keeping only the wavelengths of the 12 or 15 water matrix coordinates 
#' in the first overtone (1300nm - 1600nm), or by providing a user-defined list 
#' containing the wavelengths to be kept.
#' @param dataset The standard dataset as produced by \code{\link{gfd}}.
#' @param wlg List or character. Provide an integer matrix with two columns and n 
#' rows to keep the ranges of wavelengths defined in each row - see examples. 
#' Provide either \code{ot1.12} or \code{ot1.15} to only keep the respective 
#' wavelengths of the 12 or 15 water matrix coordinates in the first overtone. 
#' (The definition of the wavelengths is in root of the package aquap2.)
#' @param avg Logical If, for further reduction of wavelengths, the values of the 
#' wavelengths in each group (as defined in each row of the matrix) should be 
#' averaged.
#' @seealso \code{\link{selectWls}}, \code{\link{siWl}} 
#' @return The transformed dataset.
#' @examples
#' \dontrun{
#' fd <- gfd()
#' m <- matrix(c(300, 320, 400, 450, 530, 570), ncol=2, byrow=TRUE)
#' fdc <- siWlg(fd, wlg=m) 
#' fdc <- siWlg(fd) # using all the 12 wavelength ranges in the first overtone
#' fdc <- siWlg(fd, TRUE) # averaging within the 12 ranges, resulting in only 12
#' # wavelengths in the dataset
#' }
#' @family Classification Helpers
#' @family Extract Elements
#' @family Helper Functions
#' @export
siWlg <- function(dataset, avg=FALSE, wlg="ot1.12") {
	errMsg <- c("Please provide either a matrix with 2 columns and n rows, or a character length one (`ot1.12` or `ot1.15`) to the argument `wlg`.")
	useWls <- wlg
	if (any(is.character(wlg))) {
		if (!all(is.character(wlg)) | length(wlg) != 1) {
			stop(paste0(errMsg, "\nThanks for your consideration, and have a good day."), call.=FALSE)
		} # end if
		aqs <- readInAquagramPSettings()
		if (wlg == "ot1.12") {
			useWls <- aqs$ot1$wls$wls12
		} else {
			if (wlg == "ot1.15") {
				useWls <- aqs$ot1$wls$wls15
			} else {
				stop(errMsg, call.=FALSE)
			}
		}
	} # end if any is character
	# now we should have only numbers as input
	if (ncol(useWls) != 2 | !all(is.numeric(useWls)) ) {stop(errMsg, call.=FALSE)}
	# now everything should be good
	wls <- getWavelengths(dataset)
	inds <- apply(useWls, 1, function(x, wls) which(wls >= x[1] & wls <= x[2]), wls=wls) # is a list !
	if (!avg) {
		inds <- unlist(inds)
		cns <- colnames(dataset$NIR)[inds]
		rns <- rownames(dataset$NIR)
		NIR <- dataset$NIR[,inds, drop=FALSE]
		colnames(NIR) <- cns
		rownames(NIR) <- rns
		dataset$NIR <- I(NIR)
		return(dataset)
	} else { # so we want to average within each group
		NIR <- dataset$NIR
		rns <- rownames(NIR)
		outMat <- matrix(NA, nrow=nrow(NIR), ncol=length(inds))
		for (i in 1: nrow(NIR)) {
			obsAvg <- vector("numeric", length(inds))
			for (k in 1: length(inds)) {
				obsAvg[k] <- mean(NIR[i,inds[[k]]]) # averaging all the absorbance values within the single elements defined by inds
			} # end for k
			outMat[i,] <- obsAvg
		} # end for i
		cpwl <- substr(colnames(NIR)[1], dataset@ncpwl, dataset@ncpwl)
		wlsAvg <- round(unlist(lapply(lapply(inds, function(x, wls) wls[x], wls=wls) , mean)),0) # averages the wavelength names (number), for a central wavelength for the colnames
		rownames(outMat) <- rns
		colnames(outMat) <- paste0(cpwl, wlsAvg)
		NIR <- outMat
		dataset$NIR <- I(NIR)
		return(dataset)
	}
} # EOF

setCheck_NumericLengthOne <- function(num, char) {
	if (!all(is.numeric(num)) | length(num) != 1) {
		stop(paste0("Please provide a numeric length one to the argument '", char, "' in the settings.r file."), call.=FALSE)
	}
}

doApsTrick <- function(aps, cube, ...) {
	aps <- checkApsChar(aps)
	if (aps == "cube") {
		ap <- getap(.lafw_fromWhere="cube", cube=cube, ...)			 # the ... are here used for additionally modifying (if matching arguments) the analysis procedure obtained from the cube
	} else {
	#	if (aps == "cube") {aps <- "defFile"}
		check_apDefaults(fn=aps)
		ap <- getap(fn=aps, ...) # load from file, possibly modify via ...
	}
	return(ap)
} # EOF

makeColorsTransparent <- function(colVec, alpha=100) {
	mat <- col2rgb(colVec, alpha=TRUE)
	mat[4,] <- alpha
	colVec <- apply(mat, 2, function(x) rgb(x[1], x[2], x[3], x[4], maxColorValue=255))
	return(colVec)
} # EOF

exportAdditionalModelToAp2GD <- function(doExport, thisMod, thisType) {
#	print("----------"); print(str(thisMod)); print(thisType); print(doExport); wait()
	if (doExport) {
		# the initial list is initialized in processSingleRow_CPT
		if (is.null(thisMod)) {
			thisMod <- list(NULL)
		}
		modColName <- pv_extraMods # global variable
		existing <- get(modColName, pos=gl_ap2GD)
		typeCol <- c(existing$type, thisType)
		modCol <- c(existing$mod, list(thisMod)) # add the model to the list
		out <- list(type=typeCol, mod=modCol)
 		assign(modColName, out, pos=gl_ap2GD) # if from gdmm: we do that in each set
	} # end if do export
	return(invisible(NULL))
} # EOF

#' @title Re-Color a dataset
#' @description Recalculate all the colors contained in \code{colRep} in a 
#' dataset.
#' @details This can be useful if, for example, a subset of data taken from 
#' a rather big dataset where a possible color-gradient was very wide, only 
#' presents itself as having more or less the same color.
#' @param dataset An object of class 'aquap_data' as produced e.g. by 
#' \code{\link{gfd}}.
#' @return The dataset with recalculated color representation.
#' @examples
#' \dontrun{
#' fd <- gfd()
#' fd2 <- reColor(fd)
#' }
#' @family Helper Functions
#' @export
reColor <- function(dataset) {
	colRep <- extractClassesForColRep(dataset$header)
	dataset$colRep <- I(colRep)
	rownames(dataset$colRep) <- rownames(dataset$header)
	return(dataset)
} # EOF
