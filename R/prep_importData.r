# read spectra ----------------------------------------------------------------------
readSpec_checkDefaults <- function(possibleFiletypes, md, filetype, naString, sh) {
	stn <- getstn()
	if (all(filetype == "def")) {
		filetype <- stn$imp_specFileType
		# If a value other than "def" is provided in the argument "filetype" in "getFullData", this is overriding the value of "filetype" in the metadata file.
		if (md$meta$filetype != filetype) { # we only look at the value for "filetype" in the metadata if filetype in getFullData is "def"
			filetype <- md$meta$filetype
		}
	}
	if (!all(is.character(filetype)) | length(filetype) != 1) {
		stop("Please provide a character length one to the argument 'filetype'. Refer to the help for 'getFullData' for possible values.", call.=FALSE)
	}
	if (grepl("custom@", filetype)) {
		custName <- strsplit(filetype, "custom@")[[1]][2]
		if (is.null(sh)) { # the normal case
			shName <- aquap2_handover_to_uniset()$pkgUniset_RenvironSettingsHomeName
			pathSH <- Sys.getenv(shName)
		} else { # so incoming sh is not null, we are in a test scenario
			pathSH <- sh
		} # end else
		if (!file.exists(paste(pathSH, custName, sep="/"))) {
 			stop(paste("The file \"", custName, "\" that contains the custom-function for importing of raw-spectra does not seem to exist in \n\"", pathSH, "\".\n", sep=""), call.=FALSE)
		} else {
			fc <- filetype
		}
	} else {
		fc <- NULL
	}
	possValues <- c(possibleFiletypes, fc) # so we are sure that we have a valid filename under fc 
	if (!any(possValues %in% filetype)) {
		stop("Please refer to the help for 'getFullData' for possible values for the argument 'filetype'. Have a good day.", call.=FALSE)
	}
	assign("filetype", filetype, pos=parent.frame(n=1))
	###
	if (!all(is.character(naString)) | length(naString) != 1) {
		stop("Please provide a character length one to the argument 'naString'.", call.=FALSE)
	}
	###
	if (is(md, "aquap_md")) {
		filename <- md$meta$expName
	} else {
		stop("Please provide a valid metadata object to the argument 'md'", call.=FALSE)
	}
	rawFolder <- stn$fn_rawdata
	a <- list.files(rawFolder)
	if (!any(grepl(filename, a, fixed=TRUE))) {
		stop(paste("The file \"", filename, "\" does not seem to exist in \"", rawFolder, "\", sorry. \n", sep=""), call.=FALSE)
	}
	assign("filename", filename, pos=parent.frame(n=1))
} # EOF


#' @title Read Spectra
#' @description Reads in just the spectra from the provided rawdata file.
#' @details This function is mainly for providing a possibility to test custom 
#' written rawdata import functions. (see \code{\link{custom_import}})
#' @inheritParams getFullData
#' @return The spectral data in the format as described in 'Value' in 
#' \code{\link{custom_import}}
#' @seealso \code{\link{getFullData}}
#' @family Development Functions
#' @export
readSpectra <- function(md=getmd(), filetype="def", naString="NA", sh=NULL) {
	stn <- autoUpS()
	possibleFiletypes <- pv_filetypes #global constant, they get handed down to the checking function !  	
#	pv_filetypes <- c("vision_NSAS.da", "tabDelim.txt", "Pirouette.pir", "xls", "YunosatoDatFile.dat", "MicroNIR.csv")
	filename <- NULL # will be changed in the checking
	readSpec_checkDefaults(possibleFiletypes, md, filetype, naString, sh)
	rawFolder <- stn$fn_rawdata
	folderFile <- paste(rawFolder, "/", filename, sep="")
	##
	if (filetype == possibleFiletypes[1]) {
		aaa <- paste(folderFile, ".da", sep="")
		assign("spectraFilePath", aaa, pos=parent.frame(n=1))
		return(getNIRData_Vision_da(aaa))
	}
	##
	if (filetype == possibleFiletypes[2]) {
		aaa <- paste(folderFile, ".txt", sep="")
		assign("spectraFilePath", aaa, pos=parent.frame(n=1))
 		return(getNirData_plainText(aaa, naString))
	}
	if (filetype == possibleFiletypes[3]) {
		aaa <- paste(folderFile, ".pir", sep="")
		assign("spectraFilePath", aaa, pos=parent.frame(n=1))
 		return(getNIRData_Pirouette(aaa))
	} 
	if (filetype == possibleFiletypes[4]) {
		aaa <- paste(folderFile, ".xlsx", sep="")
		assign("spectraFilePath", aaa, pos=parent.frame(n=1))
 		return(getNirData_Excel(aaa, stn))
	}
	if (filetype == possibleFiletypes[5]) {
		aaa <- paste(folderFile, ".dat", sep="")
		assign("spectraFilePath", aaa, pos=parent.frame(n=1))
 		return(getNirDataPlusMeta_YunosatoDat(aaa, stn)) # per definition, the Yunosato .dat file contains raw spectra AND class and numeric variables
	}	
	if (filetype == possibleFiletypes[6]) {
		aaa <- paste(folderFile, ".csv", sep="")
		assign("spectraFilePath", aaa, pos=parent.frame(n=1))
 		return(getNirData_microNIR(aaa, stn)) # # is assigning instrSerNr in gfd()
	}
	## if nothing of the above happend, then we must have (checked!) the path to a valid custom .r file in "filetype" 
	custName <- strsplit(filetype, "custom@")[[1]][2]
	if (is.null(sh)) { # the normal case
		shName <- aquap2_handover_to_uniset()$pkgUniset_RenvironSettingsHomeName
		pathSH <- Sys.getenv(shName)
	} else { # so incoming sh is not null, we are in a test scenario
		pathSH <- sh
	} # end else
	pathToCustom <- paste(pathSH, custName, sep="/")
	e <- new.env()
	sys.source(pathToCustom, envir=e)
	aaa <- paste(folderFile, e$fileExtension, sep="")
	assign("spectraFilePath", aaa, pos=parent.frame(n=1))
	return(e$spectralImport(aaa))
} # EOF

gfd_check_imports <- function(specImp) {
#	outList <- list(sampleNr=sampleNr, conSNr=conSNr, timePoints=timePoints, ecrm=ecrm, repl=repl, group=group, temp=temp, relHum=relHum, C_cols=C_cols, Y_cols=Y_cols, timestamp=timestamp, info=info, NIR=NIR)
	namesOfListElements <- c("sampleNr", "conSNr", "timePoints", "ecrm", "repl", "group", "temp", "relHum", "C_cols", "Y_cols", "timestamp", "info", "NIR") ## XXXVARXXX
	if (!all(names(specImp) %in% namesOfListElements)) {
		stop("One or more of the elements in the list of your custom import function do have wrong names. \nPlease see the help for 'custom_import' for further information.", call.=FALSE)
	}
	namesOfInfoList <- c("nCharPrevWl") 	## XXXVARXXX
	if (!all(names(specImp$info) %in% namesOfInfoList)) {
		stop("One or more of the elements in the info-list generated by your custom import function do have wrong names. \nPlease see the help for 'custom_import' for further information.", call.=FALSE)		
	}
	a <- specImp$sampleNr
	if (!is.null(a)) {
		if (!is.data.frame(a)) {
			stop(paste("Please provide a data frame for the element 'sampleNr' in the import function."),call.=FALSE)
		}
	}
	a <- specImp$conSNr
	if (!is.null(a)) {
		if (!is.data.frame(a)) {
			stop(paste("Please provide a data frame for the element 'conSNr' in the import function."),call.=FALSE)
		}
	}
	a <- specImp$timePoints
	if (!is.null(a)) {
		if (!is.data.frame(a)) {
			stop(paste("Please provide a data frame for the element 'timePoints' in the import function."),call.=FALSE)
		}
	}
	a <- specImp$ecrm
	if (!is.null(a)) {
		if (!is.data.frame(a)) {
			stop(paste("Please provide a data frame for the element 'ecrm' in the import function."),call.=FALSE)
		}
	}
	a <- specImp$repl
	if (!is.null(a)) {
		if (!is.data.frame(a)) {
			stop(paste("Please provide a data frame for the element 'repl' in the import function."),call.=FALSE)
		}
	}
	a <- specImp$group
	if (!is.null(a)) {
		if (!is.data.frame(a)) {
			stop(paste("Please provide a data frame for the element 'group' in the import function."),call.=FALSE)
		}
	}
	a <- specImp$temp
	if (!is.null(a)) {
		if (!is.data.frame(a)) {
			stop(paste("Please provide a data frame for the element 'temp' in the import function."),call.=FALSE)
		}
	}
	a <- specImp$relHum
	if (!is.null(a)) {
		if (!is.data.frame(a)) {
			stop(paste("Please provide a data frame for the element 'relHum' in the import function."),call.=FALSE)
		}
	}
	a <- specImp$C_cols
	if (!is.null(a)) {
		if (!is.data.frame(a)) {
			stop(paste("Please provide a data frame for the element 'C_cols' in the import function."),call.=FALSE)
		}
	}
	a <- specImp$Y_cols
	if (!is.null(a)) {
		if (!is.data.frame(a)) {
			stop(paste("Please provide a data frame for the element 'Y_cols'" ), call.=FALSE)
		}
	}
	a <- specImp$timestamp
	if (!is.null(a)) {
		if (!is.data.frame(a)) {
			stop(paste("Please provide a data frame for the element 'timestamp' in the import function."),call.=FALSE)
		}
	}
	if (!is.null(specImp$NIR)) {
		if (!is.matrix(specImp$NIR)) {
			stop(paste("Please provide a matrix for the element 'NIR' in the import function."),call.=FALSE)
		}
	}
	a <- specImp$info$nCharPrevWl
	if (!all(is.numeric(a)) | length(a) !=1) {
		stop(paste("Please provide an integer length 1 for the element 'nCharPrevWl' in the import function."),call.=FALSE)
	} # end if
	ncpwl <- specImp$info$nCharPrevWl
	options(warn = -1)
	a <- as.numeric(substr(colnames(specImp$NIR), ncpwl+1, nchar(colnames(specImp$NIR))))
	options(warn = 0)
	if (any(is.na(a))) {
		stop("There is an error with the column names of the NIR spectra representing the wavelength. \nMaybe not all columns do have the same number of characters before the wavelength, or some wrong columns are assigned to the NIR columns, what can happen if the number of columns in the metadata xls sheet is not set correctly. \nPlease check in the import function if the provided number of characters before the wavelength is correct, and if the column names of the NIR data are correct as well.", call.=FALSE)
	}
	return(invisible(NULL))
} # EOF

gfd_makeNiceColumns <- function(specImp) {
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
	absTime <- stn$p_absTime
	chron <- stn$p_chron
	tiStaColName <- gl_timestamp_column_name
	nr <- nrow(specImp$NIR)
	##
	if (!is.null(specImp$sampleNr)) {
		colnames(specImp$sampleNr) <- paste(yPref, sampleNrColn, sep="")
	} else {
		specImp$sampleNr <- data.frame(DELETE = rep(NA, nr))
	}
	if (!is.null(specImp$conSNr)) {
		colnames(specImp$conSNr) <- paste(yPref, conSNrColn, sep="")
	} else {
		specImp$conSNr <- data.frame(DELETE = rep(NA, nr))
	}	
	if (!is.null(specImp$timePoints)) {
		if (!is.factor(specImp$timePoints[,1])) {
			specImp$timePoints[,1] <- as.factor(specImp$timePoints[,1])
		}
		colnames(specImp$timePoints) <- paste(cPref, timePointsColn, sep="")
	}  else {
		specImp$timePoints <- data.frame(DELETE = rep(NA, nr))
	}	
	if (!is.null(specImp$ecrm)) {
		if (!is.factor(specImp$ecrm[,1])) {
			specImp$ecrm[,1] <- as.factor(specImp$ecrm[,1])
		}	
		colnames(specImp$ecrm) <- paste(cPref, ecrmColn, sep="")
	}  else {
		specImp$ecrm <- data.frame(DELETE = rep(NA, nr))
	}	
	if (!is.null( specImp$repl)) {
		if (!is.factor(specImp$repl[,1])) {
			specImp$repl[,1] <- as.factor(specImp$repl[,1])
		}		
		colnames(specImp$repl) <- paste(cPref, replColn, sep="")
	}  else {
		specImp$repl <- data.frame(DELETE = rep(NA, nr))
	}	
	if (!is.null(specImp$group)) {
		if (!is.factor(specImp$group[,1])) {
			specImp$group[,1] <- as.factor(specImp$group[,1])
		}
		colnames(specImp$group) <- paste(cPref, groupColn, sep="")
	} else {
		specImp$group <- data.frame(DELETE = rep(NA, nr))
	}	
	if (!is.null(specImp$temp)) {
		colnames(specImp$temp) <- paste(yPref, tempColn, sep="")
	} else {
		specImp$temp <- data.frame(DELETE = rep(NA, nr))
	}	
	if (!is.null(specImp$relHum)) {
		colnames(specImp$relHum) <- paste(yPref, relHumColn, sep="")
	} else {
		specImp$relHum <- data.frame(DELETE = rep(NA, nr))
	}	
	if (!is.null(specImp$C_cols)) {
		lePref <- nchar(cPref)
		a <- colnames(specImp$C_cols)
		noPrefInd <- which(substr(a, 1, lePref) != cPref)
		if (length(noPrefInd) > 0) {
			colnames(specImp$C_cols)[noPrefInd] <- paste(cPref, a[noPrefInd], sep="")
		}  # end if
		specImp$C_cols <- as.data.frame(apply(specImp$C_cols, 2, function(x) {
				co <- as.factor(x)
				names(co) <- NULL
				return(co)
			} ))
	} else {
		specImp$C_cols <- data.frame(DELETE = rep(NA, nr))
	}
	if (!is.null(specImp$Y_cols)) {
		lePref <- nchar(yPref)
		a <- colnames(specImp$Y_cols)
		noPrefInd <- which(substr(a, 1, lePref) != yPref)
		if (length(noPrefInd) > 0) {
			colnames(specImp$Y_cols)[noPrefInd] <- paste(yPref, a[noPrefInd], sep="")
		}
		specImp$Y_cols <- convertYColsToNumeric(specImp$Y_cols)
	} else {
		specImp$Y_cols <- data.frame(DELETE = rep(NA, nr))
	}
	if (!is.null(specImp$timestamp)) {
		if (stn$imp_makeTimeDistanceColumn) {
			startDate <- as.POSIXct(stn$imp_startDate)
			startDateNr <- unclass(startDate)
			a <- unclass(specImp$timestamp[,1]) # OK
			deltaT <- (a - startDateNr)
			chrons <- data.frame(absTime=deltaT, chron=1:length(deltaT))
			specImp$timestamp <- cbind(specImp$timestamp, chrons)
			colnames(specImp$timestamp) <- c(tiStaColName, paste0(yPref, absTime), paste0(yPref, chron))
		} else {
			colnames(specImp$timestamp) <- tiStaColName		
		}
	} else {
		specImp$timestamp <- data.frame(DELETE = rep(NA, nr))
	}
	return(specImp)
} # EOF

gfd_checkNrOfRows <- function(header, headerFilePath, nrowsNIR, spectraFilePath, multiplyRows, nrConScans) {
	if (!is.null(header)) {
		if (nrow(header) != nrowsNIR) {
			if (multiplyRows) {
				to <- paste("after multiplying the rows in the sample list by ", nrConScans, " \n(if no correction from error column was applied) consecutive scans", sep="")
			} else {
				to <- "(multiplication of rows was *not* performed)"
			} # end else
			stop(paste("The header that was constructed from the file \n\"", headerFilePath, "\"\n consists of ", nrow(header), " rows ", to, ", while the imported spectra from file \n\"", spectraFilePath, "\"\n consist of ", nrowsNIR, " rows. \nPlease make sure that all data to be imported have the same number of rows.", sep=""), call.=FALSE)
		} # end if nrow not good
	} # end if is null header
} # EOF

gfd_getExpNameNoSplit <- function(metadata, nRows) {
	stn <- getstn()
	cPref <- stn$p_ClassVarPref
	makeExpNameColumn <- stn$imp_makeExpNameColumn
#	makeNoSplitColumn <- stn$imp_makeNoSplitColumn
	makeNoSplitColumn <- TRUE
	##
	if (makeExpNameColumn) {
		expName <- data.frame(rep(metadata$meta$expName, nRows))
		colnames(expName) <- paste(cPref, stn$p_expNameCol, sep="")
	} else {
		expName <- data.frame(DELETE = rep(NA, nRows))
	}
	assign("expName", expName, pos=parent.frame(n=1))
	##
	if (makeNoSplitColumn) {
		noSplit <- data.frame(rep(stn$p_commonNoSplit, nRows))
		colnames(noSplit) <- paste(cPref, stn$p_commonNoSplitCol, sep="")
	} else {
		noSplit <- data.frame(DELETE = rep(NA, nRows))	
	}
	assign("noSplit", noSplit, pos=parent.frame(n=1))
} # EOF

gfd_checkRemoveDoubleColumns <- function(header, spectraFilePath=NULL, headerFilePath=NULL, slType=NULL, remDC=TRUE, remMsg=TRUE) {
#	return(header)
	collect  <-  NULL
	patterns <- paste(".", 1:12, sep="")
	allCns <- colnames(header)
	alteKollekte <- function(allCns, patterns) {
		for (k in 1: length(patterns)) {	
			inds <- grep(patterns[k], allCns)
			if (length(inds) > 0) {
				collect <- c(collect, inds)
			}
		}  # end for k	
	} # EOIF
	#
	makeKollekte <- function(allCns, patterns) {
		for (k in 1: length(patterns)) {
			inds <- endsWith(allCns, patterns[k]) # is a logical vector the full length of allCns
#			print("---------");print(patterns[k]); print(length(which(inds))); wait()
			indsNum <- which(inds == TRUE)
			if (length(indsNum) > 0) {
				collect <- c(collect, indsNum)
			}
		} # end for k
		return(collect)
	} # EOIF
	collect <- makeKollekte(allCns, patterns)
#	dblCns <- allCns[sort(collect)]
	if (is.null(collect)) {
		return(header)
	} else {
		if (remDC) { # so we want to remove double columns
			if (remMsg) {
				leco <- length(collect)
				if (leco == 1) {add <- " has"} else {add <- "s have"} 
				msg <- paste0("   * ",leco, " double column", add, " been removed from the dataset. *\n")
				cat(msg)
			} # end if remMsg
			return( header[,-collect])		# #	headerClean <- header[,-collect] # we had doubles
		} else { # so no removing of double columns, go towards error
			if (is.null(slType)) {
				files <- paste("\"", spectraFilePath, "\"", sep="")		
			} else {
 				files <- paste("\"", headerFilePath, "\" and ", "\"", spectraFilePath, "\".", sep="")
			} # end else
			cols <- paste(allCns[collect], collapse=", ")
			leco <- length(collect)
			if (leco == 1) {add <- " column seems"} else {add <- " columns seem"} 
			msg <- paste("The following ", length(allCns[collect]), add, " to appear more than once: \n", cols,"\n-----------------\nPlease check the files used for importing data, that is \n", files, "\n-----------------\n", sep="")
			msg2 <- paste("Alternatively, you can set 'remDC' in the import function 'gfd()' to TRUE")
			stop(c(msg,  msg2), call.=FALSE)	
		} # end else remDC		
	} # end else is.null
#	print("---------");print(colnames(headerClean)); print(length(colnames(headerClean))); print("---------");
} # EOF

gfd_importData <- function() {


} # EOF

gfd_checkLoadSaveLogic <- function(ttl, stf) {
	if (!is.logical(ttl) | !is.logical(stf)) {
		stop("Please provide 'TRUE' or 'FALSE' to the arguments 'ttl' / 'stf' ", call.=FALSE)
	}
} # EOF

gfd_checkMetadata <- function(md) {
	if (!is(md, "aquap_md")) {
		stop("Please provide a valid metadata object of class 'aquap_md' to the argument 'md'", call.=FALSE)
	}
} # EOF

gfd_TRH_checkCustomImport <- function(trhImp) {
	possVals <- c("Time", "Temp", "RelHum")
	if (!is(trhImp, "data.frame")) {
		stop("The returned value of your custom-import function for importing temp. and rel.hum. has to be a data frame.", call.=FALSE)
	}
	if (ncol(trhImp) != 3) {
		stop("The returned data frame of your custom-import function for importing temp. and rel.hum. has to have three columns", call=FALSE)
	}
	if (!any(grepl("POSIX", class(trhImp[1,1]))) ) {
		stop("The first column has to contain timestamps in 'POSIX' format.", call.=FALSE)
	}
	if (!is.numeric(trhImp$Temp) | !is.numeric(trhImp$RelHum)) {
		stop("The temperature and rel.humidity columns have to contain numerical values.", call=FALSE)
	}
	if (!all(possVals %in% colnames(trhImp))) {
		stop("The column names of the imported dataframe have to be 'Time', 'Temp' and 'RelHum'", call.=FALSE)		
	}
} # EOF

readTRHlogfile <- function(trhLog) {
	stn <- getstn()
	rawFolder <- stn$fn_rawdata
	logfileName <- stn$imp_TRH_logfile_name
	pvLTs <- pv_loggerTypes # c("ESPEC", "HOBO")

	if (trhLog == pvLTs[1]) { # ESPEC
		ext <- ".txt"
		path <- paste(rawFolder, "/", logfileName, ext, sep="")
		TRHimp <- importTRH_ESPEC(path)
	} # end if ESPEC
	#
	if (trhLog == pvLTs[2]) { # HOBO
		# we say in the manual that 'HOBO' can import both .csv and .xlsx files. 
		# First check which one is there, if both are there throw an error
		# we already checked in gfd_check_trhLog_defaults that we do not have doubles or nothing
		# so we must have one of the below
		pathCsv <- paste0(rawFolder, "/", logfileName, ".csv")
		pathXls <- paste0(rawFolder, "/", logfileName, ".xlsx")
		haveCsv <- file.exists(pathCsv)
		haveXls <- file.exists(pathXls)
		if (haveCsv) {
			finalPath <- pathCsv
			what <- "csv"
		} else {
			finalPath <- pathXls		
			what <- "xls"
		} # end else 	
		TRHimp <- importTRH_HoboWare(finalPath, what)
	} # end if HOBO
	#
	if (grepl("custom@", trhLog)) {
		custName <- strsplit(trhLog, "custom@")[[1]][2]
		shName <- aquap2_handover_to_uniset()$pkgUniset_RenvironSettingsHomeName
		pathSH <- Sys.getenv(shName)
		pathToCustom <- paste(pathSH, custName, sep="/")
		e <- new.env()
		sys.source(pathToCustom, envir=e)
		a <- paste(rawFolder, "/", logfileName, e$fileExtension, sep="")
		TRHimp <- e$trhImport(a)
		gfd_TRH_checkCustomImport(TRHimp)
	}
	return(TRHimp)	
} # EOF

alignTempRelHum <- function(timeDataset, TRHlog) {	
	stn <- getstn()
	narrowMinutes <-stn$imp_narrowMinutes
	secsNarrowPrecision <-stn$imp_secsNarrowPrecision
	mtp <-stn$imp_minutesTotalPrecision
	diffT <- abs(difftime(TRHlog$Time[1], TRHlog$Time[2], units="secs"))
	TIndUpRange <- round((narrowMinutes*60)/as.double(diffT), 0) ## for a narrowed-down area of narrowMinutes
	rowNr <- rep(NA, length(timeDataset))
	if (min(abs(difftime(TRHlog$Time, timeDataset[1], units="secs"))) > mtp*60) {
		stop(paste("The first spectra aquisition time seems to be more than", mtp, "minutes separated from any data in the time-log. \nThe import of the  data-file has been aborted. Sorry."), call.= FALSE)
	} # end if difftime 
	firstIndex <- which( (TRHlog$Time-timeDataset[1]) == min( abs(TRHlog$Time-timeDataset[1])) )
	if (length(firstIndex) < 1 ) {
		firstIndex <- which( -1*(TRHlog$Time-timeDataset[1]) == min(abs(TRHlog$Time-timeDataset[1])) )
	} # end if
	fromIndex <- firstIndex
	for ( i in 1: length(timeDataset)) {
		toIndex <- fromIndex + TIndUpRange
		if (toIndex > nrow(TRHlog)) {
			toIndex <- nrow(TRHlog)
		}
	## first check in the narrowed down area
		index <- which( (TRHlog$Time[fromIndex:toIndex] - timeDataset[i]) == min( abs(TRHlog$Time[fromIndex:toIndex] - timeDataset[i])) )
		if (length(index) < 1 ) {
			index <- which( -1*(TRHlog$Time[fromIndex:toIndex] - timeDataset[i]) == min(abs(TRHlog$Time[fromIndex:toIndex] - timeDataset[i])) )
		} # end if
		index <- fromIndex + index - 1
		if (min(abs(difftime(TRHlog$Time[index], timeDataset[i], units="secs"))) > secsNarrowPrecision) {
			index <- NULL
		}
	## if nothing, look in the whole vector
		if (length(index) < 1) { ## so if we still did not find a matching Time in the given narrowed down range, then go look through the whole set
			index <- which( (TRHlog$Time - timeDataset[i]) == min( abs(TRHlog$Time - timeDataset[i])) )
		}
		if (length(index) < 1) {
			index <- which( -1*(TRHlog$Time - timeDataset[i]) == min(abs(TRHlog$Time - timeDataset[i])) )
		}
		fromIndex <- index
		rowNr[i] <- index
		if (min(abs(difftime(TRHlog$Time[index], timeDataset[i], units="secs"))) > mtp*60) {
			stop(paste("I am sorry, but I could not find log-data for", timeDataset[i], "less than", mtp, "minutes away. The import of the data-file has been aborted."), call.= FALSE)
		}
	} # end for i
	out <- TRHlog[rowNr ,]
} # EOF

gfd_importMakeTRH_alignment <- function(header, trhLog) {
	stn <- getstn()
	a <- which(colnames(header) == gl_timestamp_column_name)
	if (length(a) == 0 & trhLog != FALSE) {
		message(paste("There is no '", gl_timestamp_column_name, "' column in your dataset to which data from a logger could be aligned. \n(Alignment is controlled by argument 'trhLog'.) "))
		message("Importing continues without the alignment of log data.")
	}
	if (length(a) !=0 ) {	 # so only do all this if we actually have a timestamp in the columns
		tempCol <- paste(stn$p_yVarPref, stn$p_tempCol, sep="")
		rhCol <- paste(stn$p_yVarPref, stn$p_RHCol, sep="")
		if (trhLog == FALSE) {
			return(header)
		}
		tInd <- which(colnames(header) == tempCol) # check if we do have a temp column
		if (length(tInd) == 0) { # it not, add one
			cns <- colnames(header)
			header <- cbind(header, data.frame(rep(NA, nrow(header))))
			colnames(header) <- c(cns, tempCol)
			tInd <- which(colnames(header) == tempCol)
		}
		rhInd <- which(colnames(header) == rhCol)
		if (length(rhInd) == 0) {
			cns <- colnames(header)
			header <- cbind(header, data.frame(rep(NA, nrow(header))))
			colnames(header) <- c(cns, rhCol)
			rhInd <- which(colnames(header) == rhCol)
		}
		TRHimp <- readTRHlogfile(trhLog)
		if (!stn$allSilent) {cat("   Aligning temp. and rel.hum. values to timestamp...\n") }
		TRH_alig <- alignTempRelHum(header$Timestamp, TRHimp)
		if (!stn$allSilent) {cat("   Done.\n") }
#			tempF <- data.frame(SpectTime= header$Timestamp, LogTime = TRH_alig$Time, Temp=TRH_alig$Temp, Hum=TRH_alig$RelHum)
#			print(tempF[1:20,]); wait()
		header[tInd] <- TRH_alig$Temp
		header[rhInd] <- TRH_alig$RelHum 
	} # end checking if we have a timestamp column
	return(header)
} # EOF

gfd_check_trhLog_defaults <- function(trhLog) {
	stn <- getstn()
	# can be: FALSE, 'def' "ESPEC", "custom@name.r"
	filename <- stn$imp_TRH_logfile_name
	rawFolder <- stn$fn_rawdata
	possibleValues <- pv_loggerTypes # c("ESPEC", "HOBO")
	stopMsg <- "Please refer to the help for 'getFullData' to find out about the possible values for the argument 'trhLog'"
	if (trhLog == TRUE) {
		stop(stopMsg, call.=FALSE)
	}
	if (!is.logical(trhLog)) {
		if (all(trhLog == "def")) {
			trhLog <- stn$imp_use_TRH_logfile
		}
	}
	afix <- any(possibleValues %in% trhLog)
	bcust <- grepl("custom@", trhLog)
	if (!is.logical(trhLog) & !afix & !bcust ) {
		stop(stopMsg, call.=FALSE)
	}
	if (afix) {
		if (trhLog == "ESPEC") {
			ext <- ".txt"
			if (!file.exists(paste(rawFolder, "/", filename, ext, sep=""))) {
				stop(paste("The file \"", filename, ext, "\" does not seem to exist in the ", rawFolder, " folder, sorry.", sep=""), call.=FALSE)
			}
		} # end ESPEC
		if (trhLog == "HOBO") {
			# we say in the manual that 'HOBO' can import both .csv and .xlsx files. 
			# First check which one is there, if both are there throw an error
			haveCsv <- file.exists(paste0(rawFolder, "/", filename, ".csv"))
			haveXls <- file.exists(paste0(rawFolder, "/", filename, ".xlsx"))
			if (haveCsv & haveXls) {
				stop(paste0("It seems that both '.csv' and '.xlsx' data logger files are present.\n   Please decide which one to use and remove or rename the other one. \n   Thank you."), call.=FALSE)
			} # end if
			if (!haveCsv & !haveXls) {  # so we have nothing
				stop(paste0("The file '", filename, ".csv/.xlsx' does not seem to exist in the ", rawFolder, " folder, sorry."), call.=FALSE)
			} # end if
		} # end HOBO		
	} # end if afix	
	if (bcust) {
		shName <- aquap2_handover_to_uniset()$pkgUniset_RenvironSettingsHomeName
		shPath <- Sys.getenv(shName)
		# XXX modify here for testing !!
		customFilename <- strsplit(trhLog, "custom@")[[1]][2]
		if (!file.exists(paste(shPath, "/", customFilename, sep=""))) {
			stop(paste("The file \"", customFilename, "\" does not seem to exist in ", shPath, ", sorry.", sep=""), call.=FALSE)
		}
		a <- list.files(rawFolder)
		isThere <- any(grepl(filename, a))
		if(!isThere) {
			stop(paste("The file \"", filename, "\" does not seem to exist in ", rawFolder, ", sorry.", sep=""), call.=FALSE)
		}
	}	
	assign("trhLog", trhLog, pos=parent.frame(n=1))
} # EOF

flagOutliers_allScope <- function(NIR, detectOutliers) {
	stn <- getstn()
	# first check the content of detectOutliers
	if (all(detectOutliers == "def") & !is.null(detectOutliers)) {
		detectOutliers <- stn$imp_flagOutliers
	}
	if (!all(is.logical(detectOutliers)) | length(detectOutliers) > 1) {
		stop("Please provide either TRUE or FALSE (or 'def') to the argument 'dol'.", call.=FALSE)
	} # so now we are sure to have either TRUE or FALSE in detectOutliers
	##
	if (!detectOutliers) {
		return(data.frame(DELETE = rep(NA, nrow(NIR))))
	} else {
		cPref <- stn$p_ClassVarPref
		cnOtl <- paste0(stn$p_outlierCol, stn$p_outlierCol_allSuffix)
		tol <- stn$simca_tolerance
		kmax <- stn$simca_kMax
		flatDf <- data.frame(grouping=rep("x", nrow(NIR)))
		flatDf <- cbind(flatDf, as.data.frame(NIR))
#		kmax <- 10
		if (!stn$allSilent) {cat("   detecting outliers... ")}
		simcaMod <- rrcovHD::RSimca(grouping ~ ., data=flatDf, kmax=kmax, tol=tol)  ## k=0 does not work ??, but still calculating k
		flags <- as.factor(!simcaMod@flag) # to invert them and tranform to factor, having TRUE for the outliers
		nrOutliers <- length(which(flags == TRUE))
		usedK <- simcaMod@k
		if (nrOutliers == 0) {
			msg <- "none found. "
		} else {
			msg <- paste("found *", nrOutliers, "*. [Using ", usedK, " components.] ", sep="")
		}
		if (!stn$allSilent) {cat(paste(msg, "\n", sep=""))}
#		print(str(simcaMod))
		outlierFlags <- data.frame(flags)
		colnames(outlierFlags) <- paste(cPref, cnOtl, sep="")
		return(outlierFlags)
	} # end else
} # EOF

checkDatasetVersion <- function(dataset, dsName=NULL) {
	stn <- getstn()
	if (stn$gen_versionCheckDataset) {
		if (dataset@version != pv_versionDataset) {
			if (is.null(dsName)) {
				dsName <- deparse(substitute(dataset))
			}
			stop(paste("The dataset '", dsName, "' was created with an older version of package 'aquap2' and so has a different structure than what is required now.\nPlease re-import the raw-data.\n(You can switch off the checking of the dataset-version in the settings-file at the parameter 'gen_versionCheckDataset'.)", sep=""), call.=FALSE)
		}
	}
} # EOF

checkForPresenceOfData <- function() {
	stn <- getstn()
	rawFolder <- stn$fn_rawdata
	aa <- list.files(rawFolder)
	if (length(aa) == 0) {
		stop(paste0("Sorry, it appears there are no data in the current '", rawFolder, "' folder."), call.=FALSE)
	}
	return(TRUE)
} # EOF

turnStringsIntoFactors <- function(header) {
	for (i in 1: ncol(header)) {
		if (is.character(header[,i])) {
			header[,i] <- as.factor(header[,i]) # 
		} # end if
	} # end for i
	return(header)
} # EOF

gfd_checkOverwrite_sl_trh_multRows <- function(md, slType, trhLog, multiplyRows) {
	# if in the input arguments (slType, trhLog, multiplyRows) in getFullData there is anything other than "def", this should be used instead of the values in the md object
	# assign the three args from md, if other than "def" in the function use that
	# if all three are left at "def", use what is in the md
	use_slType <- md$meta$slType
	use_trhLog <- md$meta$trhLog
	use_multRows <- md$meta$multRows
	#
	if (is.null(slType)) {
		use_slType <- NULL
	} else if (all(slType == "def")) {
		# then its good
	} else if (length(slType) != 1 | !all(slType == "xls")) {
		stop("Please provide either 'NULL' or 'xls' to the argument 'slType'.", call.=FALSE)
	} else {
		use_slType <- slType # so the input must have been "xls"
	} # end else
	assign("slType", use_slType, pos=parent.frame(n=1))	
	#
	if (trhLog != "def") {
		if (trhLog == TRUE) {
			stop("Please provide either 'FALSE' or a character length one to the argument 'trhLog'.", call.=FALSE)
		}
		if (trhLog != FALSE) {
			if (length(trhLog) != 1 | !all(is.character(trhLog))) {
				stop("Please provide a character length one to the argument 'trhLog'.", call.=FALSE)
			} # end if	
		} # end check
		use_trhLog <- trhLog
	} # end trhLog
	assign("trhLog", use_trhLog, pos=parent.frame(n=1))
	#
	if (multiplyRows != "def") {
		if (multiplyRows == "auto") {
			use_multRows <- "auto"
		} else if (length(multiplyRows) != 1 | !all(is.logical(multiplyRows)))  {
			stop("Please provide either 'TRUE', 'FALSE' or 'auto' to the argument 'multiplyRows'.", call.=FALSE)
		} else {
			use_multRows <- multiplyRows
		} # end else
	} # end multRows
	assign("multiplyRows", use_multRows, pos=parent.frame(n=1))	
} # EOF

gfd_removeHeaderDataFromRawImport <- function(si) {
	# the incoming 'si' is the import as generated by 'readSpectra'
	# info, NIR and possibly timestamp should remain
	sampleNr <- conSNr <- timePoints <- ecrm <- repl <- group <- temp  <- relHum <- C_cols <- Y_cols <- NULL
	outList <- list(sampleNr=sampleNr, conSNr=conSNr, timePoints=timePoints, ecrm=ecrm, repl=repl, group=group, temp=temp, relHum=relHum, C_cols=C_cols, Y_cols=Y_cols, timestamp=si$timestamp, info=si$info, NIR=si$NIR)
	return(outList)
} # EOF

# get full data ---------------------------------------------------------------
#' @template mr_getFullData
#' @export
getFullData <- function(md=getmd(), filetype="def", slType="def", trhLog="def", multiplyRows="def", ttl=TRUE, stf=TRUE, naString="NA", dol="def", sh=NULL, remDC=getstn()$imp_remDoubleCols, rawOnlyNIR=FALSE) {
	stn <- autoUpS()
	if (is.null(remDC)) { # that could happen in case a user did not yet call any update settings functions, and there´s still an old version of the settings.R file present
		remDC <- TRUE # the default is to remove double columns
	} # end if
	gfd_checkLoadSaveLogic(ttl, stf)
	gfd_checkMetadata(md)
	dataset <- NULL
	if (ttl) {
		dataset <- loadAQdata(md, verbose=FALSE)
	}
	if(!is.null(dataset)) { # so the path existed and it could be loaded
		checkDatasetVersion(dataset, md$meta$expName)
		if(!stn$allSilent) {cat(paste("Dataset \"", md$meta$expName, "\" was loaded.\n", sep="")) }
		return(invisible(dataset)) # returns the dataset and we exit here
	}
	# some more checking
  	checkForPresenceOfData()
  	gfd_checkOverwrite_sl_trh_multRows(md, slType, trhLog, multiplyRows) # is possible re-assigning: slType, trhLog, multiplyRows
	gfd_check_trhLog_defaults(trhLog) # 

  	# import starts 
  	if(!stn$allSilent) {cat("Importing data...\n")}	
	###########
	headerFilePath <- NULL # gets assigned in readHeader()
	header <- readHeader(md, slType, multiplyRows) ## re-assigns 'slType' and 'multiplyRows' also here -- in parent 2 level frame 
	###########									## if slType is NULL, header will be returned as NULL as well
	###########
	spectraFilePath <- instrumentInfo <- NULL # gets maybe assigned in readSpectra()
	si <-  readSpectra(md, filetype, naString, sh) ### !!!!!!!!! here the import !!!!!!!!! ## the MicroNIRS Import is assigning instrumentInfo here
	###########
	if (rawOnlyNIR) {
		si <- gfd_removeHeaderDataFromRawImport(si)
	} # end if
	gfd_check_imports(si) # makes sure eveything is NULL or data.frame / matrix (NIR)
	si <- gfd_makeNiceColumns(si) # makes all column names, transforms Y-variables to numeric
	nr <- nrow(si$NIR)
	gfd_checkNrOfRows(header, headerFilePath, nr, spectraFilePath, multiplyRows, nrConScans=md$postProc$nrConScans)  # makes sure spectra and sample list have same number of rows
	if (is.null(header)) {
		header <- data.frame(DELETE=rep(NA, nr)) # XXX adapt DELETE to possible custom value
	} # end if
	expName <- noSplit <- NULL # gets assigned below in gfd_getExpNameNoSplit()
	gfd_getExpNameNoSplit(metadata=md, nRows=nr)
	NIR <- si$NIR
	
	outliers <- flagOutliers_allScope(NIR, detectOutliers=dol) # the value of dol is checked / changeed inside this function
	#####
	headerFusion <- cbind(expName, noSplit, header, si$sampleNr, si$conSNr, si$timePoints, si$ecrm, si$repl, si$group, si$C_cols, si$Y_cols, outliers, si$temp, si$relHum, si$timestamp, stringsAsFactors=TRUE) # stringsAsFactors problem with the changed defaults in R 4.x --> but does not help here !!
	headerFusion <- headerFusion[, -(which(colnames(headerFusion) == "DELETE"))] 		
	headerFusion <- turnStringsIntoFactors(headerFusion) # see 2 lines above
	######

	check_conScanColumn(headerFusion, headerFilePath, spectraFilePath, slType, filetype) 
	# ? check for existence of sample number column as well ?
	headerFusion <- gfd_checkRemoveDoubleColumns(headerFusion, spectraFilePath, headerFilePath, slType, remDC)
	headerFusion <- gfd_importMakeTRH_alignment(headerFusion, trhLog)

	copiedCols <- NULL
	if (stn$imp_autoCopyYvarsAsClass) {  # if TRUE, copy all columns containing a Y-variable as class variable
		aaa <- copyYColsAsClass(headerFusion)  # remMsg=FALSE: we do not want to hear about removing all the copied stuff. just upstairs is enough. 
			headerFusion <- aaa$headerExt
			copiedCols <- aaa$newColnames
#		headerFusion <- copyYColsAsClass(headerFusion) # remMsg=FALSE: we do not want to hear about removing all the copied stuff. just upstairs is enough. 
		headerFusion <- gfd_checkRemoveDoubleColumns(headerFusion, remDC=remDC, remMsg=TRUE) # because if we exported header to sample list, we already have the Y as C copied elements here.
	} # end if
	headerFusion <- remakeTRHClasses_sys(headerFusion)
	colRep <- extractClassesForColRep(headerFusion)		## the color representation of the factors
	rownames(NIR) <- make.unique(rownames(NIR)) # just to be sure
	rownames(headerFusion) <- rownames(colRep) <- rownames(NIR)
	#
	header <- headerFusion
	fd <- data.frame(I(header), I(colRep), I(NIR))
	fullData <- new("aquap_data", fd)

#	fullData <- new("aquap_data")
#	fullData@header <- headerFusion
#	fullData@colRep <- colRep
#	fullData@NIR <- NIR

#	fullData@version <- as.character(packageVersion("aquap2")) # do not do this -- only change the info manualy here if really the structure of the dataset is different.
	fullData@version <- pv_versionDataset # get from the constants -- change at the constants only if the structure of the dataset has changed !!! XXX
	fullData@metadata <- md
	fullData@colCopyInfo <- copiedCols 
	fullData@info <- list(instrument=instrumentInfo)
	fullData@anproc <- NULL # the ap not yet here of course
	fullData@ncpwl <- si$info$nCharPrevWl
	if (stf) {
		saveAQdata(fullData, md, verbose=TRUE)
	} else {
		if(!stn$allSilent) {cat("Done. (not saved) \n")}	
	}
	return(invisible(fullData))
} # EOF

#' @rdname getFullData
#' @export
gfd <- function(md=getmd(), filetype="def", slType="def", trhLog="def", multiplyRows="def", ttl=TRUE, stf=TRUE, naString="NA", dol="def", sh=NULL, remDC=getstn()$imp_remDoubleCols, rawOnlyNIR=FALSE) {
	return(getFullData(md, filetype, slType, trhLog, multiplyRows, ttl, stf, naString, dol, sh, remDC, rawOnlyNIR))
} # EOF


#' @title Save and load aquap2 datasets
#' @description Save and load the standard aquap2 dataset (class "aquap_data") 
#' to / from the 'R-data' folder.
#' @details  From the provided metadata the experiment name is extracted.
#' \itemize{
#'  \item saveAQdata The dataset is saved under the same name as the experiment name.
#'  \item laodAQdata The file having the same name as the experiment name is being 
#'  loaded.
#' } 
#' @inheritParams getFullData
#' @param dataset An object of class 'aquap_data'
#' @param verbose Logical, if messages should be displayed.
#' @return loadData 
#' @family Helper Functions
#' @seealso \code{\link{getFullData}}
#' @examples
#' \dontrun{
#' saveAQdata(dataset)
#' loadAQdata()
#' saveAQdata(dataset, getmd(expName="FooBar")) # save under the name 'FooBar'
#' }
#' @export
saveAQdata <- function(dataset, md=getmd(), verbose=TRUE) {
	stn <- autoUpS()
  	if (!is(dataset, "aquap_data")) {
    	stop("Please provide an object of class 'aquap_data' to the argument 'dataset'", call.=FALSE)
  	}
  	expName <- md$meta$expName
	path <- paste(stn$fn_rdata, expName, sep="/")
	save(dataset, file=path)
	if (verbose & !stn$allSilent) {
		cat(paste("Dataset saved under \"", path, "\".\n", sep=""))
	}
} # EOF

#' @rdname saveAQdata
#' @export
loadAQdata <- function(md=getmd(), verbose=TRUE) {
	stn <- autoUpS()
  	expName <- md$meta$expName
	path <- paste(stn$fn_rdata, expName, sep="/")
	if (file.exists(path)){
		a <- load(path)
		if (verbose & !stn$allSilent) {
			cat(paste("Dataset \"", path, "\" loaded.", sep=""))
		}
		return(invisible(eval(parse(text=a))))
	} else {
		if (verbose) {
			message(paste("Dataset \"", path, "\" does not seem to exist, sorry.", sep=""))
		}
		return(NULL)
	}	
} # EOF


#' @title Export Data to xlsx
#' @description Export a dataset to xlsx. The header and the NIR data are 
#' being exported into a single worksheet. In an additional worksheet called 
#' the name of the experiment with \code{_meta} as suffix, the number of columns 
#' in the header is denoted, as well as if the first column is the rownames.
#' In the third column called \code{ncpwl} the number of characters
#' before the actual wavelength is denoted.
#' @details By providing \code{expName="xxx"} in the \code{getmd} function at 
#' parameter \code{md} it is possible to save the generated xlsx file under 
#' a different name. See examples.
#' @param dataset An object of class 'aquap_data'
#' @param onlyNIR Logical. If only the NIR data should be exported to xlsx. If 
#' left at the default \code{FALSE}, the header as well as the NIR data will 
#' be exported into a single worksheet. 
#' @param rowns Logical. If rownames should be exported as well. Defaults to 
#' \code{TRUE}.
#' @inheritParams getFullData
#' @return Invisible \code{TRUE} for a successful, invisible \code{FALSE} for 
#' an unsuccessful generation of the xlsx file. Used for its side effect, i.e. 
#' to export a dataset to xlsx.
#' @family Helper Functions
#' @examples
#' \dontrun{
#' fd <- gfd()
#' export_ap2_ToXlsx(fd)
#' export_ap2_ToXlsx(fd, md=getmd(expName="otherName"))
#' }
#' @export
export_ap2_ToXlsx <- function(dataset, md=getmd(), onlyNIR=FALSE, rowns=TRUE) {
	stn <- autoUpS()
	fn_rawdata <- stn$fn_rawdata
	dataSheetSfx <- gl_xlsx_DataSheetSuffix
	metaSSfx <- gl_xlsx_metaSheetSuffix
	ncolHeader_name <- gl_xlsx_ncolHeader_name
	rownamesAsFirstColumn <- gl_xlsx_rownamesAsFirstColumn
	colnameNCPWL <- gl_xlsx_ncpwlColumn
	wsZoom <- 120
	#
	gfd_checkMetadata(md)
	if (!is(dataset, "aquap_data")) {
		stop("Please provide an object of class 'aquap_data' to the argument 'dataset'.", call.=FALSE)
	} # end if
	#
	if (!stn$allSilent) {cat(paste0("Writing data to xlsx... "))}
	#
	expName <- md$meta$expName
	wb <- openxlsx::createWorkbook(expName)
	ncpwl <- dataset@ncpwl
	dataSheet <- paste0(expName, dataSheetSfx)
	openxlsx::addWorksheet(wb, sheetName=dataSheet, zoom=wsZoom)
	haveRows <- rowns
	if (onlyNIR) {
		ncolHeader <- 0
		rowsAsFirstCol <- haveRows		
		outDf <- as.data.frame(cbind(as.matrix(dataset$NIR)))
	} else {	
		ncolHeader <- ncol(dataset$header)
		rowsAsFirstCol <- haveRows
		outDf <- as.data.frame(cbind(as.matrix(dataset$header), as.matrix(dataset$NIR)))
	} # end else
	openxlsx::writeData(wb, sheet=dataSheet, outDf, rowNames=haveRows)
	#
	metaSheet <- paste0(expName, metaSSfx)
	metaDf <- data.frame(x=ncolHeader, y=rowsAsFirstCol, z=ncpwl)
	colnames(metaDf) <- c(ncolHeader_name, rownamesAsFirstColumn, colnameNCPWL)
	openxlsx::addWorksheet(wb, sheetName=metaSheet, zoom=wsZoom)	
	openxlsx::writeData(wb, sheet=metaSheet, metaDf, rowNames=FALSE)
	openxlsx::setColWidths(wb, sheet=metaSheet, cols = 1:ncol(metaDf), widths = "auto")	
	path <- paste0(fn_rawdata, "/", expName, ".xlsx")
	ok <- openxlsx::saveWorkbook(wb, path, overwrite = TRUE, returnValue=TRUE)
	if (ok & !stn$allSilent) {
		cat(paste0("done.\n"))
	} else {
		message(paste0("Sorry, the xlsx file '", paste0(expName, ".xlsx"), "' could not be saved.\n"))
		return(invisible(FALSE))
	} # end else
	return(invisible(TRUE))
} # EOF

#' @title Export Header to Xls
#' @description Exports only the header, i.e. the class- and numerical variables, 
#' to an xls-file in the folder \code{sl-in}. The filename is the experiment name, 
#' followed by the suffix \code{_fromHeader} if parameter \code{asSlIn} is 
#' \code{FALSE}. If it is \code{TRUE}, a possibly existing sampleList-in file 
#' with the same name will be overwritten. 
#' @param dataset An object of class 'aquap_data'
#' @param asSlIn Logical, if the created xlsx file should be named ending in 
#' \code{-in}, thus being ready to reimport as sample list file. Defaults to 
#' \code{FALSE}. **Warning!** If \code{asSlIn} is set to \code{TRUE}, 
#' any existing sample list with the same name will be overwritten without 
#' warning!
#' @param rowns Logical. If rownames should be exported as well. Defaults to 
#' \code{TRUE}.
#' @param cutCCC Logical. If the numeric columns possibly copied as class 
#' variables during rawdata import should be omitted. Defaults to \code{TRUE}.
#' (cutCCC: 'cut copied C-Cols')
#' @inheritParams getFullData
#' @section Warning:
#' If \code{asSlIn} is set to \code{TRUE}, any existing sample list with the 
#' same name will be overwritten without warning!
#' @return \code{TRUE} if the operation was successful, \code{FALSE} if not. 
#' Used for its side effects, i.e. the header exported to xls in the \code{sl-in} 
#' folder. 
#' @family Helper Functions
#' @export
export_header_toXls <- function(dataset, md=getmd(), asSlIn=FALSE, rowns=TRUE, cutCCC=TRUE) {
	stn <- autoUpS()
	finSuff <- "_fromHeader"
	dataSheet <- "header"
	tscol <- gl_timestamp_column_name
	wsZoom <- 120
	haveRows <- rowns
	fn_sampleLists <- stn$fn_sampleLists
	fn_sampleListIn <- stn$fn_sampleListIn
	slInAdd <- "-in"
	#
	expName <- md$meta$expName
	if (asSlIn) {
		filename <- paste0(expName, slInAdd)
	} else {
		filename <- paste0(expName, finSuff)
	}
	
	wb <- openxlsx::createWorkbook(filename)
	openxlsx::addWorksheet(wb, sheetName=dataSheet, zoom=wsZoom)
	outDf <- getHeader(dataset)
	if (cutCCC) {
		ccpi <- dataset@colCopyInfo
		delInd <- which(colnames(outDf) %in% ccpi)
		outDf <- outDf[,-delInd]
	} # end if
	
	# we never should export the timestamp -- it is, if available, only in the raw data
	tsind <- which(colnames(outDf) == tscol)
	outDf <- outDf[,-tsind]
	
	if (!stn$allSilent) {cat(paste0("Writing the header to xlsx... "))}
	openxlsx::writeData(wb, sheet=dataSheet, outDf, rowNames=haveRows)	
	finaShow <- paste0(filename, ".xlsx")
	path <- paste0(fn_sampleLists, "/", fn_sampleListIn, "/", filename, ".xlsx")
	ok <- openxlsx::saveWorkbook(wb, path, overwrite = TRUE, returnValue=TRUE)
	if (ok & !stn$allSilent) {
		cat(paste0("done.\n"))
		cat(paste0("The file '", finaShow, "' has been written to '", fn_sampleLists, "/", fn_sampleListIn, "\n"))
		return(invisible(TRUE))
	} else {
		message(paste0("Sorry, the xlsx file '", filename, "' could not be saved.\n"))
		return(invisible(FALSE))
	} # end else
	

} # EOF

#' @title Import Raw, Export to Xls
#' @description Imports a given rawdata file in any of the supported formats, 
#' reads in the Timestamps (if present) and exports the content (NIR spectra 
#' and any class- and numerical variables) immediately to xlsx. Most checks 
#' that are done when calling 'gfd()' will be omitted. This function can be 
#' helpful if you want to just get your hands on the rawdata.
#' @inheritParams getFullData
#' @return Returns (invisible) the obtained data. Mainly used for its side-effects, 
#' i.e. to directly export rawdata to xlsx.
#' @export
importExport_raw_toXls <- function(md=getmd(), filetype="def", sh=NULL) {
	stn <- autoUpS()
	slType <- NULL
	trhLog <- FALSE
	ttl <- FALSE
	stf <- FALSE
	dol <- FALSE
	remDC <- FALSE
	multiplyRows <- FALSE
	rawOnlyNIR <- FALSE
	checkThings <- FALSE
	naString <- "NA"
	#
	fn_rawdata <- stn$fn_rawdata
	tiStaName <- gl_timestamp_column_name
	dataSheetSfx <- gl_xlsx_DataSheetSuffix
	metaSSfx <- gl_xlsx_metaSheetSuffix	
	colnameNCPWL <- gl_xlsx_ncpwlColumn
	ncolHeader_name <- gl_xlsx_ncolHeader_name
	rownamesAsFirstColumn <- gl_xlsx_rownamesAsFirstColumn
	haveRows <- TRUE
	wsZoom <- 120
	#
	assign(".slType", slType, pos=gl_ap2GD)
	dirRawSufx <- "_directRaw" 
	#
	checkForPresenceOfData()
  	gfd_checkOverwrite_sl_trh_multRows(md, slType, trhLog, multiplyRows) # is possible re-assigning: slType, trhLog, multiplyRows
	gfd_check_trhLog_defaults(trhLog)
	#	
	si <- readSpectra(md, filetype, naString, sh)
	si <- gfd_makeNiceColumns(si) # makes all column names, transforms Y-variables to numeric
	if (!is.null(si$timestamp)) {
		if (ncol(si$timestamp) > 1) {
			si$timestamp <- si$timestamp[,tiStaName, drop=FALSE] # kick out Y_absTime and Y_chron			
		} #end if
	} # end if
	nonNir <- cbind(si$sampleNr, si$conSNr, si$timePoints, si$ecrm, si$repl, si$group, si$C_cols, si$Y_cols, si$temp, si$relHum, si$timestamp, stringsAsFactors=TRUE) # stringsAsFactors problem with the changed defaults in R 4.x --> but does not help here !!
	indDel <- which(colnames(nonNir) == "DELETE")
	nonNir <- nonNir[,-indDel] # if there is no nonNir, data 'nonNir' comes in as data frame with zero columns
	NIR <- si$NIR
	######
	if (!stn$allSilent) {cat(paste0("Writing rawdata to xlsx... "))}
	#
	# we have to check how nonNir behaves when we only have spectra in the dataset
	expName <- md$meta$expName
	wb <- openxlsx::createWorkbook(expName)
	ncpwl <- si$info$nCharPrevWl
	dataSheet <- paste0(expName, dataSheetSfx)
	openxlsx::addWorksheet(wb, sheetName=dataSheet, zoom=wsZoom)
	ncolHeader <- ncol(nonNir)
	rowsAsFirstCol <- haveRows
	if (ncolHeader == 0) {
		outDf <- as.data.frame(cbind(as.matrix(NIR)))
	} else {	
		outDf <- as.data.frame(cbind(as.matrix(nonNir), as.matrix(NIR)))
	} # end else
	openxlsx::writeData(wb, sheet=dataSheet, outDf, rowNames=haveRows)
	#
	metaSheet <- paste0(expName, metaSSfx)
	metaDf <- data.frame(x=ncolHeader, y=rowsAsFirstCol, z=ncpwl)
	colnames(metaDf) <- c(ncolHeader_name, rownamesAsFirstColumn, colnameNCPWL)
	openxlsx::addWorksheet(wb, sheetName=metaSheet, zoom=wsZoom)	
	openxlsx::writeData(wb, sheet=metaSheet, metaDf, rowNames=FALSE)
	openxlsx::setColWidths(wb, sheet=metaSheet, cols = 1:ncol(metaDf), widths = "auto")	
	pathOrig <- paste0(fn_rawdata, "/", expName, ".xlsx")
	pathAdd <- paste0(fn_rawdata, "/", expName, dirRawSufx, ".xlsx")
	if (file.exists(pathOrig)) {
		fiNaAdd <- dirRawSufx
		cat(paste0("\n   The file '", paste0(expName, ".xlsx' seems to exist in '", fn_rawdata, "'.\n   The exported file containing the direct rawdata will be renamed into \n   '", expName, dirRawSufx, ".xlsx'.\n")))
		ok <- openxlsx::saveWorkbook(wb, pathAdd, overwrite = TRUE, returnValue=TRUE)
	} else { # so the original file des exist (should only happen in case of xlsx, and that would be stupid to export the same xlsx into xlsx. But... well... )
		fiNaAdd <- ""
		ok <- openxlsx::saveWorkbook(wb, pathOrig, overwrite = FALSE, returnValue=TRUE)		
	}
	if (ok & !stn$allSilent) {
		cat(paste0("done.\n"))
	} else {
		message(paste0("Sorry, the xlsx file '", paste0(expName, dirRawSufx, ".xlsx"), "' could not be saved.\n"))
		return(invisible(FALSE))
	} # end else
	return(invisible(outDf))	
} # EOF

# refine header -------------------------------------------------------------
transformNrsIntoColorCharacters <- function(numbers) {
	whatColors = c("black", "red", "green", "blue", "cyan", "magenta", "yellow2", "gray")
	colRamp <- colorRampPalette(whatColors)
	colorChar <- colRamp(length(unique(numbers))) 		## XXX unique needed here?
	return(as.character(colorChar[numbers]))
} # EOF

generateHeatMapColorCoding <- function(whatColors, numbers) {
	colRamp <- colorRampPalette(whatColors)
	colorChar <- colRamp(length(unique(numbers)))
	out <- as.character(colorChar[numbers])
} # EOF

extractClassesForColRep <- function(header) { ## does not need "NIR" present in the data frame
	stn <- getstn()
	tempCol <- stn$p_tempCol ## depends on "grepl" or not
	RHCol <- stn$p_RHCol
	absTimeCol <- stn$p_absTime
	chronCol <- stn$p_chron
	TRHColors <- stn$col_RampForTRH
	TimesColors <- stn$col_RampForTimes
	userRampColors <- stn$col_userDefinedRamps # is a list
	userColnames <- stn$p_userDefinedSpecialColnames # is a vector
	out <- data.frame(matrix(NA, nrow=nrow(header)))
	for (i in 1: ncol(header)) {
		if (is.factor(header[,i])) {
			fac <- header[,i]
			options(warn=-1)
			nums <- as.numeric(levels(fac))
			options(warn=0)
			if (any(is.na(nums))) {	# so we have a character
				a <- data.frame( as.numeric(unclass(header[,i]))) ##
			} else { # so we have a numeric
				nrs <- as.numeric(fac)
				ord <- order(as.numeric(levels(fac)))
				a <- as.numeric(sapply(nrs, function(x, Ord) which(Ord==x), Ord=ord)) # re-list them by their index, so to say... !! we have to keep in integers ! (Problem with level ordering!)
				a <- data.frame(a)
			}
			levelsA <- unique(a[,1])
			cn <- colnames(header[i])
			if (grepl(tempCol, cn) | grepl(RHCol, cn)) {
	#			print("TRH we have"); print(cn); print("-----")
				a[1] <- generateHeatMapColorCoding(TRHColors, a[,1])
	#			print(str(a))
			} else {
				if (grepl(absTimeCol, cn) | grepl(chronCol, cn)) {
					a[1] <- generateHeatMapColorCoding(TimesColors, a[,1])
				} else {
					ind <- which(lapply(userColnames, function(x) grep(x, cn)) == TRUE)
					if (length(ind) != 0) { # so there is a user defined special colname character in the colname cn
						a[1] <- generateHeatMapColorCoding(userRampColors[[ind]], a[,1])
					} else {
						if (length(levelsA) > 8) { # so we have none of the special cases and more than 8 levels (we only have 8 standard integer representations for colors)
							a[1] <- transformNrsIntoColorCharacters(a[,1])
						} # end if
					} # end else	
				} # end else
			} # end else
			names(a) <- colnames(header[i])
	#		print("the final is:"); print(str(a))
			out <- data.frame(out,a)
		} # end if is factor 
	} # end for i
	return(out[-1])		# cut off the first column containing only the NAs
} # EOF 

copyYColsAsClass <- function(sampleList) {
	# the incoming is the headerFusion
#	print(str(sampleList)); wait()
	stn <- getstn()
	yPref <- stn$p_yVarPref
	cPref <- stn$p_ClassVarPref
	ind  <- grep(yPref, colnames(sampleList))
#	print(ind); wait()
#	print(colnames(sampleList[ind])); wait()
	add <- data.frame(matrix(NA,nrow(sampleList)))
	for (i in 1: length(ind)) {
		colName <- names(sampleList[ind[i]])
		newColName <- sub(yPref, cPref, colName)
		numCol <- sampleList[, ind[i] ]
		factorCol <- factor(as.character(numCol), exclude=NA) 
		newDF <- data.frame(factorCol)
		names(newDF)[1] <- newColName
		add <- data.frame(add, newDF )
	} # end for i
	add <- add[-1]
	outList <- list(headerExt=data.frame(sampleList, add), newColnames = colnames(add))
	return(outList)
#	return(data.frame(sampleList, add))
} # EOF

remakeTRHClasses_sys <- function(headerOnly, TDiv=stn$imp_TClassesDiv, TRound=stn$imp_TRounding, RHDiv=stn$imp_RHClassesDiv, RHRound= stn$imp_RHRounding) {
	stn <- getstn()
	options(warn=-1)
	cPref <- stn$p_ClassVarPref
	yPref <- stn$p_yVarPref
	YTemp <- paste(yPref, stn$p_tempCol, sep="")
	YRH <- paste(yPref, stn$p_RHCol, sep="")
	Tpat <- paste(cPref, stn$p_tempCol, sep="") 						# read in the prefix for class and temperature from settings
	RHpat <- paste(cPref, stn$p_RHCol, sep="")							# read in the prefix for class and rel. humidity from settings
	alwaysReduceClasses <- stn$imp_alwaysReduceTRHClasses												
	
#	TInd <- grep(stn$p_tempCol, colnames(headerOnly), fixed=TRUE)		# find column-index that has the temperatur - the source
	TInd <- which(colnames(headerOnly) == YTemp)	# find column-index that has the temperatur - the source
	if (length(TInd) > 0) {
#		TClInd  <- grep(Tpat, colnames(headerOnly), fixed=TRUE)					# find column-index that the temperatur already as class - the target
		TClInd  <- which(colnames(headerOnly) == Tpat) 						# find column-index that the temperatur already as class - the target
		numsTemp <- headerOnly[, TInd[1] ]										# extract the numbers
		if (any(countDecimals(numsTemp) > 1) | alwaysReduceClasses) {			# to avoid reducing the number of classes when there is only one comma value
			headerOnly[TClInd] <- factor(round((numsTemp/TDiv),TRound)*TDiv)		# insert the new classes
		}
	}
	##
#	RHInd <- grep(stn$p_RHCol, colnames(headerOnly), fixed=TRUE)		# find column-index that has the rel. hum.
	RHInd <- which(colnames(headerOnly) == YRH)
	if (length(RHInd) > 0 ) {
#		RHClInd  <- grep(RHpat, colnames(headerOnly), fixed=TRUE)				# find column-index that the re.hum. already as class - the target
		RHClInd  <- which(colnames(headerOnly) == RHpat)
		numsRH <- headerOnly[, RHInd[1] ]										# extract the numbers
		if (any(countDecimals(numsRH) > 1) | alwaysReduceClasses) {
			headerOnly[RHClInd] <- factor(round((numsRH/RHDiv),RHRound)*RHDiv)		# insert the new classes
		}
	}
	options(warn=0)
	return(headerOnly)
} # EOF

# read header ----------------------------------------------------------------
convertYColsToNumeric <- function(sampleList) {
	stn <- getstn()
	options(warn=-1)
	pref <- stn$p_yVarPref
	ind  <- grep(pref, colnames(sampleList))
	if (length(ind) > 0) {
		for (i in 1: length(ind)) {
			sampleList[ind[i]] <- as.numeric( as.character(sampleList[, ind[i]]) )
		}
	} # end if
	options(warn=0)
	return(sampleList)
} # EOF

convertCColsToFactor <- function(rawHead) {
	for (i in 1: ncol(rawHead)) {
		if (all(is.character(rawHead[,i]))) {
			rawHead[,i] <- as.factor(rawHead[,i])
		} # end if
	} # end for i	
	return(rawHead)
} # EOF

### x-fold every line (Nr. Cons. Measurements), and add a numbering for each of the consecutive scans
gfd_possibly_multiplySampleListRows <- function(sampleList, nrScans, multiplyRows, doas = TRUE) {
	# we have to check multiplyRows: if it is:
	# a) "auto" (the default): see if we have the error column present. That means we do NOT have the Y_conSNr column present
	#  		if we HAVE the error column switch to b) TRUE, if not to c) FALSE
	# b) TRUE multiply rows, use the value in the error column to adjust
	#		inform: if an error col is present, if there are any values in it
	# 		check: throw an error if the Y_conSNr is present
	# c) FALSE take it as it is
	# 		check: throw an error if the error column is present
	#		check: throw an error if NO Y_conSNr is present
	stn <- getstn()
	yPref <- stn$p_yVarPref
	errCol <- paste0(yPref, gl_conScanError)
	conSNrCol <- paste0(yPref, stn$p_conSNrCol)
	sampleNrCol <- paste0(yPref, stn$p_sampleNrCol)
	slIn <- paste0(stn$fn_sampleLists, "/", stn$fn_sampleListIn)
	#
	doMultiply <- multiplyRows # multiplyRows comes in as "auto", TRUE or FALSE
	#
	cns <- colnames(sampleList)
	if ((errCol %in% cns) & (conSNrCol %in% cns)) {
		stop(paste0("There can not be a column for \n\tconsecutive scan number '", conSNrCol, "' and \n\tan error column '", errCol, "' in a sample list.\nPlease modify your sample list in '", slIn, "' "), call.=FALSE)
	} # end if
 	
 	##
 	if (multiplyRows == "auto") {
 		if ((!errCol %in% cns) & (!conSNrCol %in% cns)) { # would be the old single line sample lists without error column (and of course without conSNr column)
 			doMultiply <- TRUE 
 		} else if (errCol %in% cns) { # the single line sample list with error column
 			doMultiply <- TRUE 	
 		} else { # so we should have the conSNr column present
 			doMultiply <- FALSE
 		} # end else
 	}  # end if
 	if (doas) {
		assign("multiplyRows", doMultiply, pos=parent.frame(n=1)) # not elegant... meanwhile... sorry... really bad. True. But it grew over the years...
		assign("multiplyRows", doMultiply, pos=parent.frame(n=2))  # that is needed to always have the correct value for multiplyRows in the getFullData function
 	} # end if	
 	
 	if (!doMultiply) {
		if (!conSNrCol %in% cns) { # I think this should never happen.... 
 			stop(paste0("There should be the column for consecutive scans '", conSNrCol, "' in the sample list."), call.=FALSE)
 		}  # end if	
 		outList <- sampleList # outList will be packed into the list in the end
 	}  # end if !doMultiply
 	
 	##
 	if (doMultiply) {
		# if we are here doMultiply must be TRUE
		# if we multiply, it can be with or without error column
		if (!stn$allSilent) {cat("   Multiplying rows in sample list... \n")}
		errValUse <- rep(nrScans, nrow(sampleList))
		errValZero <- vector("numeric", nrow(sampleList))
		remErrCol <- FALSE
		if (errCol %in% cns) {
			errVal <- sampleList[,errCol] # is all NA if there are no values, or NA mixed with numbers
			indErr <- which(!is.na(errVal)) # the indices of the error values
			errValZero[indErr] <- errVal[indErr] # is a vector of numbers with mostly zeroes, sometimes a number indicating the correction value
			errValUse <- errValUse + errValZero # add the intended nr of consecutive scans. So it is a vector undulating around the nrConScans.
			remErrCol <- TRUE
			#
			len <- length(indErr)
			cha1 <- "s"; cha2 <- ""
			if (len == 1) {cha1 <- ""; cha2 <- "s"} 
			if (all(is.na(errVal))) {
				msgTxt <- "but no abberation in number of consecutive scans.\n"
			} else {
				msgTxt <- paste0(len, " sample", cha1, " show", cha2, " aberrant number of consecutive scans.\n")
			} # end else
			if (!stn$allSilent) {
				cat(paste0("       Error column present, ", msgTxt))
			} # end if
			#	
		} # end if errCol present
		
		###
		aaa <-  sampleList[rep(seq_len(nrow(sampleList)), errValUse), ] # multiply each line by the value in errValUse
		###
		if (remErrCol) {
			kk <- which(cns == errCol)
			aaa <- aaa[, -kk] # remove the error column
		} # end if
		rl <- rle(aaa[,sampleNrCol]) #  
		ConSNr <- NULL
		for (i in seq_along(rl$lengths)){
		  ConSNr <- c(ConSNr, 1:rl$lengths[i]) # generate vector for consecutive scans
		} # end for i 
		if (ncol(aaa) > 1) {
			aaa <- data.frame(aaa[1], ConSNr=ConSNr, aaa[-1]) # insert into data frame
		} else {
			aaa <- data.frame(aaa[1], ConSNr=ConSNr) # insert into data frame		
		}
		ColNames <- colnames(aaa)
		ColNames[2] <- paste(stn$p_yVarPref, stn$p_conSNrCol, sep="")
		colnames(aaa) <- ColNames
		rownames(aaa) <- seq(1:nrow(aaa)) # correct all the rownames
		outList <- aaa	
	} # end if doMultiply
	out <- list(sampleList=outList, isMult=doMultiply) # easy. Just leave it as it is	
	return(out)
} # EOF

readHeader_checkDefaults <- function(slType, possibleValues, md, multiplyRows) {
	# possibleValues is only "xls" any more
	stn <- getstn()
	if (!is.null(slType)) {
		if (all(slType == "def")) {
			slType <- stn$imp_sampleListType # can be only NULL or xlsx
		}
		if (!all(slType == possibleValues) | length(slType) != 1) {
			stop(paste0("Please provide either '", paste(possibleValues, collapse=", "),"' or 'NULL' to the argument 'slType'."), call.=FALSE)
		}
	} # end if
	assign("slType", slType, pos=parent.frame(n=1))
	assign("slType", slType, pos=parent.frame(n=2)) # that is needed to always have the correct value for slType in the getFullData function
	assign(".slType", slType, pos=gl_ap2GD)
	###
	if (is(md, "aquap_md")) {
		filename <- md$meta$expName
	} else {
		stop("Please provide a valid metadata object of class 'aquap_md' to the argument 'md'", call.=FALSE)
	}
	assign("filename", filename, pos=parent.frame(n=1))
	###
	if (all(multiplyRows == "def")) {
		multiplyRows <- stn$imp_multiplyRows
	} else {
		if (!all(multiplyRows == "auto")) {
			if (!all(is.logical(multiplyRows)) | length(multiplyRows) != 1) {
				stop("Please provide either 'TRUE', 'FALSE' or 'auto' to the argument 'multiplyRows'.", call.=FALSE)
			} # end if		
		} # end if
	} # end else
	assign("multiplyRows", multiplyRows, pos=parent.frame(n=1))
	assign("multiplyRows", multiplyRows, pos=parent.frame(n=2))  # that is needed to always have the correct value for multiplyRows in the getFullData function
} # EOF

check_sl_existence <- function(filename, ext) {
	stn <- getstn()
	slInFolder <- paste(stn$fn_sampleLists, "/", stn$fn_sampleListIn, sep="")
	fn <- paste(filename, ext, sep="")
	a <- paste(slInFolder, "/", fn, sep="")
	if (!file.exists(a)) {
		stop(paste("The sample-list file \"", fn, "\" does not seem to exist in \"", slInFolder, "\".", sep=""), call.=FALSE)
	}	
} # EOF

check_conScanColumn <- function(header, headerFilePath, spectraFilePath, slType, filetype) {
	stn <- getstn()
	custImp <- grepl("custom@", filetype)
	a <- paste(stn$p_yVarPref, stn$p_conSNrCol, sep="")
	lookHelp <- ""
	if (!any(a %in% colnames(header))) {
		if (is.null(slType)) {
			from <- paste(" in the file \n'", spectraFilePath, "' containing the spectral data.", sep="")
			if (custImp) {
				whereCust <-  " and modify the custom import function accordingly.\n"
				lookHelp <- "\nSee the help for 'custom_import' for further information."
			} else {
				whereCust <- "."
				lookHelp <- ""
			}
			where <- paste(" to the file containing the spectral data", whereCust, sep="")
			maybe <- ""
		} else {
			from <- paste(", neither in your sample-list file \n'", headerFilePath, "', nor in the file \n'", spectraFilePath, "' containing the spectral data.", sep="")
			where <- paste(" either to the sample list or to the file containing the spectral data. In the latter case please modify the custom import function accordingly.\n")
 			maybe <- "(You maybe encounter this error because you did choose to *not* multiply the rows of the sample list by the nr. of consecutive scans, in which case a column for the nr. of cons. scan gets inserted automatically.)\n"
		}
		msg <- paste("You do not have a column for the nr of the consecutive scan", from, " \nPlease, add a column called '", a, "'", where, maybe, lookHelp, sep="")
		stop(msg, call.=FALSE)
	}	
} # EOF

check_sampleNumberColumn <- function(header) {
	stn <- getstn()
	yPref <- stn$p_yVarPref
	sampleNrCol <- paste0(yPref, stn$p_sampleNrCol)
	if (!sampleNrCol %in% colnames(header)) {
		stop(paste0("Sorry, but there has to be a column called '", sampleNrCol, "' containing the sample number in the sample list."), call.=FALSE)
	} # end if
} # EOF

#' @title Read Header
#' @description This functions reads in the sample-list file and prepares the 
#' header to be fused with the spectral data. 
#' Usually you do not need to call this function.
#' @details  From the metadata, provided in the first argument, the experiment 
#' name is extracted; the sample list (what is used to create the header) must be in the 
#' sampleLists/sl_in folder and must be named with the experiment name, followed 
#' by a "-in" and then the file extension.
#' @inheritParams getFullData
#' @seealso \code{\link{getFullData}}
#' @examples
#' \dontrun{
#'  header <- readHeader()
#' }
#' @family Development Functions
#' @export
readHeader <- function(md=getmd(), slType="def", multiplyRows="def") {
	stn <- autoUpS()
	poss_sl_types <- pv_sampleListType # only xls
	# 		
	filename <- NULL # will be changed in the checking
	readHeader_checkDefaults(slType, poss_sl_types, md, multiplyRows) # is re-assigning multiplyRows
	slInFolder <- paste(stn$fn_sampleLists, "/", stn$fn_sampleListIn, "/", sep="")
	if (is.null(slType)) {
		return(NULL)
	}
# 	if (slType == "csv") {
# 		ext <- "-in.csv"
# 		check_sl_existence(filename, ext)
# 		rawHead <- read.csv(paste(slInFolder, filename, ext, sep=""))
# 	}
# 	if (slType == "txt") {
# 		ext <- "-in.txt"
# 		check_sl_existence(filename, ext)
# 		rawHead <- read.table(paste(slInFolder, filename, ext, sep=""), header=TRUE)
# 	}
	#
	ext <- "-in.xlsx"
	check_sl_existence(filename, ext)
	assign("headerFilePath", paste(slInFolder, filename, ext, sep=""), pos=parent.frame(n=1))
	##
	rawHead <- openxlsx::read.xlsx(paste(slInFolder, filename, ext, sep="")) # ! character imports are NOT factors yet !!
	check_sampleNumberColumn(rawHead)
	rawHead <- convertCColsToFactor(rawHead)	
	rawHead <- convertYColsToNumeric(rawHead)
	###
	nrScans <- md$postProc$nrConScans
	header <- gfd_possibly_multiplySampleListRows(rawHead, nrScans, multiplyRows)$sampleList
	return(header)
} # EOF


#  assigns all the necessary list elements except NIR, info and timestamp and in the frame where it is called
#' @title Search and Ask for Column Representation
#' @description This function can be used in a custom import function. All 
#' required list elements except 'timestamp', 'info' and 'NIR' will be assigned in 
#' the environment from where this function was called.
#' @details This function checks if any of the in the settings.r file defined 
#' standard column names are present in the provided class-variables and numerical 
#' variables. If no exact match is found, the function asks which one of the 
#' provided columns is representing each of the standard columns. Those columns 
#' that do not get assigned to one of the standard columns will be imported under 
#' their respective name with the corresponding prefix for class- or numerical 
#' variables.
#' @param allC_var A data frame containing only class variables.
#' @param allY_var A data frame containing only numerical variables.
#' @param slType What type of sample list is used during the import. If no 
#' additional sample list is used (slType = 'NULL'), the function stops if no 
#' column for the nr. of consec. scan is present or gets assigned. You can get 
#' the current slType with 'get(".slType", pos=gl_ap2GD)' - see example.
#' @param oT Logical. Only used for testing. Leave at the default \code{FALSE}.
#' @return All the list elements needed in the \code{\link{custom_import}} function 
#' except 'timestamp', 'info' and 'NIR get assigned in the environment from where 
#' this function was called.
#' @family Development Functions
#' @examples
#' \dontrun{
#' # this could be the content of the .r file for defining a custom function, 
#' # in this example for an import from an Excel-file.
#' fileExtension <- ".xlsx"
#' ##
#' spectralImport <- function(dataFile) {
#'    require(openxlsx)
#'    import <- openxlsx::read.xlsx(dataFile)
#'    #
#'    allY_var <- import[,c(1,2,10,11,6)]
#'    allC_var <- import[, c(3,4,8,9,5,7)] 
#'    info <- list(nCharPrevWl = 2)
#'    NIR <- as.matrix(import[, 12:18])
#'    rownames(NIR) <- paste("S", 1:nrow(NIR), sep="")
#'    timestamp <- NULL
#'    imp_searchAskColumns(allC_var, allY_var, get(".slType", pos=gl_ap2GD)) 
#'    # assigns all list elements except timestamp, info and NIR
#'    #
#'    return(list(sampleNr=sampleNr, conSNr=conSNr, timePoints=timePoints, 
#'    ecrm=ecrm, repl=repl, group=group, temp=temp, relHum=relHum, C_cols=C_cols, 
#'    Y_cols=Y_cols, timestamp=timestamp, info=info, NIR=NIR))
#'    #
#'  } # EOF
#' }
#' @export
imp_searchAskColumns <- function(allC_var, allY_var, slType=get(".slType", pos=gl_ap2GD), oT=FALSE) {
	stn <- autoUpS()
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
	## XXXVARXXX
	listElementNames_C <- c("timePoints", "ecrm", "repl", "group")
	stdColumnNames_C <- c(paste(cPref, timePointsColn, sep=""), paste(cPref, ecrmColn, sep=""), paste(cPref, replColn, sep=""), paste(cPref, groupColn, sep=""))
	listElementNames_Y <- c("sampleNr", "conSNr", "temp", "relHum")
	stdColumnNames_Y <- c(paste(yPref, sampleNrColn, sep=""), paste(yPref, conSNrColn, sep=""), paste(yPref, tempColn, sep=""), paste(yPref, relHumColn, sep=""))
	listElementNames <- list(listElementNames_C, listElementNames_Y)
	stdColumnNames <- list(stdColumnNames_C, stdColumnNames_Y)
	allVars <- list(allC_var, allY_var)
	listElementNamesRestCols <- list("C_cols", "Y_cols")
	##
	stopMsg <- "Import aborted."
	stopDoubleMsg1 <- "The standard column name \""
	stopDoubleMsg2 <- "\" appears more than once in the input file. Please check the column names. \n"
	msg1 <- "\n\nThe standard column name \""
	msg2 <-  "\" could not be found in the input-file. \nWhich of the following columns does represent "
	msg3 <- "?\nPlease enter the appropriate number; type 0 for not represented; type any non-numeric to stop.\n"
	msg2_zero <- "\" could not be found in the input-file. \nPlease confirm by typing '0' that "
	msg3_zero <- " is not represented in this data set; type any non-numeric to stop.\n"
	notRep <- " 0 -- not represented \n "
	##
	checkNumber <- function(nr, nCols, tol = .Machine$double.eps^0.5) {
   		if (is.na(nr)) { stop(stopMsg, call.=FALSE) }
	    isInteger <- (abs(nr - round(nr)) < tol)
  		if (nr > nCols | nr < 0 | !isInteger) { 
  			message("Please enter a positive integer in the range shown above.\n")
  			return(FALSE)
  		} else {
  			return(TRUE)
  		}
	} # EOIF
	##
	
	
	for (k in 1: length(listElementNames)) { # to go first through the C-variables, then through the Y-variables
		for (i in 1: length(listElementNames[[k]])) { # go through the single element name
	#		print(ht(allVars[[k]])); cat("\n--------------------------------------------------------------------\n")
			cn <- stdColumnNames[[k]][i]
			ind <- which(colnames(allVars[[k]]) == cn )
			cle <- length(colnames(allVars[[k]]))
			if (length(ind) == 0) {
				if (cle > 0) {
					if (!oT) { cat(paste(msg1, cn, msg2, cn, msg3, sep="")) }
				} else {
					if (!oT) { cat(paste(msg1, cn, msg2_zero, cn, msg3_zero, sep="")) }
				}
				if (!oT) { cat(notRep) }
				if (cle > 0) {
					if (!oT) { cat(paste(1:cle, " -- ", colnames(allVars[[k]]), "\n", sep="")) }
				}
				nrOk <- FALSE
				while(!nrOk) {
					if (!oT) {
						a <- readLines(n=1)
						options(warn=-1); a <- as.numeric(a); options(warn=0)
					} else {
						a <- 0
					} # end else
						nrOk <- checkNumber(a, cle)					
				} # end while
				if (a == 0) { # user chooses to set to NULL
					if (stdColumnNames[[k]][i] == paste(yPref, conSNrColn, sep="") & is.null(slType) ) { # so we choose to *not* assigne the cons. scan column and we do *not* have a sample list imported
						stop(paste("You need a column for the consec. scan in your dataset. Please, add a column called \"", yPref, conSNrColn, "\" to the file containing the spectral data.", sep=""), call.=FALSE)
					}
					assign(listElementNames[[k]][i], NULL, pos=parent.frame(n=1))
				} else { # so we have (checked) a valid input giving a representation
					assign(listElementNames[[k]][i], as.data.frame(allVars[[k]][, a]), pos=parent.frame(n=1))
					cnsPrev <- colnames(allVars[[k]])
					allVars[[k]] <- as.data.frame(allVars[[k]][,-a])
					colnames(allVars[[k]]) <-cnsPrev[-a]
				}
			} else { # so we did find something
				if (length(ind) > 1 ) { # more than one
					stop(paste(stopDoubleMsg1, cn, stopDoubleMsg2, sep=""), call.=FALSE)
				} # if still here the index == 1, that means we found exactly one match in the column names
				assign(listElementNames[[k]][i], as.data.frame(allVars[[k]][, ind]), pos=parent.frame(n=1))
				cnsPrev <- colnames(allVars[[k]])
				allVars[[k]] <- as.data.frame(allVars[[k]][,-ind])
				colnames(allVars[[k]]) <-cnsPrev[-ind]
			} # end else 
		} # end for i
		if (ncol(allVars[[k]]) == 0) {
			assign(listElementNamesRestCols[[k]], NULL, pos=parent.frame(n=1))
		} else {
			assign(listElementNamesRestCols[[k]], as.data.frame(allVars[[k]]), pos=parent.frame(n=1))			
		}
	} # end for k
} # EOF

## maybe add the user-function for re-making the T and RH classes
# XXX

## note: make @numRep in Aquacalc to take numerics OR character, because if we have more than 8 elements...  :-)

