# read spectra ----------------------------------------------------------------------
readSpec_checkDefaults <- function(possibleFiletypes, md, filetype, naString) {
	if (all(filetype == "def")) {
		filetype <- .ap2$stn$imp_specFileType
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
		pathSH <- Sys.getenv("AQUAP2SH")
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
	if (class(md) == "aquap_md") {
		filename <- md$meta$expName
	} else {
		stop("Please provide a valid metadata object to the argument 'md'", call.=FALSE)
	}
	rawFolder <- .ap2$stn$fn_rawdata
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
readSpectra <- function(md=getmd(), filetype="def", naString="NA") {
	autoUpS()
	possibleFiletypes <- pv_filetypes #global constant, they get handed down to the checking function !  	# 	pv_filetypes <- c("vision_NSAS.da", "tabDelim.txt", "Pirouette.pir", "xlsx")
	filename <- NULL # will be changed in the checking
	readSpec_checkDefaults(possibleFiletypes, md, filetype, naString)
	rawFolder <- .ap2$stn$fn_rawdata
	folderFile <- paste(rawFolder, "/", filename, sep="")
	##
	if (filetype == possibleFiletypes[1]) {
		a <- paste(folderFile, ".da", sep="")
		assign("spectraFilePath", a, pos=parent.frame(n=1))
		return(getNIRData_Vision_da(a))
	}
	##
	if (filetype == possibleFiletypes[2]) {
		a <- paste(folderFile, ".txt", sep="")
		assign("spectraFilePath", a, pos=parent.frame(n=1))
 		return(getNirData_plainText(a, naString))
	}
	if (filetype == possibleFiletypes[3]) {
		a <- paste(folderFile, ".pir", sep="")
		assign("spectraFilePath", a, pos=parent.frame(n=1))
 		return(getNIRData_Pirouette(a))
	} 
	if (filetype == possibleFiletypes[4]) {
		a <- paste(folderFile, ".xlsx", sep="")
		assign("spectraFilePath", a, pos=parent.frame(n=1))
 		return(getNirData_Excel(a))
	} 
	## if nothing of the above happend, then we must have (checked!) the path to a valid custom .r file in "filetype" 
	custName <- strsplit(filetype, "custom@")[[1]][2]
	pathSH <- Sys.getenv("AQUAP2SH")
	pathToCustom <- paste(pathSH, custName, sep="/")
	e <- new.env()
	sys.source(pathToCustom, envir=e)
	a <- paste(folderFile, e$fileExtension, sep="")
	assign("spectraFilePath", a, pos=parent.frame(n=1))
	return(e$spectralImport(a))
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
	}
	a <- specImp$info$nCharPrevWl
	if (!all(is.numeric(a)) | length(a) !=1) {
		stop("Please provide a length one numeric as the input for the element 'nCharPrevWl' in the import function", call.=FALSE)
	}
	ncpwl <- specImp$info$nCharPrevWl
	options(warn = -1)
	a <- as.numeric(substr(colnames(specImp$NIR), ncpwl+1, nchar(colnames(specImp$NIR))))
	options(warn = 0)
	if (any(is.na(a))) {
		stop("There is an error with the column names of the NIR spectra representing the wavelength. \nMaybe not all columns do have the same number of characters before the wavelength, or some wrong columns are assigned to the NIR columns. \nPlease check in the import function if the provided number of characters before the wavelength is correct, and if the column names of the NIR data are correct as well.", call.=FALSE)
	}
} # EOF

gfd_makeNiceColumns <- function(specImp) {
	yPref <- .ap2$stn$p_yVarPref
	cPref <- .ap2$stn$p_ClassVarPref
	sampleNrColn <- .ap2$stn$p_sampleNrCol
	conSNrColn <- .ap2$stn$p_conSNrCol
	timePointsColn <- .ap2$stn$p_timeCol
	ecrmColn <- .ap2$stn$p_ECRMCol
	replColn <- .ap2$stn$p_replicateCol
	groupColn <- .ap2$stn$p_groupCol
	tempColn <- .ap2$stn$p_tempCol
	relHumColn <- .ap2$stn$p_RHCol
	absTime <- .ap2$stn$p_absTime
	chron <- .ap2$stn$p_chron

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
		}
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
		if (.ap2$stn$imp_makeTimeDistanceColumn) {
			startDate <- as.POSIXct(.ap2$stn$imp_startDate)
			startDateNr <- as.double(startDate)
			a <- unclass(specImp$timestamp[,1])
			MinuteTimStamp <- as.numeric(round((a - startDateNr)/60, 2))
			chrons <- data.frame(absTime=MinuteTimStamp, chron=1:length(MinuteTimStamp))
			timestamp <- specImp$timestamp
			specImp$timestamp <- cbind(specImp$timestamp, chrons)
			colnames(specImp$timestamp) <- c("Timestamp", paste0(yPref, absTime), paste0(yPref, chron))
		} else {
			colnames(specImp$timestamp) <- "Timestamp"		
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
				to <- paste("after the multiplication of every row by ", nrConScans, " consecutive scans", sep="")
			} else {
				to <- "(multiplication of rows was *not* performed)"
			}
			stop(paste("The header that was constructed from the file \n\"", headerFilePath, "\"\n consists of ", nrow(header), " rows ", to, ", while the imported spectra from file \n\"", spectraFilePath, "\"\n consist of ", nrowsNIR, " rows. \nPlease make sure that all data to be imported have the same number of rows.", sep=""), call.=FALSE)
		}
	}
} # EOF

gfd_getExpNameNoSplit <- function(metadata, nRows) {
	cPref <- .ap2$stn$p_ClassVarPref
	makeExpNameColumn <- .ap2$stn$imp_makeExpNameColumn
#	makeNoSplitColumn <- .ap2$stn$imp_makeNoSplitColumn
	makeNoSplitColumn <- TRUE
	##
	if (makeExpNameColumn) {
		expName <- data.frame(rep(metadata$meta$expName, nRows))
		colnames(expName) <- paste(cPref, .ap2$stn$p_expNameCol, sep="")
	} else {
		expName <- data.frame(DELETE = rep(NA, nRows))
	}
	assign("expName", expName, pos=parent.frame(n=1))
	##
	if (makeNoSplitColumn) {
		noSplit <- data.frame(rep(.ap2$stn$p_commonNoSplit, nRows))
		colnames(noSplit) <- paste(cPref, .ap2$stn$p_commonNoSplitCol, sep="")
	} else {
		noSplit <- data.frame(DELETE = rep(NA, nRows))	
	}
	assign("noSplit", noSplit, pos=parent.frame(n=1))
} # EOF

gfd_checkForDoubleColumns <- function(header, spectraFilePath, headerFilePath, slType) {
	collect  <-  NULL
	patterns <- paste(".", 1:12, sep="")
	a <- colnames(header)
	for (k in 1: length(patterns)) {	
		inds <- grep(patterns[k], a)
		if (length(inds) > 0) {
			collect <- c(collect, inds)
		}
	}  # end for k
	if (!is.null(collect)) {
		if (is.null(slType)) {
			files <- paste("\"", spectraFilePath, "\"", sep="")		
		} else {
 			files <- paste("\"", headerFilePath, "\" and ", "\"", spectraFilePath, "\".", sep="")
		}
		cols <- paste(a[collect], collapse=", ")
		msg <- paste("Some columns seem to appear twice: \n", cols,"\nPlease check the files used for importing data, that is \n", files, sep="")
		stop(msg, call.=FALSE)
	}
} # EOF

gfd_importData <- function() {


} # EOF

gfd_checkLoadSaveLogic <- function(ttl, stf) {
	if (!is.logical(ttl) | !is.logical(stf)) {
		stop("Please provide 'TRUE' or 'FALSE' to the arguments 'ttl' / 'stf' ", call.=FALSE)
	}
} # EOF

gfd_checkMetadata <- function(md) {
	if (class(md) != "aquap_md") {
		stop("Please provide a valid metadata object of class 'aquap_md' to the argument 'md'", call.=FALSE)
	}
} # EOF

gfd_TRH_checkCustomImport <- function(trhImp) {
	possVals <- c("Time", "Temp", "RelHum")
	if (class(trhImp) != "data.frame") {
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
	rawFolder <- .ap2$stn$fn_rawdata
	logfileName <- .ap2$stn$imp_TRH_logfile_name
	if (trhLog == "ESPEC") {
		ext <- ".txt"
		path <- paste(rawFolder, "/", logfileName, ext, sep="")
		TRHimp <- importTRH_ESPEC(path)
	}	
	if (grepl("custom@", trhLog)) {
		custName <- strsplit(trhLog, "custom@")[[1]][2]
		pathSH <- Sys.getenv("AQUAP2SH")
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
	narrowMinutes <-.ap2$stn$imp_narrowMinutes
	secsNarrowPrecision <-.ap2$stn$imp_secsNarrowPrecision
	mtp <-.ap2$stn$imp_minutesTotalPrecision
	diffT <- abs(difftime(TRHlog$Time[1], TRHlog$Time[2], units="secs"))
	TIndUpRange <- round((narrowMinutes*60)/as.double(diffT), 0) ## for a narrowed-down area of narrowMinutes
	rowNr <- rep(NA, length(timeDataset))
	if (min(abs(difftime(TRHlog$Time, timeDataset[1], units="secs"))) > mtp*60) {
		stop(paste("The first spectra aquisition time seems to be more than", mtp, "minutes separated from any data in the time-log. The import of the  data-file has been aborted. Sorry."), call.= FALSE)
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
	a <- which(colnames(header) == "Timestamp")
	if (length(a) == 0 & trhLog != FALSE) {
		message(paste("There is no 'Timestamp' column in your dataset to which data from a logger could be aligned. \n(Alignment is controlled by argument 'trhLog'.) "))
		message("Importing continues without the alignment of log data.")
	}
	if (length(a) !=0 ) {	 # so only do all this if we actually have a timestamp in the columns
		tempCol <- paste(.ap2$stn$p_yVarPref, .ap2$stn$p_tempCol, sep="")
		rhCol <- paste(.ap2$stn$p_yVarPref, .ap2$stn$p_RHCol, sep="")
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
		if (!.ap2$stn$allSilent) {cat("   Aligning temp. and rel.hum. values to timestamp...\n") }
		TRH_alig <- alignTempRelHum(header$Timestamp, TRHimp)
		if (!.ap2$stn$allSilent) {cat("   Done.\n") }
#			tempF <- data.frame(SpectTime= header$Timestamp, LogTime = TRH_alig$Time, Temp=TRH_alig$Temp, Hum=TRH_alig$RelHum)
#			print(tempF[1:20,]); wait()
		header[tInd] <- TRH_alig$Temp
		header[rhInd] <- TRH_alig$RelHum 
	} # end checking if we have a timestamp column
	return(header)
} # EOF

gfd_check_trhLog_defaults <- function(trhLog) {
	# can be: FALSE, 'def' "ESPEC", "custom@name.r"
	filename <- .ap2$stn$imp_TRH_logfile_name
	rawFolder <- .ap2$stn$fn_rawdata
	possibleValues <- c("ESPEC") ## XXXVARXXX
	stopMsg <- "Please refer to the help for 'getFullData' to find out about the possible values for the argument 'trhLog'"
	if (trhLog == TRUE) {
		stop(stopMsg, call.=FALSE)
	}
	if (!is.logical(trhLog)) {
		if (all(trhLog == "def")) {
			trhLog <- .ap2$stn$imp_use_TRH_logfile
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
		}
	}
	if (bcust) {
		shPath <- Sys.getenv("AQUAP2SH")
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
	# first check the content of detectOutliers
	if (all(detectOutliers == "def") & !is.null(detectOutliers)) {
		detectOutliers <- .ap2$stn$imp_flagOutliers
	}
	if (!all(is.logical(detectOutliers)) | length(detectOutliers) > 1) {
		stop("Please provide either TRUE or FALSE (or 'def') to the argument 'dol'.", call.=FALSE)
	} # so now we are sure to have either TRUE or FALSE in detectOutliers
	##
	if (!detectOutliers) {
		return(data.frame(DELETE = rep(NA, nrow(NIR))))
	} else {
		cPref <- .ap2$stn$p_ClassVarPref
		cnOtl <- paste0(.ap2$stn$p_outlierCol, .ap2$stn$p_outlierCol_allSuffix)
		tol <- .ap2$stn$simca_tolerance
		kmax <- .ap2$stn$simca_kMax
		flatDf <- data.frame(grouping=rep("x", nrow(NIR)))
		flatDf <- cbind(flatDf, as.data.frame(NIR))
#		kmax <- 10
		if (!.ap2$stn$allSilent) {cat("   detecting outliers... ")}
		simcaMod <- rrcovHD::RSimca(grouping ~ ., data=flatDf, kmax=kmax, tol=tol)  ## k=0 does not work ??, but still calculating k
		flags <- as.factor(!simcaMod@flag) # to invert them and tranform to factor, having TRUE for the outliers
		nrOutliers <- length(which(flags == TRUE))
		usedK <- simcaMod@k
		if (nrOutliers == 0) {
			msg <- "none found. "
		} else {
			msg <- paste("found *", nrOutliers, "*. [Using ", usedK, " components.] ", sep="")
		}
		if (!.ap2$stn$allSilent) {cat(paste(msg, "\n", sep=""))}
#		print(str(simcaMod))
		outlierFlags <- data.frame(flags)
		colnames(outlierFlags) <- paste(cPref, cnOtl, sep="")
		return(outlierFlags)
	} # end else
} # EOF

checkDatasetVersion <- function(dataset, dsName=NULL) {
	if (.ap2$stn$gen_versionCheckDataset) {
		if (dataset@version != pv_versionDataset) {
			if (is.null(dsName)) {
				dsName <- deparse(substitute(dataset))
			}
			stop(paste("The dataset '", dsName, "' was created with an older version of package 'aquap2' and so has a different structure than what is required now.\nPlease re-import the raw-data.\n(You can switch off the checking of the dataset-version in the settings-file at the parameter 'gen_versionCheckDataset'.)", sep=""), call.=FALSE)
		}
	}
} # EOF

checkForPresenceOfData <- function() {
	rawFolder <- .ap2$stn$fn_rawdata
	aa <- list.files(rawFolder)
	if (length(aa) == 0) {
		stop(paste0("Sorry, it appears there are no data in the current '", rawFolder, "' folder."), call.=FALSE)
	}
} # EOF

turnStringsIntoFactors <- function(header) {
	for (i in 1: ncol(header)) {
		if (is.character(header[,i])) {
			header[,i] <- as.factor(header[,i]) # 
		} # end if
	} # end for i
	return(header)
} # EOF

# get full data ---------------------------------------------------------------
#' @template mr_getFullData
#' @export
getFullData <- function(md=getmd(), filetype="def", naString="NA", slType="def", trhLog="def", multiplyRows="def", ttl=TRUE, stf=TRUE, dol="def") {
	autoUpS()
	gfd_checkLoadSaveLogic(ttl, stf)
	gfd_checkMetadata(md)
	dataset <- NULL
	if (ttl) {
		dataset <- loadAQdata(md, verbose=FALSE)
	}
	if(!is.null(dataset)) { # so the path existed and it could be loaded
		checkDatasetVersion(dataset, md$meta$expName)
		if(!.ap2$stn$allSilent) {cat(paste("Dataset \"", md$meta$expName, "\" was loaded.\n", sep="")) }
		return(invisible(dataset)) # returns the dataset and we exit here
	}
  	# import starts 
  	checkForPresenceOfData()
  	if(!.ap2$stn$allSilent) {cat("Importing data...\n")}	
	gfd_check_trhLog_defaults(trhLog)
	headerFilePath <- NULL # gets assigned in readHeader()
	header <- readHeader(md, slType, multiplyRows) ## re-assigns 'slType' and 'multiplyRows' also here -- in parent 2 level frame 
													## if slType is NULL, header will be returned as NULL as well
	spectraFilePath <- NULL # gets assigned in readSpectra()
	si <-  readSpectra(md, filetype, naString) ### !!!!!!!!! here the import !!!!!!!!!
	gfd_check_imports(si) # makes sure eveything is NULL or data.frame / matrix (NIR)
	si <- gfd_makeNiceColumns(si) # makes all column names, transforms Y-variables to numeric
	nr <- nrow(si$NIR)
	gfd_checkNrOfRows(header, headerFilePath, nr, spectraFilePath, multiplyRows, nrConScans=md$postProc$nrConScans)  # makes sure spectra and sample list have same number of rows
	if (is.null(header)) {
		header <- data.frame(DELETE=rep(NA, nr))
	}
	expName <- noSplit <- NULL # gets assigned below in gfd_getExpNameNoSplit()
	gfd_getExpNameNoSplit(metadata=md, nRows=nr)
	NIR <- si$NIR
	outliers <- flagOutliers_allScope(NIR, detectOutliers=dol) # the value of dol is checked / changeed inside this function
	headerFusion <- cbind(expName, noSplit, header, si$sampleNr, si$conSNr, si$timePoints, si$ecrm, si$repl, si$group, si$C_cols, si$Y_cols, outliers, si$temp, si$relHum, si$timestamp, stringsAsFactors=TRUE) # stringsAsFactors problem with the changed defaults in R 4.x --> but does not help here !!
	headerFusion <- headerFusion[, -(which(colnames(headerFusion) == "DELETE"))] 		
	headerFusion <- turnStringsIntoFactors(headerFusion) # see 2 lines above
	check_conScanColumn(headerFusion, headerFilePath, spectraFilePath, slType, filetype) 
	# ? check for existence of sample number column as well ?
	gfd_checkForDoubleColumns(headerFusion, spectraFilePath, headerFilePath, slType)
	headerFusion <- gfd_importMakeTRH_alignment(headerFusion, trhLog)
	if (.ap2$stn$imp_autoCopyYvarsAsClass) {  # if TRUE, copy all columns containing a Y-variable as class variable
		headerFusion <- copyYColsAsClass(headerFusion)
	}
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
	fullData@anproc <- NULL # the ap not yet here of course
	fullData@ncpwl <- si$info$nCharPrevWl
	if (stf) {
		saveAQdata(fullData, md, verbose=TRUE)
	} else {
		if(!.ap2$stn$allSilent) {cat("Done. (not saved) \n")}	
	}
	return(invisible(fullData))
} # EOF

#' @rdname getFullData
#' @export
gfd <- function(md=getmd(), filetype="def", naString="NA", slType="def", trhLog="def", multiplyRows="def", ttl=TRUE, stf=TRUE, dol="def") {
	return(getFullData(md, filetype, naString, slType, trhLog, multiplyRows, ttl, stf, dol))
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
	autoUpS()
  	if (class(dataset) != "aquap_data") {
    	stop("Please provide an object of class 'aquap_data' to the argument 'dataset'", call.=FALSE)
  	}
  	expName <- md$meta$expName
	path <- paste(.ap2$stn$fn_rdata, expName, sep="/")
	save(dataset, file=path)
	if (verbose & !.ap2$stn$allSilent) {
		cat(paste("Dataset saved under \"", path, "\".\n", sep=""))
	}
} # EOF

#' @rdname saveAQdata
#' @export
loadAQdata <- function(md=getmd(), verbose=TRUE) {
	autoUpS()
  	expName <- md$meta$expName
	path <- paste(.ap2$stn$fn_rdata, expName, sep="/")
	if (file.exists(path)){
		a <- load(path)
		if (verbose & !.ap2$stn$allSilent) {
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

extractClassesForColRep_old <- function(header) { ## does not need "NIR" present in the data frame
	tempCol <- .ap2$stn$p_tempCol ## depends on "grepl" or not
	RHCol <- .ap2$stn$p_RHCol
	absTimeCol <- .ap2$stn$p_absTime
	chronCol <- .ap2$stn$p_chron
	TRHColors <- .ap2$stn$col_RampForTRH
	TimesColors <- .ap2$stn$col_RampForTimes
	userRampColors <- .ap2$stn$col_userDefinedRamps # is a list
	userColnames <- .ap2$stn$p_userDefinedSpecialColnames # is a vector
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
				a[1] <- generateHeatMapColorCoding(TRHColors, a[,1])
			}
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
				}
			} # end else
			names(a) <- colnames(header[i])
			out <- data.frame(out,a)
		} # end if is factor 
	} # end for i
	return(out[-1])		# cut off the first column containing only the NAs
} # EOF 

extractClassesForColRep <- function(header) { ## does not need "NIR" present in the data frame
	tempCol <- .ap2$stn$p_tempCol ## depends on "grepl" or not
	RHCol <- .ap2$stn$p_RHCol
	absTimeCol <- .ap2$stn$p_absTime
	chronCol <- .ap2$stn$p_chron
	TRHColors <- .ap2$stn$col_RampForTRH
	TimesColors <- .ap2$stn$col_RampForTimes
	userRampColors <- .ap2$stn$col_userDefinedRamps # is a list
	userColnames <- .ap2$stn$p_userDefinedSpecialColnames # is a vector
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
	yPref <- .ap2$stn$p_yVarPref
	cPref <- .ap2$stn$p_ClassVarPref
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
	}
	add <- add[-1]
	return(data.frame(sampleList, add))
} # EOF

remakeTRHClasses_sys <- function(headerOnly, TDiv=.ap2$stn$imp_TClassesDiv, TRound=.ap2$stn$imp_TRounding, RHDiv=.ap2$stn$imp_RHClassesDiv, RHRound= .ap2$stn$imp_RHRounding) {
	options(warn=-1)
	cPref <- .ap2$stn$p_ClassVarPref
	yPref <- .ap2$stn$p_yVarPref
	YTemp <- paste(yPref, .ap2$stn$p_tempCol, sep="")
	YRH <- paste(yPref, .ap2$stn$p_RHCol, sep="")
	Tpat <- paste(cPref, .ap2$stn$p_tempCol, sep="") 						# read in the prefix for class and temperature from settings
	RHpat <- paste(cPref, .ap2$stn$p_RHCol, sep="")							# read in the prefix for class and rel. humidity from settings
	alwaysReduceClasses <- .ap2$stn$imp_alwaysReduceTRHClasses												
	
#	TInd <- grep(.ap2$stn$p_tempCol, colnames(headerOnly), fixed=TRUE)		# find column-index that has the temperatur - the source
	TInd <- which(colnames(headerOnly) == YTemp)			 				# find column-index that has the temperatur - the source
	if (length(TInd) > 0) {
#		TClInd  <- grep(Tpat, colnames(headerOnly), fixed=TRUE)					# find column-index that the temperatur already as class - the target
		TClInd  <- which(colnames(headerOnly) == Tpat) 						# find column-index that the temperatur already as class - the target
		numsTemp <- headerOnly[, TInd[1] ]										# extract the numbers
		if (any(countDecimals(numsTemp) > 1) | alwaysReduceClasses) {			# to avoid reducing the number of classes when there is only one comma value
			headerOnly[TClInd] <- factor(round((numsTemp/TDiv),TRound)*TDiv)		# insert the new classes
		}
	}
	##
#	RHInd <- grep(.ap2$stn$p_RHCol, colnames(headerOnly), fixed=TRUE)		# find column-index that has the rel. hum.
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
	options(warn=-1)
	pref <- .ap2$stn$p_yVarPref
	ind  <- grep(pref, colnames(sampleList))
	if (length(ind) > 0) {
		for (i in 1: length(ind)) {
			sampleList[ind[i]] <- as.numeric( as.character(sampleList[, ind[i]]) )
		}
	}	
	options(warn=0)
	return(sampleList)
} # EOF

### x-fold every line (Nr. Cons. Measurements), and add a numbering for each of the consecutive scans
multiplySampleListRows <- function(sampleList, nrScans) {
	multiList <- NULL
#	sampleList <- as.data.frame(sampleList)
	for (i in 1:nrScans) {
		multiList <- rbind(multiList, sampleList)
	} # end i loop
	second <- matrix(rep(1:nrScans, each=nrow(sampleList) ))
	first <- matrix(rep(1:nrow(sampleList), nrScans) )
	a <- data.frame(multiList, second=second, first=first)
	a <- as.data.frame(a[order(first, second), ][,1:(ncol(a)-2)]) # order, then cut away the last two columns

	ConSNr <- rep(1:(nrScans),nrow(sampleList)) # generate a vector numbering all the consecutive scans
	if (ncol(a) > 1) {
		a <- data.frame(a[1], ConSNr=ConSNr, a[-1]) # insert into data frame
	} else {
		a <- data.frame(a[1], ConSNr=ConSNr) # insert into data frame		
	}
	ColNames <- colnames(a)
	ColNames[2] <- paste(.ap2$stn$p_yVarPref, .ap2$stn$p_conSNrCol, sep="")
	colnames(a) <- ColNames
	rownames(a) <- seq(1:nrow(a)) # correct all the rownames
	return(a)
} # EOF

readHeader_checkDefaults <- function(slType, possibleValues, md, multiplyRows) {
	if (all(slType == "def") & !is.null(slType)) {
		slType <- .ap2$stn$imp_sampleListType
	}
	if (!is.null(slType)) {
		if (!all(is.character(slType)) | length(slType) != 1) {
			stop("Please provide a character length one or NULL to the argument 'slType'.", call.=FALSE)
		}
	}
	if (!any(possibleValues %in% slType) & !is.null(slType)) {
		stop("Please refer to the help for 'getFullData' for the possible values for the argument 'slType'", call.=FALSE)
	}
	assign("slType", slType, pos=parent.frame(n=1))
	assign("slType", slType, pos=parent.frame(n=2)) # that is needed to always have the correct value for slType in the getFullData function
	assign(".slType", slType, envir=.ap2)
	###
	if (class(md) == "aquap_md") {
		filename <- md$meta$expName
	} else {
		stop("Please provide a valid metadata object of class 'aquap_md' to the argument 'md'", call.=FALSE)
	}
	assign("filename", filename, pos=parent.frame(n=1))
	###
	if (all(multiplyRows == "def")) {
		multiplyRows <- .ap2$stn$imp_multiplyRows
	} else {
		if (!all(is.logical(multiplyRows)) | length(multiplyRows) != 1) {
			stop("Please provide either 'TRUE' or 'FALSE' to the argument 'multiplyRows'.", call.=FALSE)
		}
	}
	assign("multiplyRows", multiplyRows, pos=parent.frame(n=1))
	assign("multiplyRows", multiplyRows, pos=parent.frame(n=2))  # that is needed to always have the correct value for multiplyRows in the getFullData function
} # EOF

check_sl_existence <- function(filename, ext) {
	slInFolder <- paste(.ap2$stn$fn_sampleLists, "/", .ap2$stn$fn_sampleListIn, sep="")
	fn <- paste(filename, ext, sep="")
	a <- paste(slInFolder, "/", fn, sep="")
	if (!file.exists(a)) {
		stop(paste("The sample-list file \"", fn, "\" does not seem to exist in \"", slInFolder, "\".", sep=""), call.=FALSE)
	}	
} # EOF

check_conScanColumn <- function(header, headerFilePath, spectraFilePath, slType, filetype) {
	custImp <- grepl("custom@", filetype)
	a <- paste(.ap2$stn$p_yVarPref, .ap2$stn$p_conSNrCol, sep="")
	lookHelp <- ""
	if (!any(a %in% colnames(header))) {
		if (is.null(slType)) {
			from <- paste(" in the file \n\"", spectraFilePath, "\" containing the spectral data.", sep="")
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
			from <- paste(", neither in your sample-list file \n\"", headerFilePath, "\", nor in file \n\"", spectraFilePath, "\" containing the spectral data.", sep="")
			where <- paste(" either to the sample list or to the file containing the spectral data. In the latter case please modify the custom import function accordingly.\n")
 			maybe <- "(You maybe encounter this error because you did choose to *not* multiply the rows of the sample list by the nr. of consecutive scans, in which case a column for the nr. of cons. scan gets inserted automatically.)\n"
		}
		msg <- paste("You do not have a column for the nr of the consecutive scan", from, " \nPlease, add a column called \"", a, "\"", where, maybe, lookHelp, sep="")
		stop(msg, call.=FALSE)
	}	
} # EOF

check_conScanColumn_2 <- function(header, filename, ext) {
	a <- paste(.ap2$stn$p_yVarPref, .ap2$stn$p_conSNrCol, sep="")
	if (!any(a %in% colnames(header))) {
		stop(paste("You do not have a column for the nr of the consecutive scan in your sample list called \"", filename, ext, "\". \nPlease, add a column called \"", a, "\" to the file as the second column and provide the right values in this column. \n(You probably encounter this error because you did choose to *not* multiply the rows of the sample list by the nr. of consecutive scans.)\nPlease refer to the help for 'getFullData' for further information.", sep=""), call.=FALSE)
	}
	ind <- grep(a, colnames(header), fixed=TRUE)
	if (ind != 2) {
		stop(paste("Your 'custom-column' for the number of the consecutive scan should, please, be the second column. \nPlease modify the file \"", filename, ext, "\" accordingly.", sep=""), call.=FALSE)
	}
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
	autoUpS()
	poss_sl_types <- c("csv", "txt", "xls") 			### XXXVARXXX
	filename <- NULL # will be changed in the checking
	readHeader_checkDefaults(slType, poss_sl_types, md, multiplyRows)
	slInFolder <- paste(.ap2$stn$fn_sampleLists, "/", .ap2$stn$fn_sampleListIn, "/", sep="")
	if (is.null(slType)) {
		return(NULL)
	}
	if (slType == "csv") {
		ext <- "-in.csv"
		check_sl_existence(filename, ext)
		rawHead <- read.csv(paste(slInFolder, filename, ext, sep=""))
	}
	if (slType == "txt") {
		ext <- "-in.txt"
		check_sl_existence(filename, ext)
		rawHead <- read.table(paste(slInFolder, filename, ext, sep=""), header=TRUE)
	}
	if (slType == "xls") {
		ext <- "-in.xlsx"
		check_sl_existence(filename, ext)
		rawHead <- openxlsx::read.xlsx(paste(slInFolder, filename, ext, sep="")) # ! character imports are NOT factors yet !!
		for (i in 1: ncol(rawHead)) {
			if (all(is.character(rawHead[,i]))) {
				rawHead[,i] <- as.factor(rawHead[,i])
			}
		}
		
	}
	assign("headerFilePath", paste(slInFolder, filename, ext, sep=""), pos=parent.frame(n=1))
	###
	rawHead <- convertYColsToNumeric(rawHead)
	nrScans <- md$postProc$nrConScans
	if (multiplyRows) {
		header <- multiplySampleListRows(rawHead, nrScans)
	} else {
		header <- rawHead
		# check_conScanColumn_2(header, filename, ext)
	}
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
#' the current slType with '.ap2$.slType' - see example.
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
#'    imp_searchAskColumns(allC_var, allY_var, .ap2$.slType) 
#'    # assigns all list elements except timestamp, info and NIR
#'    #
#'    return(list(sampleNr=sampleNr, conSNr=conSNr, timePoints=timePoints, 
#'    ecrm=ecrm, repl=repl, group=group, temp=temp, relHum=relHum, C_cols=C_cols, 
#'    Y_cols=Y_cols, timestamp=timestamp, info=info, NIR=NIR))
#'    #
#'  } # EOF
#' }
#' @export
imp_searchAskColumns <- function(allC_var, allY_var, slType=.ap2$.slType) {
	autoUpS()
	yPref <- .ap2$stn$p_yVarPref
	cPref <- .ap2$stn$p_ClassVarPref
	sampleNrColn <- .ap2$stn$p_sampleNrCol
	conSNrColn <- .ap2$stn$p_conSNrCol
	timePointsColn <- .ap2$stn$p_timeCol
	ecrmColn <- .ap2$stn$p_ECRMCol
	replColn <- .ap2$stn$p_replicateCol
	groupColn <- .ap2$stn$p_groupCol
	tempColn <- .ap2$stn$p_tempCol
	relHumColn <- .ap2$stn$p_RHCol
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
					cat(paste(msg1, cn, msg2, cn, msg3, sep=""))
				} else {
					cat(paste(msg1, cn, msg2_zero, cn, msg3_zero, sep=""))				
				}
				cat(notRep)
				if (cle > 0) {
					cat(paste(1:cle, " -- ", colnames(allVars[[k]]), "\n", sep=""))
				}
				nrOk <- FALSE
				while(!nrOk) {
					a <- readLines(n=1)
					options(warn=-1); a <- as.numeric(a); options(warn=0)
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

