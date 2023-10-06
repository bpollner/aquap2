# Spectra from .da file --------------------------------------------------
getNIRData_Vision_da <- function(dataFile) {
	nrBytesTrail0 <- 96
	fCon <- file(dataFile, open="rb")
	fSize <- file.info(dataFile)$size
	a <- readBin(fCon, integer(), 2,2) ## to get to position 14, because 'seek' is said not to work on windows
	a <- readBin(fCon, raw(), 10, 1)
	nrCol <- readBin(fCon, integer(), 1, 2)
	firstWl <- readBin(fCon, integer(), 1, 2)
	lastWl <- readBin(fCon, integer(), 1, 2)

#	a <- readBin(fCon, raw(), 108, 1) ## to get to position 128
#	a <- readBin(fCon, double(), nrCol, 4) ## to get to the end fo the data
#	continue <- TRUE
#	cnt <- 1
#	firstByte <- readBin(fCon, raw(), 1, 1)
#	while(continue) {
#		nextByte <- readBin(fCon, raw(), 1, 1)
#		if (nextByte != firstByte) {
#			break
#		} # end if
#		cnt <- cnt + 1
#	} # end while
#	print(cnt); wait()
	
	delta <- (lastWl - firstWl) / nrCol
	if(delta == 0.5) {
		nrBytesTrail0 <- 96
	} else {
		if (delta == 2) {
			nrBytesTrail0 <- 24
		} else {
			cat("Error at Delta Wavelength. Please stay calm.", call.=FALSE)
		}
	} # end else
	bpr <-  128 + 4*nrCol + nrBytesTrail0 # the number of bytes in a single row
	nrRows <- fSize / bpr
	wlsChar <- paste("w", seq(firstWl, lastWl - delta, by=delta), sep="")
	sampleNr <- matrix(NA, nrRows, 1)
	sampleName <- matrix(NA, nrRows, 1)
	timeOut <- NULL
	NIR <- matrix(NA, nrRows, nrCol)
	close(fCon)
	fCon <- file(dataFile, open="rb") ## to be back at position 0 (no 'seek' on windows)
	for (i in 1: nrRows) {
		a <- readBin(fCon, integer(), 1, 2) ## to pos 2
		sampleNr[i,1] <- readBin(fCon, integer(), 1, 2) ## to pos 4
		sampleName[i,1] <- rawToChar(readBin(fCon, raw(), 10, 1)) ## to pos 14
		a <- readBin(fCon, raw(), 50, 1) ## to pos 64
		dateTime <- readBin(fCon, integer(), 6, 2) ## to pos 76		
		timeNice <- as.POSIXct(strptime(paste(dateTime, collapse="-"), "%d-%m-%Y-%H-%M-%S"))
		timeOut <- c(timeOut, timeNice)
		a <- readBin(fCon, raw(), 52, 1) ## to pos 128
		NIR[i,] <- readBin(fCon, double(), nrCol, 4) ## to pos 128 + 4*nrCol
		a <- readBin(fCon, raw(), nrBytesTrail0, 1) ## to end of row
	} # end for i
	close(fCon)
	timePosix <- as.POSIXct(timeOut, origin="1970-01-01")
	dfTime <- data.frame(timePosix)
	dfOut <- cbind(sampleNr, sampleName, dfTime, NIR)
	colnames(dfOut) <- c("sampleNr", "sampleName", "Timestamp", wlsChar)
	rownames(dfOut) <- make.unique(sampleName)
#	out <- dfOut[, -(1:2)]
#	out <- dfOut
	NIR <- as.matrix(dfOut[, -(1:3)])
	rownames(NIR) <- make.unique(sampleName)	
	nCharPrevWl <- 1
	timestamp <-  dfOut[3]
	sampleNr <- NULL
	conSNr <- NULL
	timePoints <- NULL
	ecrm <- NULL
	C_cols <- NULL
	Y_cols <- NULL
	repl <- NULL
	group <- NULL
	temp <- NULL
	relHum <- NULL
	info <- list(nCharPrevWl=nCharPrevWl)
	outList <- list(sampleNr=sampleNr, conSNr=conSNr, timePoints=timePoints, ecrm=ecrm, repl=repl, group=group, temp=temp, relHum=relHum, C_cols=C_cols, Y_cols=Y_cols, timestamp=timestamp, info=info, NIR=NIR)
	return(outList)
} # EOF 


# Spectra from tab delim .txt file ---------------------------------------
getNirData_plainText <- function(dataFile, naStrings="NA") {
	sampleNr <- conSNr <- timePoints <- ecrm <- repl <- group <- temp  <- relHum <- C_cols <- Y_cols <- timestamp <- NULL
	info <- list(nCharPrevWl=1)
	NIR <- as.matrix(read.table(dataFile, na.strings=naStrings))
	return(list(sampleNr=sampleNr, conSNr=conSNr, timePoints=timePoints, ecrm=ecrm, repl=repl, group=group, temp=temp, relHum=relHum, C_cols=C_cols, Y_cols=Y_cols, timestamp=timestamp, info=info, NIR=NIR))
} # EOF


# Spectra from pirouette .pir file ---------------------------------------

PirInfo <- function(dataFile) {
  fCon <- file(dataFile, open = "rb")
  nByts <- readBin(fCon, integer(), n = 5, size = 4)[5]
  nColSpect <- readBin(fCon, integer(), n = 1, size = 4)
  nColClass <- readBin(fCon, integer(), n = 1, size = 4)
  nColDepen <- readBin(fCon, integer(), n = 1, size = 4)
  nColAll <- readBin(fCon, integer(), n = 30, size = 4)[30]
  nRow <- readBin(fCon, integer(), n = 1, size = 4)
  close(fCon)
  return(list(NoColAll = nColAll, NoRow = nRow, NoColSpect = nColSpect, NoColClass = nColClass , NoColDepen = nColDepen, NoByts = nByts) )
} # EOF

PirIndexes <- function(dataFile, nByts) { #could be quicker with for like in header function
	fCon <- file(dataFile, open = "rb")
	Raw <- readBin(fCon, raw(), n = nByts * 128)
	close(fCon)
	nrow<-floor(length(Raw)/4)
	AAA <- matrix(Raw, ncol=nrow)
	AAA<-t(AAA)
  
	posi <- NA
	while(TRUE) {
  		for (i in 1:nrow){
    		match <- which(AAA[i,]=="ff")
    		if (length(match)==4){
      			posi <- c(posi,i)
    		}
		}	
  		if(length(posi)>6){
  			break
 		}
  	}
	return(4*(posi[-c(1,2)])+136) # posi
} # EOF

getPirTable <- function(dataFile, fromPosi, toPosi, nRow, nColAll) {
  fCon <- file(dataFile, open = "rb")
  a <- readBin(fCon, raw(), fromPosi + 4) # jump to the start point
  
  DatatoRead <- toPosi - fromPosi
  a <- readBin(fCon, raw(), DatatoRead) # read all the data as raw
  
  IndNoData <- seq(116, length(a), 128)
  IndNoData <- sort(c(IndNoData, IndNoData-1, IndNoData-2, IndNoData-3))
  
  a <- a[-IndNoData] # eliminate the byte indexes (one block contains 128 units but always starts with FF FF FF FF, we do not need)
  close(fCon)
  a <- readBin(a, double(), size=4, DatatoRead) # translet it into numbers
  a <- a[1:(nRow*nColAll)]
  a[which(a==9.99999968028569246551e+37)] <- NA
  
  out <- NIR <- matrix(round(a, 7), nRow, nColAll)
}#Eof function

getPirHead <- function(dataFile, fromPosi, toPosi, lengthHead) {
  fCon <- file(dataFile, open = "rb")
  a<-readBin(fCon, raw(), fromPosi)
    
  b<-readBin(fCon, raw(), toPosi-fromPosi)#135
  close(fCon)
  if (length(b) > 128){
    IndNoData <- seq(120, length(b), 128)
    IndNoData <- sort(c(IndNoData, IndNoData-1, IndNoData-2, IndNoData-3))
    b <- b[-IndNoData]
  }
  colNames <- readBin(b, character(), length(b))
  return(colNames[1:(lengthHead)])
} # EOF

createPirMatrix <- function(PirTable, colNames, rowNames, nColSpect, nColClass, noColDepen) {
  if(nColSpect+nColClass+noColDepen==1){
    PirTable <- matrix(PirTable, ncol=1)
  }
  colnames(PirTable) <- colNames
  rownames(PirTable) <- rowNames
  if (nColSpect > 0){
    spectCol <- 1:nColSpect
    IndepenVar = matrix(PirTable[,spectCol], ncol = nColSpect)
    colnames(IndepenVar) <- paste0("X", colNames[spectCol])
    rownames(IndepenVar) <- rowNames
    colnames(PirTable)[spectCol] <- colnames(IndepenVar)
  } else {
    IndepenVar = NA
  }
  if (nColClass > 0){
    classCol <- (nColSpect+1):(nColSpect+nColClass)
    ClassVar = as.data.frame(PirTable[,classCol])
    colnames(ClassVar) <- colnames(PirTable)[classCol]
  } else {
    ClassVar = NA
  }
  if (noColDepen > 0){
#   depenCol <- c((ncol(PirTable) - noColDepen) : ncol(PirTable))
    depenCol <- c((ncol(PirTable) - noColDepen + 1) : ncol(PirTable)) ## B.P. added the +1 here
    DepenVar = as.data.frame(PirTable[,depenCol])
    colnames(DepenVar) <- colnames(PirTable)[depenCol]
  } else {
    DepenVar = NA
  }
  return(list(FullData = PirTable, IndepenVar = IndepenVar, ClassVar = ClassVar, DepenVar = DepenVar))
} # EOF

pir_importPirFile <- function(dataFile) {
	Info <- PirInfo(dataFile)
	Indexes <- PirIndexes(dataFile, nByts = Info$NoByts)
	PirData <- getPirTable(dataFile, fromPosi=Indexes[1], toPosi=Indexes[2], nRow=Info$NoRow, nColAll=Info$NoColAll) #toPosi is still not in use
	headAll <- getPirHead(dataFile, fromPosi = Indexes[2], toPosi = Indexes[3], lengthHead = Info$NoColAll)	
	rowNames <- getPirHead(dataFile, fromPosi = Indexes[3], toPosi = Indexes[4], lengthHead = Info$NoRow)
  	return(createPirMatrix(PirTable=PirData, colNames=headAll, rowNames=rowNames, nColSpect=Info$NoColSpect, nColClass=Info$NoColClass, noColDepen=Info$NoColDepen))
} # EOF

# Master
getNIRData_Pirouette <- function(dataFile) {
	a <- getStdColnames()
	stdColsY <- a$stdColsY
	stdColsC <- a$stdColsC
	#
	pir <- pir_importPirFile(dataFile)
	#
	nCharPrevWl <- 1
	info <- list(nCharPrevWl=nCharPrevWl)
	NIR <- pir$IndepenVar
	timestamp <- NULL
	allC_var <- pir$ClassVar
	allY_var <- pir$DepenVar
#	print(allC_var); print(allY_var); wait()
	sampleNr <- conSNr <- timePoints <- ecrm <- repl <- group <- temp <- temp <- relHum <- C_cols <- Y_cols <- NULL
	slType <- get(".slType", pos=gl_ap2GD) ## .slType gets assigned in readHeader_checkDefaults
	imp_searchAskColumns(allC_var, allY_var, slType) # assigns all the necessary list elements except NIR, info and timestamp in this frame !!!
	outList <- list(sampleNr=sampleNr, conSNr=conSNr, timePoints=timePoints, ecrm=ecrm, repl=repl, group=group, temp=temp, relHum=relHum, C_cols=C_cols, Y_cols=Y_cols, timestamp=timestamp, info=info, NIR=NIR)
	return(outList)	
} # EOF

# Spectra from an Excel File ---------------------------
getNirData_Excel <- function(dataFile, stn) {
	metaSheetSfx <- gl_xlsx_metaSheetSuffix
	dataSheetSfx <- gl_xlsx_DataSheetSuffix
	allMetaCns <- c(gl_xlsx_ncolHeader_name, gl_xlsx_rownamesAsFirstColumn, gl_xlsx_ncpwlColumn)
	yPref <- stn$p_yVarPref
	cPref <- stn$p_ClassVarPref
	outlierCol <- stn$p_outlierCol
	noSplitCol <- stn$p_commonNoSplit
	tempCol_C <- paste0(cPref, stn$p_tempCol)
	rhCol_C <-	paste0(cPref, stn$p_RHCol)
	sampleNr_C <- paste0(cPref, stn$p_sampleNrCol)
	conSNr_C <- paste0(cPref, stn$p_conSNrCol)
	absTime_C <- paste0(cPref, stn$p_absTime)
	chron_C <- paste0(cPref, stn$p_chron)
	tsChar <- "Timestamp" # XXX ? just like that?
	absTime_Y <- paste0(yPref, stn$p_absTime)
	absChron_Y <- paste0(yPref, stn$p_chron)
	#
	# do we have a worksheet with the metadata?
	wb <- openxlsx::loadWorkbook(file=dataFile)
	wbNames <- names(wb)
	ind <- grep(metaSheetSfx, wbNames)
	if (length(ind) == 0) {
		stop(paste0("Sorry, for a smooth import from xlsx it is required that there is an extra worksheet containing some metadata.\nPlease add a worksheet with its name ending in '", metaSheetSfx, "' to the file \n'", dataFile, "', \nand provide there the following information:\n\t1: The number of columns of the header. Set to 0 if the data only contains NIR spectra.\n\t2: Are there rownames in the data? Provide either TRUE or FALSE.\n\t3: How many characters are there in front of the wavelengths? Set to 0 if there are no characters in front of the wavelengths.\nPlease provide the information in one row with three columns, wiht the columns having the following names:\n\t", paste0(allMetaCns, collapse=", ")), call.=FALSE )
	} # end if
	if (length(ind) > 1) {
		stop(paste0("Sorry, there appears to me more than 1 worksheet ending in '", metaSheetSfx, "' in the file '", dataFile, "'.\nFor a smooth import from xlsx, there should be only one worksheet containing metadata.\n"), call.=FALSE)
	} # end if
	
	# by now we must have one worksheet containing the metadata
	metaDf <- openxlsx::read.xlsx(wb, sheet=ind, colNames=TRUE, rowNames=FALSE)
	metaSheetName <- wbNames[ind]
	if (!all(allMetaCns %in% colnames(metaDf))) {
		stop(paste0("Please make sure that the columns in the worksheet containing the metadata \n('", metaSheetName, "', in the file '", dataFile, "' ) \nhave the following names:\n\t", paste0(allMetaCns, collapse=", ")), call.=FALSE)
	} # end if
	
	# now we can be quite sure about our metaDf, contains all three required columns
	ncolHeader <- nch <-  metaDf[1,gl_xlsx_ncolHeader_name]
	hasRownames <- metaDf[1,gl_xlsx_rownamesAsFirstColumn]
	ncpwl <- metaDf[1, gl_xlsx_ncpwlColumn]
	
	# do we have a sheet with the data suffix?
	ind <- grep(dataSheetSfx, wbNames)
	if (length(ind) == 0) {
		dsInd <- 1 # we are assuming it would be the first worksheet holding the data
	} else {
		dsInd <- ind
	} # ed else
	allData <- openxlsx::read.xlsx(wb, sheet=dsInd, colNames=TRUE, rowNames=hasRownames)
	if (ncolHeader == 0) {
		NIR <- as.matrix(allData) # * no subscripting: we get all numbers
		header <- NULL
	} else { # so we have NIR and header in the same worksheet
		NIR <- try(as.matrix(allData[, (nch+1):ncol(allData)]), silent=TRUE) # very interesting: when subscripting from allData we get characters. Compare * above
		if (is(NIR, "try-error")) {
			stop(paste0("There was an error while trying to extract the NIR data.\nMaybe the number of columns of the header in the worksheet containing the metadata ('", metaSheetName, "' in the file \n'", dataFile, "') is not correct?"), call.=FALSE)
		} # end if
		cns <- colnames(NIR); rns <- rownames(NIR); nrc <- ncol(NIR); nrr <- nrow(NIR)
		NIR <- matrix(as.numeric(NIR), nrow=nrr, ncol=nrc, byrow=FALSE)
		colnames(NIR) <- cns
		rownames(NIR) <- rns
		header <- allData[, 1:nch]
	} # end else
#	print(str(NIR)); print(str(header)); print("----dd-----"); wait()
	sampleNr <- conSNr <- timePoints <- ecrm <- repl <- group <- temp  <- relHum <- C_cols <- Y_cols <- timestamp <- allC_var <- allY_var <- NULL
	#
	if (!is.null(header)) {
		delThisCols <- function(header, char) {
			ind <- grep(char, colnames(header))
			if (length(ind) > 0) {
				header <- header[,-ind]
			} # end if
			return(header)
		} # EOIF
		indTs <- which(colnames(header) == tsChar) # but this assumes that there is a column called "Timestamp"
		if (length(indTs) == 1) { # so we have one column called "Timestamp"
			timestamp <- as.data.frame(as.POSIXct(header[,indTs]))
			header <- delThisCols(header, absTime_Y) # absTime in Y; we do not want to have it twice
			header <- delThisCols(header, absChron_Y) # chron in Y; we do not want to have it twice
			header <- delThisCols(header, absTime_C) # absTime in C; we do not want to have it twice
			header <- delThisCols(header, chron_C)			
		} else if (length(indTs) > 1) {
			stop(paste0("Sorry, there should not be more than one column with the name '", tsChar, "'."), call.=FALSE)
		} # end else
		# get rid of possible: 
		header <- delThisCols(header, outlierCol) # outliers
		header <- delThisCols(header, noSplitCol) # no split column
		header <- delThisCols(header, tempCol_C) # no temp as class
		header <- delThisCols(header, rhCol_C) # no RH as class
		header <- delThisCols(header, sampleNr_C) # no sample number as class
		header <- delThisCols(header, conSNr_C) # no cons scan number as class
		#
		header <- convertYColsToNumeric(header)
		#
		indC <- grep(cPref, colnames(header))
		if (length(indC) > 0) {
			allC_var <- header[,indC] # collect all C_ variables
		} # end if
		indY <- grep(yPref, colnames(header))
		if (length(indY) > 0) {
			allY_var <- header[,indY] # collect all Y_ variables
		} # end if		
		slType <- get(".slType", pos=gl_ap2GD) ## .slType gets assigned in readHeader_checkDefaults
		imp_searchAskColumns(allC_var, allY_var, slType, oT=TRUE) # assigns all the necessary list elements except NIR, info and timestamp in this frame !!!
	} # end if	
	#
	info <- list(nCharPrevWl=ncpwl)
	outList <- list(sampleNr=sampleNr, conSNr=conSNr, timePoints=timePoints, ecrm=ecrm, repl=repl, group=group, temp=temp, relHum=relHum, C_cols=C_cols, Y_cols=Y_cols, timestamp=timestamp, info=info, NIR=NIR)
#	print(str(outList)); print("-----outlist----")
	return(outList)
} # EOF


##########################################################################


# Temp & relHum from ESPEC -----------------------------------------------
importTRH_ESPEC <- function(path) {
	TRH <- read.table(path, header=F, skip=4)[,-c(2,5:10)] # this the temp file
	colnames(TRH) <- c("Time", "Temp", "RelHum")
	TRH$Time <- strptime(TRH$Time,format = "%Y/%m/%d %H:%M'%S")
	return(TRH)
} # EOF
