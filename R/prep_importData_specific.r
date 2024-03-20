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

#########################################################
## Yunosato styled .dat file ####
getNirDataPlusMeta_YunosatoDat <- function(dataFile, stn, yDatPref = "w", ydCPref = "*", ydYPref = "$", ydIdSep = "_", yDimSplit="x") {
	# the incoming dataFile is the path to the .dat file, we have to open the .dat file first
	########
	# import from .dat file as styled by the Yunosato Aquaphotomics Lab, Japan
	# as discussed with their developer, the .dat file is to be styled as follows:
	# row starting with "#D" is the dimension: nr of columns and nr of observations / rows. e.g. "#D	25x30"
	# row starting with  "#C" the column names: "w" preceding the wavelengths, "*" preceding the class-variables, "$" preceding the numeric variables
	# subsequent rows starting with "#S": starting with one character as sample name, then the raw data
	# 		The sample name in the first element in all the #S rows is further structured, with individual elements being separated by a "_".
	#		e.g. "S1-1_3_20240212164741"
	# 		the last element is a date/time stamp
	#		the second-last element is the number of the consecutive scan
	#		all elements before that, may they include an "_" or not, are a user-defined sample name and will be used in a new class-variable called "ydSampleId"
	#		the required column holding the running sample number is called "$SampleNr"
	dimPref <- "#D"
	colNpref <- "#C"
	dPref <- "#S"
	yPref <- stn$p_yVarPref
	cPref <- stn$p_ClassVarPref
	sampleNrColinDat <- paste0(ydYPref, stn$p_sampleNrCol)
	sampleNrCol_Y <- paste0(yPref, stn$p_sampleNrCol)
	saIdCnUse <- paste0(cPref, gl_ydSampleIdColName)
	conSNr_Y <- paste0(yPref, stn$p_conSNrCol)

	#
	##### read in the .dat file and get indices
	fcon <- file(dataFile, open="r")
    datChars <- readLines(fcon)   # read in the Yunosato .dat file
    close(fcon)	
    dimInd <- grep(dimPref, datChars) # the number of the character that has the dimension
    cnInd <- grep(colNpref, datChars) # the number of the characters that has the colnum names
	datInd <- grep(dPref, datChars) # all the characters that contain data !! still the sample ID in the first column
    
    ### get dimensions
    dimChar <- datChars[dimInd]
	dimChar <- trimws(gsub(pattern=paste0(dimPref, "\t"), "", dimChar))
	bbb <- strsplit(dimChar,split = yDimSplit)[[1]] # (get out of the list)
	colDim <- as.numeric(bbb[1])
	rowDim <- as.numeric(bbb[2])
	
	### get columns
	colChar <- datChars[cnInd] # the one row (character vector) that contains all the column names
	colChar <- strsplit(colChar, "\t")[[1]][-1] # leave out the first, it MUST be the #C
	datColInd <- startsWith(colChar, yDatPref)
	dataCols <- colChar[datColInd]
	clInd <- startsWith(colChar, ydCPref)
	classCols <- colChar[clInd]
	numInd <-startsWith(colChar,ydYPref)
	numCols <- colChar[numInd]
	# replace yunosato prefix with aquap2 prefix
	classCols <- sub(ydCPref, cPref, classCols, fixed=TRUE)
	numCols <- sub(ydYPref, yPref, numCols, fixed=TRUE)	# fixed does the magic
	if (! sampleNrCol_Y %in% numCols) {
		stop(paste0("Sorry, it appears that the column '", sampleNrColinDat, "' for the running sample number in your .dat file is missing. \nPlease modify your .dat file and re-import."), call.=FALSE)
	} # end if
	
	### get the NIR data and extract the sample IDs
	onlyData <- datChars[datInd]
	onlyData <- strsplit(onlyData, split="\t") # gives back a list with a vector of strings for each row in each list element
	sampleIds <- unlist(lapply(onlyData, function(x) x[2])) # get out the sample IDs
	onlyData <- lapply(onlyData, function(x) x[-c(1,2)]) # only leave the real data: first the NIR data, then the columns
	
	### split up the sample IDs
	idSplit <- strsplit(sampleIds, "_")
	timestamp <- unlist(lapply(idSplit, function(x) x[length(x)])) # the last is the timestamp
	timestamp <- as.data.frame(strptime(timestamp, format="%Y%m%d%H%M%S"))
	colnames(timestamp) <- "timestamp"
	conSNr <- as.numeric(unlist(lapply(idSplit, function(x) x[(length(x)-1)]))) # the second last is the nr of consecutive scan
	conSNr <- as.data.frame(conSNr)
	colnames(conSNr) <- conSNr_Y
	sampleIds <- unlist(lapply(idSplit, function(x) paste(x[1:(length(x)-2)], collapse="_"))) # should catch all additional splits in the ID, if there might be an "_" inside

	# ok, so now we have the column names, the timestamp, the consec. scan number and the sample ID
	### lets get the data
	odDf <- do.call(rbind.data.frame, onlyData)
	NIR <- apply(odDf[,datColInd], 2, as.numeric) # gives back a matrix
	colnames(NIR) <- dataCols
	rownames(NIR) <- sampleIds
	
	# get the class and numerics
	sampleNr <- timePoints <- ecrm <- repl <- group <- temp  <- relHum <- C_cols <- Y_cols <- allC_var <- allY_var <- NULL
	if (any(clInd)) {
		allC_var <- as.data.frame(odDf[,clInd]) # as.data.frame is reduntant here... 
		allC_var <- cbind(allC_var, as.data.frame(sampleIds))
		colnames(allC_var) <- c(classCols, saIdCnUse)
	} # end if 	
	if (any(numInd)) { # so we have at least one in class or numeric
		allY_var <- odDf[,numInd]
		colnames(allY_var) <- numCols
		allY_var <- as.data.frame(apply(allY_var, 2, as.numeric))
		allY_var <- cbind(allY_var, conSNr)
	} # end if
	#
	slType <- get(".slType", pos=gl_ap2GD) ## .slType gets assigned in readHeader_checkDefaults
	imp_searchAskColumns(allC_var, allY_var, slType, oT=TRUE) # assigns all the necessary list elements except NIR, info and timestamp in this frame !!!
	info <- list(nCharPrevWl=length(yDatPref))
	outList <- list(sampleNr=sampleNr, conSNr=conSNr, timePoints=timePoints, ecrm=ecrm, repl=repl, group=group, temp=temp, relHum=relHum, C_cols=C_cols, Y_cols=Y_cols, timestamp=timestamp, info=info, NIR=NIR)
#	print(str(outList)); print("-----outlist----")
	return(outList)	
} # EOF
##########################################################################

##########################################################################
##  import from MicroNIR.csv
getNirData_microNIR <- function(dataFile, stn) {
# descr ##### 
	# first column contains the sample-id: asdfasdf-1.sam,
	# ending with '-' and then the cons. scan nr. 
	# are we sure the sample id before the '-' is unique? Yes. If same character
	# gets put in again, the cons. scan nr continues where it stopped last.
	# !! if the rest (all without the last '-1.sam') is only numbers, then use
	# this as sample Number. 
	# If it contains characters, fuse together, use as sample ID and generate the
	# sample number via rle() from this sampleID.
	#
	# column names are prepended with an X. Only 'X' for the first column,
	# X1300 (e.g.) for the wavelength.
	# The rest is called 
	# Instrument.Serial.Number Temperature            Notes            Timestamp
	#
	# rename the "Temperature" to "device_temp"
	# put the instrument serial number into an @ in the dataset
	#
	# keep the notes in, but also export as extra file only containing those
	# rows that do contain notes, together with:
	# rowname, sampleNumber, consScanNr (do we always have sample Number) ?
	# put that in sl-in
	#
	# Timestamp. Yes.
	#
	## code ####
	# first get the first column, isolate cons. scans, the rest is sample ID and rowname
	# All numbers? --> sampleNr
	# has character? --> sampleId, use rle() on rest for sampleNr
	stn <- getstn()
	yPref <- stn$p_yVarPref
	cPref <- stn$p_ClassVarPref
	Y_sampleNrCol <- paste0(yPref, stn$p_sampleNrCol)
	Y_conSNrCol <- paste0(yPref, stn$p_conSNrCol)
	C_colnSampleId <- paste0(cPref, stn$p_sampleIdCol)
	notesColName <- stn$p_NotesCol
	timeFormat <- stn$imp_timeFormat_microNir
	prWls <- "X"
	samPat <- ".sam"
	snSamSep <- "-"
	addSep <- "#"
	colnNotesIn <- "Notes"
	colnTempIn <- "Temperature"
	colnIsnIn <- "Instrument.Serial.Number"
	colnTsIn <- "Timestamp"
	txtMicroNirsAdd <- "VIAVI MicroNIR, "
	#
	colnDeviceTemp <- paste0(yPref, "deviceTemp")
	colnTiSta <- gl_timestamp_column_name
	####
	raw <- read.csv(dataFile)
	C_cols <- data.frame(raw[,colnNotesIn])  # will possibly add sampleId
	colnames(C_cols) <- notesColName
	fc <- raw[,1]
	fcSplit <- strsplit(fc, snSamSep)
	sLast <- unlist(lapply(fcSplit, function(x) x[length(x)]))
	conSNr <- as.data.frame(as.numeric(gsub(samPat, "", sLast))) # so we have the cons. scan nr. 
	colnames(conSNr) <- Y_conSNrCol
	
	saId <- unlist(lapply(fcSplit, function(x) {
		aa <- x[1:(length(x)-1)]
	  	paste(aa, collapse=snSamSep) })) # end unlist
	saId <- data.frame(saId)
	colnames(saId) <- C_colnSampleId
	#
	# all numeric?
	options(warn=-1)
	saIdNum <- as.numeric(saId[,1])
	options(warn=0)
	if (!any(is.na(saIdNum))) { # just numbers --> use for sampleNr, no sampleId
		sampleNr <- data.frame(saIdNum) # no addition to C_cols, as no sampleId
		colnames(sampleNr) <- Y_sampleNrCol
		univSaId <- saIdNum
	} else { # use it as sample ID, use rle for sampleNr
		C_cols <- cbind(C_cols, saId)
		rleSaid <- rle(saId[,1])
		snrs <- 1:length(rleSaid$lengths)
		sampleNr <- data.frame(sampleNr=rep(snrs, rleSaid$lengths)) # and we have the sampleNr
		colnames(sampleNr) <- Y_sampleNrCol
		univSaId <- saId[,1]
	} # end else
		
	# now correct for possible double sampleIds
	makeBadBoyAddition <- function(bbRl) {
		vecBad <- 1:sum(bbRl$values)
		collec <- vector("character")
		uvb <- 1 # which one of vecBad to use
	  	for (i in seq_along(bbRl$lengths)) {
			if (bbRl$values[i] == 1) { # an occurrence
		  		charAdd <- rep(paste0(addSep, vecBad[uvb]), bbRl$lengths[i])
		  		uvb <- uvb+1
			} else {
		  		charAdd <- rep("", bbRl$lengths[i])
			} # end else
			collec <- c(collec, charAdd)
	  	} # end for i
		return(collec)
	}# EOIF
	
	fuseAdditionsTogether <- function(collListSaIdAdd) {
		dfAdd <- data.frame(matrix(NA, nrow(raw)))
		for (i in 1:(length(collListSaIdAdd))) {
			tiAdd <- data.frame(collListSaIdAdd[[i]])
			dfAdd <- cbind(dfAdd, tiAdd)
	  	} # end for i 
	  	dfAdd <- dfAdd[,-1, drop=FALSE]
	  	out <- as.data.frame(apply(dfAdd, 1, function(x) paste(x, collapse="")))
	  	return(out)
	} # EOIF
	
	genNewConSNrs <- function(saIdNew) {
		newConSNrs <- vector("numeric")
	  	idLe <- rle(saIdNew)$lengths
	  	for (i in seq_along(idLe)) {
			newSi <- 1:(idLe[i])
			newConSNrs <- c(newConSNrs, newSi)
		} # end for i
		return(newConSNrs)
	}# EOIF
	
	##########
	# first: Who is more than once?
	saIdChar <- saId[,1]
	tval <- table(rle(saIdChar)$values)
	tNames <- which(tval > 1) # could be more than one !
	collListSaIdAdd <- vector("list", length(tNames))
	if (length(tNames) > 0) {
		for (i in seq_along(tNames)) {
			tiNa <- names(tNames)[i] # get the sampleId that is more than once present
			bbRl <- rle(stringr::str_count(saIdChar, tiNa))
			singleAddToSaId <- makeBadBoyAddition(bbRl)
			collListSaIdAdd[[i]] <- singleAddToSaId
		} # end for i
		dfAdd <- fuseAdditionsTogether(collListSaIdAdd) 
		ind <- which(colnames(C_cols) == C_colnSampleId)
		newDf <- cbind(C_cols[,ind],dfAdd)
		newVals <- apply(newDf, 1, function(x) paste(x, collapse=""))
		C_cols[,ind] <- newVals
	} # end if
	
	#########
	# now correct the consec scan nr
	if (length(tNames) > 0) {
		saIdNew <- C_cols[,2]
		conSNr[,1] <- genNewConSNrs(saIdNew)
	} # end if
	
	########
	# collect NIR
	NIR <- raw[,-1]
	prWls
	wlsInd <- startsWith(colnames(NIR), prefix = prWls)
	NIR <- as.matrix(NIR[,wlsInd])
	univSampId <- 
	rownames(NIR) <- niceRownames <- paste(univSaId, conSNr[,1], sep="_")
	
	#######
	# organize the rest
	rest <- raw[,-1][,which(!wlsInd)] # double subscription yea
	rest <- rest[,-3]
	instrSerNr <- rest[1,1]
	#
	Y_cols <- rest[colnTempIn] # device temperature goes into Y_cols
	colnames(Y_cols) <- colnDeviceTemp
	#
	tiStamp <- rest[,colnTsIn]
	timestamp <- as.data.frame(strptime(tiStamp, format=timeFormat)) #  the 24 hours format would be "%d/%m/%Y %H:%M:%S"
	colnames(timestamp) <- colnTiSta

	timePoints <- ecrm <- repl <- group <- temp  <- relHum <- NULL
	slType <- get(".slType", pos=gl_ap2GD) ## .slType gets assigned in readHeader_checkDefaults
	info <- list(nCharPrevWl=length(prWls))
	assign("instrumentInfo", paste0(txtMicroNirsAdd, instrSerNr), pos=parent.frame(n=2))
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


importTRH_HoboWare <- function(path, what) { # what can be "xls" or "csv"
	stn <- getstn()
	tiFormat <- stn$imp_timeFormat_HOBOware
	if (what == "csv") {
		TRH <-read.csv(path, header=FALSE)
	} else {
		TRH <- openxlsx::read.xlsx(path, detectDates=FALSE)
	} # end else
	TRH <- TRH[-c(1,2),c(2,3,4)] # only keep date, temp and relhum, skip the first two rows
	colnames(TRH) <- c("Time", "Temp", "RelHum")
	TRH$Temp <- as.numeric(TRH$Temp)
	TRH$RelHum <- as.numeric(TRH$RelHum)	
	if (what == "csv") {
		TRH$Time <- strptime(TRH$Time,format = tiFormat)
	} else {
		stop("Please use for the moment csv import for the data logger. Time import from xlsx is still not behaving... ")
		TRH$Time <- as.POSIXct(as.numeric(TRH$Time), origin = "1899-12-30") # the origin of xlsx time, but it does not work yet	
	} # end else
	#	print(str(TRH)); print(ht(TRH))
	if (any(is.na(TRH$Time))) {
		stop(paste0("Sorry, creating POSIX timestamps for the data logger import failed.\n   Is the time format as specified in 'imp_timeFormat_HOBOware' in the global settings file correct?\n   If import was done via .xlsx, you could try importing the data from .csv file."), call.=FALSE)
	} # end if
	return(TRH)
} # EOF
