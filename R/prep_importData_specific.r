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
	return(read.table(dataFile, na.strings=naStrings))
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
  a<-readBin(fCon, raw(), fromPosi+4)
  
  no <- no0 <- 0
  if (nRow > 29+32) {
    no0<-length(seq(29,nRow,32))
  }
  nrRowsAdd <- nRow+no0+2
  noData<-nrRowsAdd*(nColAll+0.1)
  DatatoRead <- noData
  if (nRow > 29+32) {
    no <- seq(29,noData,32)
    DatatoRead <- noData-61
  }
  
  a <- readBin(fCon, double(), size=4, DatatoRead)#52)
  close(fCon)
  if (nRow > 29+32){
    a <- a[-no]
  }
  lengtha <- length(a)
  a <- a[1:(nRow*nColAll)]
  lengtha <- lengtha-length(a)
  a[which(a==9.99999968028569246551e+37)] <- NA
  
  return(matrix(a, nRow, nColAll)) # NIR
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
    ClassVar = PirTable[,classCol]
  } else {
    ClassVar = NA
  }
  if (noColDepen > 0){
    depenCol <- c((ncol(PirTable) - noColDepen) : ncol(PirTable))
    DepenVar = PirTable[,depenCol]
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

pir_searchAskColumns <- function(allC_var, allY_var) {
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
	##
	stopMsg <- "Import aborted."
	stopDoubleMsg1 <- "The standard column name \""
	stopDoubleMsg2 <- " appears more than once in the Pirouette file. Please check your input. \n"
	msg1 <- "\n\nThe standard column name \""
	msg2 <-  "\" could not be found in the .pir file. \nWhich of the following columns does represent "
	msg3 <- "?\nPlease enter the appropriate number; type 0 for not represented; type any non-numeric to stop.\n"
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
	#### the C-variables ####
	for (i in 1: length(listElementNames_C)) {
		print(ht(allC_var))
		cn <- stdColumnNames_C[i]
		ind <- which(colnames(allC_var) == cn )
		cle <- length(colnames(allC_var))
		if (length(ind) == 0) {
	 		cat(paste(msg1, cn, msg2, cn, msg3, sep=""))
 			cat(notRep)
 			cat(paste(1:cle, " -- ", colnames(allC_var), "\n", sep=""))
 			nrOk <- FALSE
	 		while(!nrOk) {
 				a <- readLines(n=1)
 			 	options(warn=-1); a <- as.numeric(a); options(warn=0)
 		 		nrOk <- checkNumber(a, cle)
	 		} # end while
 			if (a == 0) {
 				assign(listElementNames_C[i], NULL, pos=parent.frame(n=1))
 			#	assign(listElementNames_C[i], NULL, pos=parent.frame(n=2))
	 		} else { # so we have a valid input giving a representation
				assign(listElementNames_C[i], as.data.frame(allC_var[, a]), pos=parent.frame(n=1))
 			#	assign(listElementNames_C[i], as.data.frame(allC_var[, a]), pos=parent.frame(n=2))
				allC_var <- as.data.frame(allC_var[,-a])
	 		}
		} else { # so we did find something
			if (length(ind) > 1 ) { # more than one
				stop(paste(stopDoubleMsg1, cn, stopDoubleMsg2, sep=""), call.=FALSE)
			} # if still here the index == 1
				assign(listElementNames_C[i], as.data.frame(allC_var[, ind]), pos=parent.frame(n=1))
 			#	assign(listElementNames_C[i], as.data.frame(allC_var[, ind]), pos=parent.frame(n=2))
				allC_var <- as.data.frame(allC_var[,-ind])
		} # end else 
	} # end for i
	### the Y-variables
	for (i in 1: length(listElementNames_Y)) {
		print(ht(allY_var))
		cn <- stdColumnNames_Y[i]
		ind <- which(colnames(allY_var) == cn )
		cle <- length(colnames(allY_var))
		if (length(ind) == 0) {
	 		cat(paste(msg1, cn, msg2, cn, msg3, sep=""))
 			cat(notRep)
 			cat(paste(1:cle, " -- ", colnames(allY_var), "\n", sep=""))
 			nrOk <- FALSE
	 		while(!nrOk) {
 				a <- readLines(n=1)
 			 	options(warn=-1); a <- as.numeric(a); options(warn=0)
 		 		nrOk <- checkNumber(a, cle)
	 		} # end while
 			if (a == 0) {
 				assign(listElementNames_Y[i], NULL, pos=parent.frame(n=1))
 			#	assign(listElementNames_Y[i], NULL, pos=parent.frame(n=2))
	 		} else { # so we have a valid input giving a representation
				assign(listElementNames_Y[i], as.data.frame(allY_var[, a]), pos=parent.frame(n=1))
 			#	assign(listElementNames_Y[i], as.data.frame(allY_var[, a]), pos=parent.frame(n=2))
				allY_var <- as.data.frame(allY_var[,-a])
	 		}
		} else { # so we did find something
			if (length(ind) > 1 ) { # more than one
				stop(paste(stopDoubleMsg1, cn, stopDoubleMsg2, sep=""), call.=FALSE)
			} # if still here the index == 1
				assign(listElementNames_Y[i], as.data.frame(allY_var[, ind]), pos=parent.frame(n=1))
 			#	assign(listElementNames_Y[i], as.data.frame(allY_var[, ind]), pos=parent.frame(n=2))
				allY_var <- as.data.frame(allY_var[,-ind])
		} # end else 		
	} # end for i
	# handle colnames; error when only one left in selection
	# handle rest of the columns - hand over to C_cols and Y_cols
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
	allC_var <- pir$ClassVar
	allY_var <- pir$DepenVar
	
	pir_searchAskColumns(allC_var, allY_var) # assigns all the necessary list elements in this frame !
	
	
	timestamp <-  NULL
#	sampleNr <- NULL
#	conSNr <- NULL
#	timePoints <- NULL
#	ecrm <- NULL
	C_cols <- NULL
	Y_cols <- NULL
#	repl <- NULL
#	group <- NULL
#	temp <- NULL
#	relHum <- NULL
	outList <- list(sampleNr=sampleNr, conSNr=conSNr, timePoints=timePoints, ecrm=ecrm, repl=repl, group=group, temp=temp, relHum=relHum, C_cols=C_cols, Y_cols=Y_cols, timestamp=timestamp, info=info, NIR=NIR)
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
