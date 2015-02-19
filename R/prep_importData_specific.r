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
	out <- dfOut[, -(1:2)]
#	out <- dfOut
	NIR <- dfOut[, -(1:3)]
	nCharPrevWl <- 1
	addCols <- dfOut[3]
	info <- list(nCharPrevWl=nCharPrevWl)
	outList <- list(addCols=addCols, NIR=NIR, info=info)
	return(outList)
} # EOF 


# Spectra from tab delim .txt file ---------------------------------------
getNirData_plainText <- function(dataFile, naStrings="NA") {
	return(read.table(dataFile, na.strings=naStrings))
} # EOF



##########################################################################


# Temp & relHum from ESPEC -----------------------------------------------
importTRH_ESPEC <- function(path) {
	TRH <- read.table(path, header=F, skip=4)[,-c(2,5:10)] # this the temp file
	colnames(TRH) <- c("Time", "Temp", "RelHum")
	TRH$Time <- strptime(TRH$Time,format = "%Y/%m/%d %H:%M'%S")
	return(TRH)
} # EOF
