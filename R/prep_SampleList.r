makeSingleClass <- function(L1, L1index, L2) {
	out <- NULL
	L1mat <- matrix()[-1]
	L2mat <- matrix()[-1]
	for (L1.cnt in 1: length(L1[[L1index]])) {
		nrCorrL2Objects <- length(L2[[L1index]][[L1.cnt]])
	#	cat("Nr of Corr L2 objects:", nrCorrL2Objects, "\n")
		L1content <- rbind(L1mat, matrix(L1[[L1index]][[L1.cnt]], nrCorrL2Objects))
#		L2CorrContent <- rbind(L2mat, matrix(unlist(L2[[L1index]][[L1.cnt]])) )
		L2CorrContent <- rbind(L2mat, matrix(L2[[L1index]][[L1.cnt]]) )
		d <- cbind(L1content,L2CorrContent)
		out <- rbind(out,d)
	} # end L1.cnt loop
	return(out)
} # EOF

multiplyClassWithNextClass <- function(prevClass, L1, nextL1Index, L2) {
	nextClass <- makeSingleClass(L1, nextL1Index, L2)
	#sizePrev <- nrow(prevClass)
	sizeNext <- nrow(nextClass)
	multipliedNextClass <- NULL
	for (i in 1: sizeNext){
		a <- matrix(nextClass[i,1],nrow(prevClass))
		b <- matrix(nextClass[i,2],nrow(prevClass))
		d <- cbind(a,b)
		multipliedNextClass <- rbind(multipliedNextClass,d)
	}#end for i
	multipliedPreviousClass <- NULL
	for (k in 1: sizeNext) {
		multipliedPreviousClass <- rbind(multipliedPreviousClass, prevClass)
	} # end of k loop
	out <- cbind(multipliedPreviousClass, multipliedNextClass)
	return(out)
} # EOF

makeAllClasses <- function(L1, L2) {
	out <- NULL
	firstClass <- makeSingleClass(L1, 1, L2)
	if (length(L1) > 1) {
		prevClass <- firstClass
		for (i in 2: length(L1)) {
			multClass <- multiplyClassWithNextClass(prevClass, L1, i, L2)
			out <-  multClass
			prevClass <- multClass
		} # end i loop
	} else {
		out <- firstClass 
	}
	return(out) ## is a matrix
} # EOF

makeReplications <- function(classMatrix, Repls) {
	amat <- NULL
	bmat <- NULL
	sizeClassMat <- nrow(classMatrix)
	for (i in 1: length(Repls)) {
		amat <- rbind(amat, classMatrix)
		b <- matrix(Repls[[i]], sizeClassMat)
		bmat <- rbind(bmat, b)
	} # end i loop
	out <- cbind(amat, bmat)
out
} # EOF

makeGroups <- function(sampleMatrix, Groups) {
	if (length(Groups) > 0) {
		amat <- NULL
		bmat <- NULL
		for (i in 1:length(Groups)) {
			amat <- rbind(amat, sampleMatrix)
			b <- matrix(Groups[[i]], nrow(sampleMatrix))
			bmat <- rbind(bmat, b)
		} # end for i
		out <- cbind(amat, bmat)
	} else {
		out <- sampleMatrix
	}
out
} # EOF

## the expClasses must be a list and in the order: L1, L2, Repls, Group
createOrderedSampleList <- function (expClasses) {
	allClasses <- makeAllClasses(expClasses$L1, expClasses$L2)
	withRepls <- makeReplications(allClasses, expClasses$Repls)
	withGroups <- makeGroups(withRepls, expClasses$Group)
	return(withGroups)
} #EOF

randomizeSampleList <- function(sampleList) {  
	rnd <- runif(nrow(sampleList))
	DF <- data.frame(sampleList, rnd)
	a <- DF[order(rnd),]
	out <- a[- ncol(DF)]
	rownames(out) <- 1:nrow(out)
	return(out) ## returns now a data frame !!?
} # EOF

###
createRandomizedSampleList <- function(expClasses, rnd) { ########### gives back a data frame
	out <- createOrderedSampleList(expClasses)
	if (rnd) {
		out <- randomizeSampleList(out)	
	}
	return(out)
} # EOF
###

insertEnvControls <- function(rndSampleList, postProc) {
	spacing <- postProc$spacing
	ECRMlabel <- postProc$ECRMLabel
	noSplitLabel <- postProc$noSplitLabel
	EClabel <- ECRMlabel[[1]]
	RMlabel <- ECRMlabel[[2]]
	rsl <- rndSampleList
	if (spacing > nrow(rsl)) {
		spacing <- nrow(rsl) -1
	}
	##
	DF <- data.frame(ECRM=RMlabel, rsl) # ad a first column all filled with the RM-label to the provided list
	##
	if (!is.logical(spacing)) { # can only come in as logical FALSE --> we WANT to insert env. control
		insertBlock <- data.frame(matrix(EClabel, nrow=length(noSplitLabel), ncol=ncol(DF)))
		names(insertBlock) <- names(DF)
		NrInserts <- floor(nrow(DF)/spacing)
		cIn <- seq(spacing+1, ((spacing+1)*NrInserts), by=(spacing+1)) ## the cutting index
		for (i in 1: (length(cIn)) ) {
			a <- DF[1:(cIn[i]-1) ,]
			b <- DF[ (cIn[i]  ) : (nrow(DF)), ]
			if ( any(is.na(b)) ) {  # so if we are over the limits ... not well tested !!! Possible mistake here !
				DF <- rbind(a, insertBlock)
			} else {
				DF <- rbind(a,insertBlock,b)
			}		
		} # end i loop
	DF <- rbind(insertBlock, DF)
	} # end !is.logical spacing
	rownames(DF) <- 1:nrow(DF)
	return(DF)
} # EOF

insertNoSplit <- function(sampleList, postProc) {
#	options(warn=-1)
	noSplitMat <- data.frame(NoSplit = matrix(postProc$noSplitLabel, nrow=nrow(sampleList) ) )	
	out <- cbind(sampleList, noSplitMat)
#	options(warn=0)
out
} # EOF

insertTRH <- function(sampleList) { # insert a column for Temperatur and RH (to be filled in by hand)
	stn <- getstn()	
	tempName <- stn$p_tempCol
	RHName <- stn$p_RHCol
	mat <- matrix("", nrow(sampleList), 2)
	DF <- data.frame(mat)
	names(DF) <- c(tempName, RHName)
	out <- cbind(sampleList, DF)
} # EOF

createSingleTimePointSampleList <- function(expMetaData, rnd) {
	stn <- getstn()
	delChar <- stn$p_deleteCol
	spacing <- expMetaData$postProc$spacing
	rndList <- createRandomizedSampleList(expMetaData$expClasses, rnd) 
	envList <- insertEnvControls(rndList, expMetaData$postProc) 
#	noSplitList <- insertNoSplit(envList, expMetaData$postProc)	
	TRHList <- insertTRH(envList)
	names(TRHList) <- cns <- expMetaData$meta$coluNames[-1]
	a <- which(grepl(delChar, cns, fixed=TRUE))					## deletes the columns having the defaule "DELETE" char (the L2 problem)
	if (length(a) != 0) {
		TRHList <- TRHList[, -a]
	}
	return(TRHList)
} # EOF

makeTimeLabelSampleList <- function(expMetaData, rnd) {
	timeLabels <- expMetaData$expClasses$timeLabels
	out <- NULL
	for (i in 1: length(timeLabels)) {
		singleTimeList <- createSingleTimePointSampleList(expMetaData, rnd)
		timeCol <- data.frame(matrix(timeLabels[[i]], nrow=nrow(singleTimeList)))
		colnames(timeCol) <- expMetaData$meta$coluNames[[1]]
		fuseSingleTime <- cbind(timeCol, singleTimeList)
		out <- rbind(out, fuseSingleTime)
	} # end i loop
	return(out)
} # EOF

insertEnumeration <- function(sampleList) {  # so that the row-numbers stay during export
	stn <- getstn()
	nrs <- seq(1: nrow(sampleList))
	mat <- matrix(nrs, nrow(sampleList))
	a <- data.frame(SampleNr=mat)
	colnames(a) <- paste(stn$p_yVarPref, stn$p_sampleNrCol, sep="")
	out <- data.frame(a, sampleList)
	out
} # EOF

insertErrorColumn <- function(sampleList) {
	stn <- getstn()
	yPref <- stn$p_yVarPref
	errCol <- paste0(yPref, gl_conScanError)
	#
	cns <- colnames(sampleList)
	sampleList <- cbind(sampleList, X="")
	colnames(sampleList) <- c(cns, errCol)
	return(sampleList)
} # EOF

sl_checkVals <- function(multiplyRows, rnd, showFirstRows, timeEstimate) {
	checkForLogic <- function(what, val) {
		if (length(val) != 1 & all(is.logical(val))) {
			stop(paste0("Please provide either 'TRUE' or 'FALSE' to the argument '", what, "'"), call.=FALSE)
		} # end if
	} # EOIF
	checkForLogic("multiplyRows", multiplyRows)
	checkForLogic("rnd", rnd)
	checkForLogic("showFirstRows", showFirstRows)
	checkForLogic("timeEstimate", timeEstimate)
	#
} # EOF

sl_multiplyRows_sys <- function(sampleList, md, stn) {
	#
	nrScans <- md$postProc$nrConScans
	multiList <- NULL
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
	ColNames[2] <- paste(stn$p_yVarPref, stn$p_conSNrCol, sep="")
	colnames(a) <- ColNames
	rownames(a) <- seq(1:nrow(a)) # correct all the rownames
	return(a)	
}# EOF

sl_writeNiceExcelFilePlease <- function(sampleList, filename, stn, md) {
	# check if we have error column:
	# a) if yes (that means we can not have Y_conSNr) we do NOT apply coloring to the lines
	# b) if no, that meas we DO have Y_conSNr, we will apply nice common color to the rows in every sample
	# extra: if more than one time point, add color to the different times
	##
	wsZoom <- 140
	boarderGrey <- "gray68"
	siSampleGrey <- "gray55"
	errBCol <- "gray40"
	errFillCol <- "lavenderblush"
	#
	grayCol1 <- "gray93"
	grayCol2 <- "gray99"
	colVec <- c(grayCol1, grayCol2)
	#
	tiCol1 <- "beige"
	tiCol2 <- "bisque"
	tiColVec <- c(tiCol1, tiCol2)
	#
	expName <- md$meta$expName
	yPref <- stn$p_yVarPref
	errCol <- paste0(yPref, gl_conScanError)
	clPrev <- stn$p_ClassVarPref
	tiCol <- stn$p_timeCol
	indTiCol <- 2 # the index of the time column. comes in fix as 2
	makeErrorBorder <- FALSE
	makeSampleBorder <- FALSE
	##
	wb <- openxlsx::createWorkbook("SampleList")
	openxlsx::addWorksheet(wb, sheetName=expName, zoom=wsZoom)	
	openxlsx::writeData(wb, sheet=expName, sampleList, rowNames=FALSE)
	#
	
	# now check for error column
	siSampleInd <- NULL
	cns <- colnames(sampleList)
	if (!errCol %in% cns) { # that means we DO have the Y_conSNr in the multiplied rows, so we need coloring
		indTiCol <- 3 # in that case we have inserted the number of cons scans, the time column moved one to the right
		uniSNr <- unique(sampleList[,1]) # so far, it is all in our hands, the sample number IS the first column
		colInd <- as.numeric(uniSNr%%2 == 0)+1 # gives alternating 1 or 2. Is weird like that. But fun. 
		for (i in 1: length(uniSNr)) {
			indRows <- which(sampleList[,1] == uniSNr[i])
			cellFillStyle <- openxlsx::createStyle(fgFill=colVec[colInd[i]]) # colInd is a long vector of alternating 1 and 2, that in turn is used to subselect the first or second from colVec
			openxlsx::addStyle(wb, sheet=expName, cellFillStyle, rows=indRows+1, cols = 1:ncol(sampleList), gridExpand=TRUE, stack=TRUE)    
			makeSampleBorder <- TRUE
			siSampleInd <- c(siSampleInd, indRows[1]) # get the row index of the first of each sample
			 				
		} # end for i 
	} else { # so we DO have an error column, we want a nice boarder separating it from the rest, and a color
		errInd <- which(cns == errCol)
		redErrFill <- openxlsx::createStyle(fgFill=errFillCol) # colInd is a long vector of alternating 1 and 2, that in turn is used to subselect the first or second from colVec
		openxlsx::addStyle(wb, sheet=expName, redErrFill, rows = 1: nrow(sampleList)+1, cols=errInd, gridExpand=TRUE, stack=TRUE)   
		makeErrorBorder <- TRUE # do that later please
	} # end else
	##
	
	# check for more than one time point
	uniTime <- unique(sampleList[,indTiCol])
	if (length(uniTime) > 1) { # we will color the time column
		tci <- 1		
		for (i in seq_along(uniTime)) {
			indRows <- which(sampleList[,indTiCol] == uniTime[i])
			timeFillStyle <- openxlsx::createStyle(fgFill=tiColVec[tci])
			if (tci == 1) {tci <- 2} else {tci <- 1}
			openxlsx::addStyle(wb, sheet=expName, timeFillStyle, rows=indRows+1, cols = indTiCol, gridExpand=TRUE, stack=TRUE)    				
		} # end for i
	} # end if
	#
	
	# make cell borders again
	borderStyle <- openxlsx::createStyle(border="TopBottomLeftRight", borderColour=boarderGrey)
	openxlsx::addStyle(wb, sheet=expName, borderStyle, rows=1:nrow(sampleList)+1, cols = 1:ncol(sampleList), gridExpand=TRUE, stack=TRUE)
	if (makeErrorBorder) { # because if we do it upstairs then the "TopBottomLeftRight" boarder from above would undo that
		ebs <- openxlsx::createStyle(border="left", borderColour=errBCol, borderStyle="thick")
		allRows <- 0: nrow(sampleList)+1
		openxlsx::addStyle(wb, sheet=expName, ebs, rows=allRows, cols = errInd, gridExpand=TRUE, stack=TRUE)    	
	} # end if
	if (makeSampleBorder) {
		siBorder <- openxlsx::createStyle(border="top", borderColour=boarderGrey, borderStyle="medium")
		openxlsx::addStyle(wb, sheet=expName, siBorder, rows=siSampleInd+1, cols = 1:ncol(sampleList), gridExpand=TRUE, stack=TRUE)   
	} # end if
	#
	openxlsx::setColWidths(wb, sheet=expName, cols = 1:ncol(sampleList), widths = "auto")
	ok <- openxlsx::saveWorkbook(wb, filename, overwrite = TRUE, returnValue=TRUE)
	return(ok)
} # EOF

#### MASTER ##
#' @title Create and Export Sample Lists
#' @description Creates and exports the randomized sample list to file.
#' @details The exclusive format for the sample list is xlsx. 
#' When the appropriate values at the bottom of the settings.r file are provided, 
#' an approximate time estimate of how long it will take to work through the 
#' sample list is given. 
#' @aliases exportSampleList esl
#' @param md An object of class \code{aquap_md} containing the metadata of the 
#' experiment. Defaults to \code{getmd()}, what is calling the default filename 
#' for the metadata file. See \code{\link{getmd}} and \code{\link{metadata_file}}.
#' @param multiplyRows Logical, defaults to \code{FALSE}. If each row in the 
#' generated sample list should be multiplied by the number of consecutive scans 
#' as specified at \code{nrConScans} in the metadata file. 
#' \itemize{
#' \item \code{FALSE} If left at the default \code{FALSE}, there is one row for 
#' each sample in the sample list. Potential abberations from the planned number 
#' of consecutive scans can be noted in the inserted column called 
#' \code{conScanError} by providing \code{±n}, with 'n' being the number the 
#' actual number of consecutive scans is differing from the intended. 
#' \item \code{TRUE} If \code{multiplyRows} is set to \code{TRUE}, each row in 
#' the generated sample list is multiplied by the number of consecutive scans 
#' as specified at \code{nrConScans} in the metadata file. In this case there 
#' is no error column allowing to denote errors, but the user has to manually 
#' delete or add rows for each sample where the number of consecutive scans is 
#' differing from the intended. If rows were added, each cell in the row has 
#' to be filled accordingly. 
#' }
#' Please also refer to \code{\link{getFullData}} and the explanations for the 
#' argument \code{multiplyRows} therein. 
#' @param rnd Logical, if the sample list should be randomized or not, defaults 
#' to TRUE. (Having a non-randomized sample list can be interesting to check the 
#' correctness of the sample list when designing the experiment.
#' @param showFirstRows Logical. If the first rows of the sample list should be 
#' displayed.
#' @param timeEstimate Logical. If time estimates should be displayed.
#' @section Note: You can provide your own values for how many seconds you 
#' need for a single scan etc. at the bottom of the settings.r file to make the 
#' time estimate valid.
#' @return An (invisible) data frame with a (randomized) sample list resp. this 
#' list saved to a file.
#' @family Import-Export
#' @examples
#' \dontrun{
#' metadata <- getmd()
#' sl <- exportSampleList(metadata)
#' sl <- esl() 	# is the same as above
#' sl <- esl(md=getmd("myMetadataFile.r"), form="txt", showFirstRows=FALSE)
#' # this would load the metadata from the file "myMetadataFile.r" and generate 
#' # a sample list based on these metadata. This would be a rare case, as usually 
#' # in an experiment home-folder you will have probably only one metadata file 
#' # but several analysis procedure files.
#' }
#' @family Core functions
#' @export exportSampleList
exportSampleList <- function(multiplyRows = FALSE, rnd=TRUE, md=getmd(), showFirstRows=TRUE, timeEstimate=FALSE) {
	stn <- autoUpS()
	fn_sl <- stn$fn_sampleLists
	fn_sl_out <- stn$fn_sampleListOut
		durationSingleScan <- stn$misc_durationSingleScan
		handlingTime <- stn$misc_handlingTime
	expName <- md$meta$expName
	timeLabels <- md$expClasses$timeLabels
	nrConScans <- md$postProc$nrConScans
		scanSeconds <- 	(nrConScans+1) * durationSingleScan 	## +1 because of the reference scan !
		totalTime <- scanSeconds + handlingTime
	##
	sl_checkVals(multiplyRows, rnd, showFirstRows, timeEstimate) 
	##
	if(!stn$allSilent) {cat("Creating sample list...\n")}
	aaa <- makeTimeLabelSampleList(md, rnd)
	bbb <- insertEnumeration(aaa)
	##
	totalTimeInHours <- round((nrow(bbb)* totalTime) / (60*60),1)
	if (multiplyRows) {
		bbb <- sl_multiplyRows_sys(bbb, md, stn) 
	} else {  # we do NOT want an error column if we export with multiplied rows
		bbb <- insertErrorColumn(bbb)
	} # end else
	ending <- "-out.xlsx"
	msg <- ".xlsx"
	filename <- paste(fn_sl, "/",fn_sl_out , "/", expName, ending, sep="")
	if(!stn$allSilent) {cat("Writing sample list to file...\n")}
	#
	ok <- sl_writeNiceExcelFilePlease(bbb, filename, stn, md)
	if (ok) {
		cat(paste("A sample list in ", msg, " format with ", nrow(bbb), " rows has been saved to \"", fn_sl, "/", fn_sl_out, "\".\n", sep="") )	
	} else {
		stop(paste0("Sorry, an error occured while trying to save the sample list to xlsx."), call.=FALSE)
	} # end else
 	#
	if (timeEstimate) {
		if (length(timeLabels) > 1) {
			cat("Minimum working time each time-point:", round((totalTimeInHours/length(timeLabels)), 1), "hours, in total", totalTimeInHours, "hours.\nHave fun!\n")
		} else {
			cat("Minimum working time:", totalTimeInHours, "hours.\nHave fun!\n")
		}# end else
	} # end if timeEstimate
	if (showFirstRows) {
		if (nrow(bbb) < 21 ) {pr <- nrow(bbb) } else {pr <- 21}
		cat("\n")
		print(bbb[1:pr,])
	}
	return(invisible(bbb))
} # EOF

#' @rdname exportSampleList
#' @export
esl <- function(multiplyRows = FALSE, rnd=TRUE, md=getmd(), showFirstRows=TRUE, timeEstimate=FALSE) {
	out <- exportSampleList(multiplyRows, rnd, md, showFirstRows, timeEstimate)
} # EOF
####### / Master ################################################
############################################################################################

sl_renameSingleLineSampleList <- function(slInFolder, filename, ext) {
	# we already did file existence checking upstairs
	singleLineSuffix <- "_singleLine"
	#
	ok <- file.rename(from=paste0(slInFolder, filename, ext), to=paste0(slInFolder, filename, singleLineSuffix, ext) )
	if (!ok) {
		stop(paste0("Sorry, the file '", paste0(filename, ext), "' could not be renamed."), call.=FALSE)	
	} # end if
} # EOF



#' @title Multiply Rows in Sample List
#' @description Reads in the sample list located in the folder 
#' \code{sampleLists/sl_in} and, if it contains only one row for each sample, 
#' is multiplying each row by the number of consecutive scans as defined in 
#' \code{nrConScans} in the metadata file. If a correction value in an error
#' column is present, it is applied when multiplying the rows of the sample list.
#' @inheritParams exportSampleList
#' @param overwrite Logical. If the single line sample list file should be 
#' overwritten. If \code{FALSE} (the default), the single line file remains in 
#' \code{sampleLists/sl_in}, but it will be renamed.
#' @return Invisible TRUE if the sample list was multiplied and the old sample 
#' list was renamed; invisible FALSE if not multiplication of the sample list 
#' took place.
#' @family Import-Export
#' @export
sampleList_multiplyRows <- function(md=getmd(), overwrite=FALSE) {
	stn <- autoUpS()
	slInFolder <- paste0(stn$fn_sampleLists, "/", stn$fn_sampleListIn, "/")
	#
	if (is(md, "aquap_md")) {
		filename <- md$meta$expName
	} else {
		stop("Please provide a valid metadata object of class 'aquap_md' to the argument 'md'", call.=FALSE)
	} # end else
	ext <- "-in.xlsx" # only xlsx left
	msgBody <- paste0("The sample list '", paste0(filename, ext, "'"))
	check_sl_existence(filename, ext)
	#
	sampleList <- openxlsx::read.xlsx(paste(slInFolder, filename, ext, sep="")) # ! character imports are NOT factors yet !!
	sampleList <- convertCColsToFactor(sampleList)	
	sampleList <- convertYColsToNumeric(sampleList)
	#
	nrScans <- md$postProc$nrConScans
	aaa <- gfd_possibly_multiplySampleListRows(sampleList, nrScans, multiplyRows="auto", doas=FALSE)
		sampleList <- aaa$sampleList
		isMult <- aaa$isMult
	if (isMult) {
		if (!overwrite) {
			sl_renameSingleLineSampleList(slInFolder, filename, ext) # stops if unsuccessful
		} # end if
		fullFiNa <- paste0(slInFolder, filename, ext)
		ok <- sl_writeNiceExcelFilePlease(sampleList, fullFiNa, stn,  md) # filename is complete here
		if (!ok) {
			stop(paste0("Sorry, an error occured while trying to save the sample list to xlsx."), call.=FALSE)
		} else {
			cat(paste0(msgBody, " was multiplied.\n"))
			return(invisible(TRUE)) 
		} # end else
	} else { # so no mult took place
		message(paste0(msgBody, " was not multiplied."))
		return(invisible(FALSE))
	} # end else
} # EOF


