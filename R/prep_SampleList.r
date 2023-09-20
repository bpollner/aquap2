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

esl_checkDefaults <- function(form) {
	stn <- getstn()
	if (form == "def") {
		form <- stn$p_sampleListExportFormat
	} else {
		if (form != "txt" & form != "xls") {
			stop("Please provide either 'txt' or 'xls' to the 'form' argument to export either a tab-delimited text file or an Excel-file.", call.=FALSE)
		}
	}
	assign("form", form, pos=parent.frame(n=1))	
} # EOF


#### MASTER ##
#' @title Create and Export Sample Lists
#' @description Creates and exports the randomized sample list to file.
#' @details Possible formats to export are tab delimited text or Excel. 
#' When the appropriate values at the bottom of the settings.r file are provided, 
#' an approximate time estimate of how long it will take to work through the 
#' sample list is given. 
#' @aliases exportSampleList esl
#' @param md An object of class \code{aquap_md} containing the metadata of the 
#' experiment. Defaults to \code{getmd()}, what is calling the default filename 
#' for the metadata file. See \code{\link{getmd}} and \code{\link{metadata_file}}.
#' @param form Character, can be 'txt' to export the sample list in a tab 
#' delimited text file, 'xls' to export as an Excel file, or if left at the 
#' default 'def' is reading in the value from the settings.r file (parameter 
#' \code{p_sampleListExportFormat}).
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
exportSampleList <- function(md=getmd(), form="def", rnd=TRUE, showFirstRows=TRUE, timeEstimate=FALSE) {
	stn <- autoUpS()
	esl_checkDefaults(form)
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
	if(!stn$allSilent) {cat("Creating sample list...\n")}
	a <- makeTimeLabelSampleList(md, rnd)
	b <- insertEnumeration(a)
	##
		totalTimeInHours <- round((nrow(b)* totalTime) / (60*60),1)
	if (form == "txt") {
		ending <- "-out.txt"
		msg <- ".txt"
		toTab <- TRUE
	} else {
		ending <- "-out.xlsx"
		msg <- ".xlsx"
		toTab <- FALSE
	}
	filename <- paste(fn_sl, "/",fn_sl_out , "/", expName, ending, sep="")
	if(!stn$allSilent) {cat("Writing sample list to file...\n")}
	if (toTab) {
		write.table(b, filename, sep="\t", row.names=FALSE)	
	} else {
		openxlsx::write.xlsx(b, filename, rowNames=FALSE, sheetName=expName)
	} # end else
 	cat(paste("A sample list in ", msg, " format with ", nrow(b), " rows has been saved to \"", fn_sl, "/", fn_sl_out, "\".\n", sep="") )
	if (timeEstimate) {
		if (length(timeLabels) > 1) {
			cat("Minimum working time each time-point:", round((totalTimeInHours/length(timeLabels)), 1), "hours, in total", totalTimeInHours, "hours.\nHave fun!\n")
		} else {
			cat("Minimum working time:", totalTimeInHours, "hours.\nHave fun!\n")
		}# end else
	} # end if timeEstimate
	if (showFirstRows) {
		if (nrow(b) < 21 ) {pr <- nrow(b) } else {pr <- 21}
		cat("\n")
		print(b[1:pr,])
	}
	return(invisible(b))
} # EOF

#' @rdname exportSampleList
#' @export
esl <- function(md=getmd(), form="def", rnd=TRUE, showFirstRows=TRUE, timeEstimate=FALSE) {
	out <- exportSampleList(md, form, rnd, showFirstRows, timeEstimate)
} # EOF
####### / Master ################################################
############################################################################################


############################################################################################
############################################################################################
############################################################################################















