
makeSingleBlock <- function(ClassVar, header, metaData) {
	ind <- which(colnames(header)==ClassVar)
	levelsChar <- levels(header[,ind])
	classPrefix <- .ap2$stn$p_ClassVarPref
	strippedClassVar <- unlist(strsplit(ClassVar, classPrefix))[2]
	if (is.na(strippedClassVar)) {
		classPrefix <- .ap2$stn$p_yVarPref
		strippedClassVar <- unlist(strsplit(ClassVar, classPrefix))[2]
	}
	EClabel <- metaData$postProc$ECRMLabel[1]
#	if (get("stngs")$autoRemoveECs & (strippedClassVar != get("stngs")$ECRMCol))
	if (TRUE & (strippedClassVar != .ap2$stn$p_ECRMCol)) { 
		ind <- which(levelsChar==EClabel)
		if (length(ind) == 0) {		## so if is a logical(0) ((also possible: !length(ind) -- gives a TRUE or FALSE
			ind <- 0				## to avoid comparing against "integer(0)" in the next step
		}		
		if (ind > 0) { 		## so if there is the EC label in levelsChar
			levelsChar <- levelsChar[-ind]
		}
	}
	classCol <- matrix(ClassVar, nrow=length(levelsChar) , ncol=1)
	valueCol <- matrix(levelsChar)
	out <- cbind(classCol, valueCol)
	out
} # EOF

multiplyBlocks <- function(block1, block2) {
	multipliedNextBlock <- NULL
	for (i in 1: nrow(block2)) {
		a <- matrix(block2[i,1], nrow(block1))
		b <- matrix(block2[i,2], nrow(block1))
		d <- cbind(a,b)
		multipliedNextBlock <- rbind(multipliedNextBlock, d)
	} # end for i
	multipliedPrevBlock <- NULL
	for (k in 1: nrow(block2)) {
		multipliedPrevBlock <- rbind(multipliedPrevBlock, block1)
	} # end for k
	out <- cbind(multipliedPrevBlock, multipliedNextBlock)
} # EOF

makeSplitByClassFrame <- function(splitGroup, header, metaData) { ## makes a matrix with split-values in a row
	firstBlock <- multBlock <- makeSingleBlock(splitGroup[1], header, metaData)
	if (length(splitGroup) > 1) {
		previousBlock <- firstBlock
		for (i in 2: length(splitGroup)) {			## go through all the items in the split-group
			ClassVar <- splitGroup[i]
			nextBlock <- makeSingleBlock(ClassVar, header, metaData)
			multBlock <- multiplyBlocks(previousBlock, nextBlock)
			previousBlock <- multBlock
			} # end for i
	} # end if > 1
	colnames(multBlock) <- rep(c("class", "value"), ncol(multBlock)/2)
	multBlock
} # EOF







makeCompPattern <- function(header, md, ap) {
	cp <- cpt <- NULL
	splitGroup <- ap$ucl$splitClasses
	patt <- makeSplitByClassFrame(splitGroup, header, md)
	print(patt); stopHere()
	return(list(cp=cp, cpt=cpt))
} # EOF

# next moves: clarify and improve autoremove of EC (not good!), add check for existence of variables !!!

ap_cleanOutZeroValues <- function(ap, dataset) {
	cPref <- .ap2$stn$p_ClassVarPref
	yPref <- .ap2$stn$p_yVarPref
	noSplitCol <- .ap2$stn$p_commonNoSplitCol
	if (is.null(ap$ucl$splitClasses)) {
		stdCol <- paste(cPref, noSplitCol, sep="")
		ind <-which(colnames(dataset$header) == stdCol)
		if (length(ind) == 0) {
			stop("Sorry, it appears that you do not have the standard column \"", stdCol, "\" in your dataset.", call.=FALSE)
		}
		ap$ucl$splitClasses <- stdCol
	}
	if (is.null(ap$ucl$splitWl)) {
		wls <- getWavelengths(dataset)
		ap$ucl$splitWl <- paste(min(wls), "-to-", max(wls), sep="")
	}
	le <- nchar(cPref)
	cns <- colnames(dataset$header)
	cnsM <- substr(cns, 1, le)
	ind <- which(cnsM == cPref)
	if (is.null(ap$pca$colorBy)) {
		ap$pca$colorBy <- cns[ind]
	}
	if (is.null(ap$simca$simcOn)) {
		ap$simca$simcOn <- cns[ind]
	}
	if (is.null(ap$plsr$regressOn)) {
		le <- nchar(yPref)
		cnsM <- substr(cns, 1, le)
		ind <- which(cnsM == yPref)
		ap$plsr$regressOn <- cns[ind]
	}
	return(ap)
} # EOF

#' @title Generate Datasets and make Models
#' @description Generate several datasets by splitting up the original dataset 
#' according to the variables and values provided in the analysis procedure and 
#' then calculate the models as specified in the analysis procedure on all of the 
#' datasets.
#' @details XXX
#' @param dataset An object of class 'aquap_data'
#' @param md The metadata, an object of class 'aquap_md'
#' @param ap The analysis procedure, an object of class 'aquap_ap'
#' @return XXX
#' @family Core functions
#' @export
gdmm <- function(dataset, md=getmd(), ap=getap()) {
	autoUpS()
	ap <- ap_cleanOutZeroValues(ap, dataset)
	a <- makeCompPattern(dataset$header, md, ap)
	cp <- a$cp
	cpt <- a$cpt




} # EOF




makeSingleCompPattern <- function(singleSplitGroup, dataset, metaData, analysisProcedure) {
	patt <- makeSplitByClassFrame(singleSplitGroup, dataset, metaData)
	patt <- multiplyByWlSplits(patt, analysisProcedure)
	patt <- multiplyBySmoothing(patt, analysisProcedure)
	patt <- multiplyByConS(patt, analysisProcedure, metaData)
	patt <- multiplyBySDavMQ(patt, analysisProcedure)
	out <- patt
} # EOF
