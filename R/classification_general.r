# core classFunc definitions -----------
coreClassCalc_LDA <- function(...) {
	return(MASS::lda(...))
} # EOF

coreClassCalc_QDA <- function(...) {
	return(MASS::qda(...))
} # EOF

coreClassCalc_FDA <- function(...) {
	return(mda::fda(...))
} # EOF

coreClassCalc_mclustDA <- function(...) {
	return(mclust::MclustDA(...)) # ! no formula interface here !
} # EOF

coreClassCalc_randomForest <- function(...) {
	return(randomForest::randomForest(...))
} # EOF

coreClassCalc_SVM <- function(...) {
	return(e1071::svm(...))
} # EOF

coreClassCalc_nnet <- function(...) {
	return(nnet::nnet(...))
} # EOF

getClassifierFunction <- function(char) {
	# pv: pv_classificationFuncs_XDA <- c("lda", "qda", "fda", "mclustda");   pv_allClassificationFuncs: + "randForest", "svm", "nnet"
	pvClass <- pv_allClassificationFuncs
	if (char == pvClass[1]) { # lda
		return(coreClassCalc_LDA)
	}
	if (char == pvClass[2]) { # qda
		return(coreClassCalc_QDA)
	}
	if (char == pvClass[3]) { # fda
		return(coreClassCalc_FDA)
	}
	if (char == pvClass[4]) { # mclustda
		return(coreClassCalc_mclustDA)
	}
	if (char == pvClass[5]) { # randForest
		return(coreClassCalc_randomForest)
	}
	if (char == pvClass[6]) { # svm
		return(coreClassCalc_SVM)
	}
	if (char == pvClass[7]) { # nnet
		return(coreClassCalc_nnet)
	}
} # EOF



# segment lists and dataset splitting ---------
createSegmentListForClassification <- function(header, nrSegs=10, classOn, stnLoc) { # looks in each group individually
	yPref <- stnLoc$p_yVarPref
	snCol <- stnLoc$p_sampleNrCol
	csnCol <- stnLoc$p_conSNrCol
	nrSegsSwitch <- 2
	## first see how many consecutive scans there are within each sample number (some (outlier) could have been removed)	
	snName <- paste0(yPref, snCol)
	snInd <- which(colnames(header) == snName)
	conSnInd <- which(colnames(header) == paste0(yPref, csnCol))
	classOnInd <- which(colnames(header) == classOn)
	headerDim <- header[,c(snInd, conSnInd, classOnInd)]
#	res <- plyr::ddply(headerDim, snName, nrow) 		## data frame with two columns, has the number of rows in in the second column
	res <- plyr::ddply(headerDim, c(snName, classOn), nrow) 		## data frame with three columns, has the number of rows in in the third column
	segList <- levChars <-  NULL
	currInd <- 1
	for (i in 1: nrow(res)) {
		indices <- currInd:( (currInd-1) + res[i,3] )
		currInd <- max(indices)+1
		segList <- c(segList, list(indices))  ## gives segList with all the same sample numbers (different cons. scans from the same sample number) in the same list element
		levChars <- c(levChars, as.character(res[i,2])) # the current character with one of the levels of classOn)
	} # end for i
#	if (length(segList) <= nrSegsSwitch) { # ???
#		return(nrow(header))
#	}
	# now group the segList according to the level character, as we want to make sure that we have equal samples from every group
	siLevs <- unique(levChars)
	groupedSegList <- list(); length(groupedSegList) <- length(siLevs)
	for (i in 1: length(siLevs)) {
		ind <- which(levChars == siLevs[i])
		groupedSegList[[i]] <- segList[ind]
	} # end for i
	## randomize
	minLeng <- min(unlist(lapply(groupedSegList, length)))
	if (nrSegs > minLeng) {
		nrSegs <- minLeng
	}
	groupedList <- list(); length(groupedList) <- length(groupedSegList)
	for (i in 1: length(groupedSegList)) {
		le <- length(groupedSegList[[i]])
		pool <- 1: le
		nrFull <- floor(le / nrSegs)
		indList <- NULL
		for (k in 1: nrSegs) {
			res <- sample(pool, nrFull)
			pool <- pool[-(which(pool %in% res))]
			indList <- c(indList, list(res))
		} # end for k
		if (length(pool) > 0) {
			for (k in 1: length(pool)) {
				indList[[k]] <- c(indList[[k]], pool[k]) # add the rest of the values to the seg list
			}
		}
		## map back to the segment list
#		out <- lapply(indList, function(x) unlist(segList[x]))
		out <- lapply(indList, function(x) unlist(groupedSegList[[i]][x]))
		groupedList[[i]] <- out
	} # end for i
	# now get together the indices from the single groups
	outList <- vector("list", nrSegs)
	for (i in 1: length(outList)) {
		for (k in 1: length(groupedList)) {
			outList[[i]] <- c(outList[[i]], groupedList[[k]][[i]])
		} 
	} # end for i
	outList <- lapply(outList, sort)
	aa <- unlist(lapply(outList, length))
	outList <- outList[order(aa)] # puts the (possibly) shortest segment on position #1
	return(outList) 		
} # EOF

makeOuterSplitList <- function(dataset, percTest, classOn, stnLoc=.ap2$stn) {
	nrOuterLoops <- round(100/percTest, 0)
	splitList <- createSegmentListForClassification(dataset$header, nrSegs=nrOuterLoops, classOn, stnLoc)
	return(splitList)
} # EOF

makeOuterSplitDatasets <- function(dataset, splitList, testInd=1) { # returns a list with two elements: $test and $cv; the testInd is the index of the testing group, the others are CV
	if (testInd > length(splitList)) {
		stop("The provided testIndex is exceeding the maximum available value from the splitList.", call.= TRUE)
	}
	if (length(testInd) > 1) {
		stop("Test testIndex can not be of size > 1", call.=TRUE)
	}
	cv <- 1: length(splitList)
	cv <- cv[cv != testInd]
	cvInds <- sort(unlist(splitList[cv]))
	testInds <- splitList[[testInd]]
	return(list(cv=dataset[cvInds], test=dataset[testInds]))
} # EOF



# universal looping outer and inner ---------
make_Xclass_model_CV_single <- function(dataset, classFunc, classOn) {
	fdf <- makeFlatDataFrame(dataset, classOn)
	mod <- classFunc(grouping ~ ., data=fdf)
	return(mod)
} # EOF


make_Xclass_models_CV_outer <- function(cvData, classFunc, valid, classOn) {
	if (valid == "LOO") {
		valid <- nrow(cvData)
	}
	segList <- createSegmentListForClassification(cvData$header, nrSegs=valid, classOn, stnLoc=.ap2$stn) # looks in each group individually
	modList <- vector("list", length(segList))
	for (i in 1: length(segList)) {
		modList[[i]] <- make_Xclass_model_CV_single(cvData[ segList[[i]] ], classFunc, classOn)
	} # end for i
	return(modList)
} # EOF

make_Xclass_models_boot <- function(cvData, classFunc, R, classOn) {
	# boot here please
} # EOF


make_Xclass_models_inner <- function(cvData, testData, classFunc, classOn, md, apCl, idString, stnLoc) {
	cvBootCutoff <- apCl$bootCutoff
	cvBootFactor <- apCl$bootFactor # the factor used for multiplying the number of observations in the group, resulting in the bootR value
	cvValid <- round(apCl$valid, 0) # round just to be sure 
	#
	neverBoot <- stnLoc$cl_gen_neverBootstrapForCV
	clPref <- stnLoc$p_ClassVarPref
	snColName <- stnLoc$p_sampleNrCol 
	snrCol <- paste0(clPref, snColName)
	#
	ind <- which(colnames(cvData$header) == snrCol)
	aa <- lapply(split(cvData$header, cvData$header[,classOn]), function(x) x[,ind]) # split into groups, then get the sampleNr column
	minNrow <-  min(unlist(lapply(aa, length))) #  returns the smallest nr of observations from all separate groups
	if (minNrow >= cvBootCutoff | neverBoot) {
		if (!stnLoc$allSilent) {cat(".")}
		mods <- make_Xclass_models_CV_outer(cvData, classFunc, valid=cvValid, classOn)
	} else {
		if (!stnLoc$allSilent) {cat(":")}
		mods <- make_Xclass_models_boot(cvData, classFunc, R=minNrow*cvBootFactor, classOn)
	}
	return(mods)
} # EOF


make_X_classif_models <- function(dataset, classFunc, md, apCl, classOn, idString=NULL, stnLoc) { 
	# will come from the ap
	doOuter <- apCl$testCV
	percTest <- apCl$percTest
	if (percTest <= 0 ) {
		doOuter <- FALSE
	}
	#
	outList <- vector("list", length(classOn))
	outListRealClassOn <- vector("list", length(classOn))
	for (k in 1: length(classOn)) {
		splitList <- makeOuterSplitList(dataset, percTest, classOn[k], stnLoc) # new split in CV and TEST data for every classOn variable
		if (!doOuter) {
			modsList <- vector("list", 1)
			realClOnList <- vector("list", 1)
			aa <- makeOuterSplitDatasets(dataset, splitList, testInd=1) # returns a list:  $test and $cv
			if (nlevels(aa$cv$header[,classOn[k]]) < 2) {
				mods <- NULL
				realClOnFill <- NULL
			} else {
				mods <- make_Xclass_models_inner(cvData=aa$cv, testData=aa$test, classFunc, classOn[k], md, apCl, idString, stnLoc)
				realClOnFill <- classOn[k]
			} # end if
			modsList[[1]] <- mods # filling in the mods for the outer loop
			realClOnList[[1]] <- realClOnFill
		} else {
			modsList <- vector("list", length(splitList))
			realClOnList <- vector("list", length(splitList))
			for (i in 1: length(splitList)) {	
				aa <- makeOuterSplitDatasets(dataset, splitList, testInd=i)	
				if (nlevels(aa$cv$header[,classOn[k]]) < 2) {
					mods <- NULL
					realClOnFill <- NULL
				} else {
					mods <- make_Xclass_models_inner(cvData=aa$cv, testData=aa$test, classFunc, classOn[k], md, apCl, idString, stnLoc)					
					realClOnFill <- classOn[k]
				}	
				modsList[[i]] <- mods # filling in the mods for the outer loop
				realClOnList[[i]] <- realClOnFill
			} # end for i
		} # end else
		outList[[k]] <- modsList
		outListRealClassOn[[k]] <- realClOnList
	} # end for k
	return(list(modsClOn=outList, realClOn=realClOnList))
} # EOF

make_X_classif_handoverType <- function(dataset, md, apCl, types, idString, priInfo, priTy="") { # this is the "general" handover function called in cube_makeModels.r; the incoming dataset from the set
	stnLoc=.ap2$stn
	classOn <- apCl$classOn
	#
	if (length(types) == 1) {add <- ""} else {add <- ""}
	outList <- vector("list", length(types))
	outListRealClOn <- vector("list", length(types))
	if (!stnLoc$allSilent) {cat(paste0("      calc. ", priInfo, ":"))}
	for (i in 1: length(types)) {		
		classFunc <- getClassifierFunction(types[i]) ####### here select the desired classifier method !! ######
		if (!stnLoc$allSilent) {cat(paste0(" ", priTy[i], add))}
			###
			aa <- make_X_classif_models(dataset, classFunc, md, apCl, classOn, idString, stnLoc) ##### make models #######
			outList[[i]] <- aa$modsClOn
			outListRealClOn[[i]] <- aa$realClOn
			###
	} # end for i
	if (!stnLoc$allSilent) {cat(" ok\n")}
	oid <- paste0(priInfo, "__", idString)
	return(list(modsTy=outList, realClassOn=NA, apCl=apCl, id=oid))
#	return(list(modsTy=outList, realClassOn=outListRealClOn, apCl=apCl, id=oid))
	#
	# the order of the lists, from outer to inner: 
	# daTypes, classOn, outerLoop (=TestCV), CV inner loop, [then list element of individual models]
} # EOF
