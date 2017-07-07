# core classFunc definitions -----------
coreClassCalc_LDA <- function(dfData, apCl=NULL) {
	return(MASS::lda(grouping ~ NIR, data=dfData))
} # EOF

coreClassCalc_QDA <- function(dfData, apCl=NULL) {
	return(MASS::qda(grouping ~ NIR, data=dfData))
} # EOF

coreClassCalc_FDA <- function(dfData, apCl=NULL) {
	return(mda::fda(grouping ~ NIR, data=dfData))
} # EOF

coreClassCalc_mclustDA <- function(dfData, apCl=NULL) { # ! no formula interface here !
	return(mclust::MclustDA(dfData$NIR, dfData$grouping)) 
} # EOF

coreClassCalc_randomForest <- function(dfData, apCl=NULL) {
	return(randomForest::randomForest(grouping ~ NIR, data=dfData))
} # EOF

coreClassCalc_SVM <- function(dfData, apCl=NULL) {
	return(e1071::svm(grouping ~ NIR, data=dfData))
} # EOF

coreClassCalc_nnet <- function(dfData, apCl=NULL) {
	return(nnet::nnet(grouping ~ NIR, data=dfData))
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
	if (percTest <= 0) {
		percTest <- 100 # so the below division still works and is resulting in 1
	}
	nrOuterLoops <- round(100/percTest, 0)
	splitList <- createSegmentListForClassification(dataset$header, nrSegs=nrOuterLoops, classOn, stnLoc)
	return(splitList)
} # EOF

makeOuterSplitDatasets <- function(dataset, splitList, testInd=1) { # returns a list with two elements: $test and $cv; the testInd is the index of the testing group, the others are CV
	if (testInd > length(splitList)) {
		stop("The provided testIndex is exceeding the maximum available value from the splitList.", call.= TRUE)
	}
	if (length(testInd) > 1) {
		stop("Test testIndex can not be of length > 1", call.=TRUE)
	}
	if (length(splitList) == 1) { # we might have zero percent as test data
		return(list(cv=dataset, test=NULL))
	}
	cv <- 1: length(splitList)
	cv <- cv[cv != testInd]
	cvInds <- sort(unlist(splitList[cv]))
	testInds <- splitList[[testInd]]
	return(list(cv=dataset[cvInds], test=dataset[testInds]))
} # EOF


# common mathematics -----------
calculate_Avg_SDs_confPercList <- function(confPercList, stnLoc=NULL) { # the input is a list with a confusion table in percent in every single list element
	rndAvg <- stnLoc$cl_gen_digitsRoundConfTablePerc
	rndSDs <- stnLoc$cl_gen_digitsRoundSDTablePerc
	#
	percDF <- plyr::ldply(confPercList, function(x) as.numeric(x)) # strings out each single confusion table into a singel row, then these rows rbind into a data frame
	aa <- confPercList[[1]] ; nr <- nrow(aa) ; nc <- ncol(aa); dn <- dimnames(aa) # just take the first one to get the dimensions
	avgsTable <- as.table(matrix(round(apply(percDF, 2, mean), rndAvg), nr, nc, dimnames=dn))
	SDsTable <- as.table(matrix(round(apply(percDF, 2, sd), rndSDs), nr, nc, dimnames=dn))
	return(list(avgsTable=avgsTable, SDsTable=SDsTable))
} # EOF

calculateAverageOfTablesList <- function(tableList, stnLoc=NULL) { # the input is a list with a table (avg or SD e.g.) in each list element
	rnd <- stnLoc$cl_gen_digitsRoundTableAverages
	#
	dataDF <- plyr::ldply(tableList, function(x) as.numeric(x)) # strings out each single table into a singel row, then these rows rbind into a data frame
	aa <- tableList[[1]] ; nr <- nrow(aa) ; nc <- ncol(aa); dn <- dimnames(aa) # just take the first one to get the dimensions
	avgOut <- as.table(matrix(round(apply(dataDF, 2, mean), rnd), nr, nc, dimnames=dn))
	return(avgOut)
} # EOF


# universal looping outer and inner ---------

makePcaVariableReduction <- function(dfTrain, dfPred=NULL, dfTest, apCl) { # dfPred can be NULL, as in the bootstrap case we do not have it
	nc <- apCl$pcaNComp # can be either "max" or the desired numbers
	if (any(nc == "max")) {
		nc <- 1: nrow(dfTrain) - 1
	}
	pcaObTrain <- stats::prcomp(dfTrain$NIR) # the default is to not scale and only mean-center
	NIR <- pcaObTrain$x[,nc]
	dfTrain$NIR <- I(NIR)
	if (!is.null(dfPred)) {
		NIR <- predict(pcaObTrain, newdata=dfPred$NIR)[,nc] # get back the scores for the prediction data
		dfPred$NIR <- I(NIR)
	}
	# we also have, in this case, to transform the test data into PCA space
	NIR <- predict(pcaObTrain, newdata=dfTest$NIR)[,nc] # the results are collected and then later the majority vote is done
	dfTest$NIR <- I(NIR)
	#########	# other version using ChemometricsWithR::PCA and its "project"
	#	pcaObTrain <- ChemometricsWithR::PCA(scale(dfTrain$NIR, scale=FALSE))
	#	NIR <- pcaObTrain$scores[,nc] # replace the NIR, select components via nc
	#	dfTrain$NIR <- I(NIR)
	#	NIR <- ChemometricsWithR::project(pcaObTrain, newdata=dfPred$NIR)[,nc]
	#	dfPred$NIR <- I(NIR)
	#########
	return(list(dfTrain=dfTrain, dfPred=dfPred, dfTest=dfTest))
} # EOF

make_Xclass_model_CV_single <- function(trainDataset, predDataset, testData, classFunc, classOn, type, apCl) { # inside the single steps of the x-fold *CV*, single models and predictions (in CV)
	dfTrain <- makeDataFrameForClassification(trainDataset, classOn) # ! is not flat
	dfPred <- makeDataFrameForClassification(predDataset, classOn) # ! is not flat
	dfTest <- makeDataFrameForClassification(testData, classOn) # still not flat
	#
	# possibly use PCA for data reduction
	if (apCl$pcaRed) {
		aa <- makePcaVariableReduction(dfTrain, dfPred, dfTest, apCl)
		dfTrain <- aa$dfTrain
		dfPred <- aa$dfPred
		dfTest <- aa$dfTest
	} # end if pcaRed
	#
	mod <-classFunc(dfTrain, apCl)
	pred <- predict(mod, newdata=dfPred) # the prediction of the one left out segment in the model made from all the other segments
	conf <- mda::confusion(pred, dfPred$grouping)
	confPerc <- sweep(conf, 2, apply(conf, 2, sum), "/") * 100  #  calculate the confusion table in percent
	corrClassPerc <- sum(diag(conf))/sum(conf)*100  # calculate the correct classification in percent
#	cat("\n"); print(confPerc); cat("\n"); print(corrClassPerc); cat("\n\n")
	cvBranch <- list(mod=mod, dfPred=dfPred, dfTest=dfTest, confPerc=confPerc, corrClassPerc=corrClassPerc)
	#
	predTestClass <- predict(mod, newdata=dfTest) # here we keep the predictions of classes (the vector with the factors)
	testBranch <- list(predTestClass=predTestClass, trueTestClass=dfTest$grouping) # trueTestClass is always the same, as the test data is not changing down here
	#
	return(list(cvBranch=cvBranch, testBranch=testBranch)) # output of function make_Xclass_model_CV_single
} # EOF

make_Xclass_models_CV_outer <- function(cvData, testData, classFunc, valid, classOn, type, apCl, stnLoc) { # inner loop via *CV*: making models and predictions
	if (valid == "LOO") {
		valid <- nrow(cvData)
	}
	segList <- createSegmentListForClassification(cvData$header, nrSegs=valid, classOn, stnLoc) # looks in each group individually
	# now, from this segList, use all except one list elements for training the model, and then the one for prediction --> collect models and predictions (errors)
	modList <- dfPredList <- confPercList <- corrClassPercList <- predTestClassList <- trueTestClassList <- vector("list", length(segList))
	indPool <- 1: length(segList)
	for (i in 1: length(segList)) { # cycling through the combinations of the n-fold CV
		predInd <- i
		trainInd <- indPool[indPool != predInd]		###### CORE ###### CORE #######
		aa <- make_Xclass_model_CV_single(cvData[ unlist(segList[trainInd]) ], cvData[ unlist(segList[predInd]) ], testData, classFunc, classOn, type, apCl)  # the individual (X-fold) crossvalidation models
		cvBranch <- aa$cvBranch
		modList[[i]] <- cvBranch$mod 
		dfPredList[[i]] <- cvBranch$dfPred
		confPercList[[i]] <- cvBranch$confPerc
		corrClassPercList[[i]] <- cvBranch$corrClassPerc
		testBranch <- aa$testBranch
		predTestClassList[[i]] <- testBranch$predTestClass # here we have the vectors (in factors) with the predictions of the test data that were done deeper down, because we might have had to transform the test data into PCA space
		trueTestClassList[[i]] <- testBranch$trueTestClass
	} # end for i
	# now, inside the lists, we have the single results of the individual CV steps
	#
	# get the single cell averages of the confusion in percent, use them to calculate the mean and SD
	aa <- calculate_Avg_SDs_confPercList(confPercList, stnLoc)
	avgsTable <- aa$avgsTable
	SDsTable <- aa$SDsTable
	#	
	# get the average of correct classifications in percent
	aa <- unlist(corrClassPercList)
	corrClassAvg <- mean(aa)
	corrClassSD <- sd(aa)
	# 								# as dfTest we only need the last one (all the test-data are the same down there), so we do not collect the test data and just take the last value in cvBranch$dfTest
	cvBranch <- list(mods=modList, dfPreds=dfPredList, dfTest=list(cvBranch$dfTest), errors=list(avg=avgsTable, SD=SDsTable), corrClass=list(avg=corrClassAvg, SD=corrClassSD))
	testBranch <- list(predTestClassList=predTestClassList, trueTestClass= trueTestClassList[[1]]) # all the list elements are identical, as the testData did not change down there
	return(list(cvBranch=cvBranch, testBranch=testBranch))  # the return of function make_Xclass_models_CV_outer
} # EOF

make_Xclass_models_boot_outer <- function(cvData, testData, classFunc, R, classOn, type, apCl, stnLoc) { # inner loop via *boot*: making  models
	dfTrain <- makeDataFrameForClassification(cvData, classOn) # ! is not flat
	dfTest <- makeDataFrameForClassification(testData, classOn) # still not flat
	# we want to produce many models via boot, then use the OOB samples for CV, then apply the test data to all the models and make a majority vote
	# as we (or at least I) can not get out the models from within boot, we will use the boot function only to generate the indices, then do everything else manually.
	# when applying the strata argument, it proved that that out-ob-bag samples are usually around 1/3 (avg ratio 2.8) of the nrow of the dataset, with all subgroups equally present.
	#	
	innerWorkings <- function(dfT, ind, APCL=apCl) { # dfT is the dataset 
	#	pool <- 1:nrow(dfT)
	#	oobInd <- pool[which(! pool %in% ind)]
	#	cat("\n")
	#	print(sort(ind)); print(sort(oobInd))
	#	a <- unlist(lapply(split(dfT[ind,], dfT[ind,]$grouping), nrow)); 	print(a)
	#	b <- unlist(lapply(split(dfT[oobInd,], dfT[oobInd,]$grouping), nrow)); print(b)
	#	print(length(ind)); print(length(oobInd)); cat(paste0("Ratio In/oob = ", round(length(ind) / length(oobInd),1))) 
	#	cat("\n\n")
		#
	#	mod <-classFunc(dfT[ind,], APCL) # subselect data via ind
	#	pred <- predict(mod, newdata=dfT[oobInd,])
		#
		out <- matrix(ind, nrow=1)
	#	rownames(out) <- NULL
		return(out)		
	} # EOIF
	thisR <- R
	bootResult <- boot::boot(dfTrain, innerWorkings, R=thisR, strata=dfTrain$grouping, parallel="no")   	### using bootstrap to just get the indices of the datasets
	indMat <- as.matrix(bootResult$t) # is a matrix with R rows (one row for every replicate) and the in-the-bag indices in the columns
	pool <- 1:ncol(indMat)
	oobIndList <- apply(indMat, 1, function(x) pool[which(! pool %in% x)]) # get the out-of-bag indices
#	a <- t(apply(indMat, 1, sort)); print(a[1:5, 1:12]);  print(str(oobIndList))
	##
	
	if (apCl$pcaRed) {
		aa <- makePcaVariableReduction(dfTrain, dfPred=NULL, dfTest, apCl)
		dfTrain <- aa$dfTrain
		dfTest <- aa$dfTest
	} # end if pcaRed

	
	
	modsList <- vector("list", nrow(indMat))
	for (i in 1: nrow(indMat)) { # make a model for every bootstrap iteration
		modsList[[i]] <- classFunc(dfTrain[indMat[i,], ], apCl)
	#	pred <- predict(mod, newdata=dfT[oobInd,])
	} # end for i nrow(indMat)
	
	# next moves: implement correct PCA spacing of data; make CV predictions, collect ... etc...
	
#	print(length(modsList))	
#	print(str(modsList, max.level=1))
	
	
	
	stop("so far", call.=FALSE)


	if (apCl$pcaRed) {
		aa <- makePcaVariableReduction(dfTrain, dfPred=NULL, dfTest, apCl)
		dfTrain <- aa$dfTrain
		dfTest <- aa$dfTest
	} # end if pcaRed








	
	
	


	#
	# get the single cell averages of the confusion in percent, use them to calculate the mean and SD
	aa <- calculate_Avg_SDs_confPercList(confPercList, stnLoc)
	avgsTable <- aa$avgsTable
	SDsTable <- aa$SDsTable
	#	
	# get the average of correct classifications in percent
	aa <- unlist(corrClassPercList)
	corrClassAvg <- mean(aa)
	corrClassSD <- sd(aa)
	# 								# as dfTest we only need the last one (all the test-data are the same down there), so we do not collect the test data and just take the last value in cvBranch$dfTest
	cvBranch <- list(mods=modList, dfPreds=dfPredList, dfTest=list(cvBranch$dfTest), errors=list(avg=avgsTable, SD=SDsTable), corrClass=list(avg=corrClassAvg, SD=corrClassSD))
	testBranch <- list(predTestClassList=predTestClassList, trueTestClass= trueTestClassList[[1]]) # all the list elements are identical, as the testData did not change down there
	return(list(cvBranch=cvBranch, testBranch=testBranch))  # the return of function make_Xclass_models_CV_outer
} # EOF

make_Xclass_models_inner <- function(cvData, testData, classFunc, classOn, md, apCl, idString, stnLoc, type) { # in the inner loop: deciding if via boot or not AND evaluate the testData predictions
	cvBootCutoff <- apCl$bootCutoff
	cvBootFactor <- apCl$bootFactor # the factor used for multiplying the number of observations in the group, resulting in the bootR value
	cvValid <- round(apCl$valid, 0) # round just to be sure 
	nrDig <- stnLoc$cl_gen_digitsRoundConfTablePerc
	nrCV <- apCl$valid
	cvIndicator <- stnLoc$cl_gen_CvIndicator
	bootIndicator <- stnLoc$cl_gen_bootIndicator
	#
	neverBoot <- stnLoc$cl_gen_neverBootstrapForCV
	clPref <- stnLoc$p_ClassVarPref
	snColName <- stnLoc$p_sampleNrCol 
	snrCol <- paste0(clPref, snColName)
	#
	ind <- which(colnames(cvData$header) == snrCol)
	aa <- lapply(split(cvData$header, cvData$header[,classOn]), function(x) x[,ind]) # split into groups, then get the sampleNr column
	minNrow <-  min(unlist(lapply(aa, length))) #  returns the smallest nr of observations from all separate groups 
	minNrowInCV <- minNrow / nrCV # ... divided by the nr of crossvalidations
	if (minNrowInCV >= cvBootCutoff | neverBoot) {
		if (!stnLoc$allSilent) {cat(cvIndicator)}
		innerList <- make_Xclass_models_CV_outer(cvData, testData, classFunc, valid=cvValid, classOn, type, apCl, stnLoc) ##### CORE ######
		method <- "CV"
	} else {
		if (!stnLoc$allSilent) {cat(bootIndicator)}
		bootR <- round(minNrow*cvBootFactor, 0)
		innerList <- make_Xclass_models_boot_outer(cvData, testData, classFunc, R=bootR, classOn, type, apCl, stnLoc) ##### CORE ######
		method <- "boot"
	}
	#
	# make the majority vote of the predicted test data, produce confusion table and calculate it into percent, pass it on uphill for averaging and SD there
	##### CORE #### ("averaging" the predictions of all available models (could come from CV or from boot))
	testBranch <- innerList$testBranch
	testPredDF <- apply(apply(plyr::ldply(testBranch$predTestClassList, function(x) as.character(x)), 2, sort), 2, rle) # make a nice data frame in character, sort the values and do rle (run length encoding), result is a list
	majorVote <- as.factor(unlist(lapply(testPredDF, function(x) x$values[which.max(x$lengths)]))) # get back the single character that appears most often in each column ! # XXX possibly here a random element when in a tie??
	##### CORE #### 
	testConf <- mda::confusion(majorVote, testBranch$trueTestClass) # all the list elements are identical, as the testData did not change down there
	testConfPerc <- round(sweep(testConf, 2, apply(testConf, 2, sum), "/") * 100, nrDig)  #  calculate the confusion table in percent
	testCorrClassPerc <- sum(diag(testConf))/sum(testConf)*100  # calculate the correct classification in percent
#	cat("\n"); print(testConfPerc); cat("\n"); cat("\n"); 
	return(list(cvBranch=innerList$cvBranch, testBranch=list(testConfPerc=testConfPerc, testCorrClassPerc=testCorrClassPerc), method=method)) # handing the individual conf. tables and the correct classif. of the test data in percent up
	# output of function make_Xclass_models_inner
} # EOF

make_X_classif_models <- function(dataset, classFunc, md, apCl, classOn, idString=NULL, stnLoc, type) { # going 1) through the classOn, and 2) outerSplit
	doOuter <- apCl$testCV
	percTest <- apCl$percTest
	if (percTest <= 0 ) {
		doOuter <- FALSE
	}
	#
	cvList <- testList <- outListRealClassOn <- cvBranchErrorsList <- cvBranchCorrClassList <- summaryList <- methodList <-  vector("list", length(classOn))
	for (k in 1: length(classOn)) {
		splitList <- makeOuterSplitList(dataset, percTest, classOn[k], stnLoc) # new split in CV and TEST data for every classOn variable
		if (!doOuter) {
			modsList <- vector("list", 1)
			realClOnList <- vector("list", 1)
			aa <- makeOuterSplitDatasets(dataset, splitList, testInd=1) # returns a list:  $test and $cv; the smallest dataset is always on place 1 for testing
			if (nlevels(aa$cv$header[,classOn[k]]) < 2) {
				mods <- NULL
				realClOnFill <- NULL
			} else {
				if (!stnLoc$allSilent) { cat(paste0(" ",classOn[k])) }
				mods <- make_Xclass_models_inner(cvData=aa$cv, testData=aa$test, classFunc, classOn[k], md, apCl, idString, stnLoc, type) ##### CORE ######
				realClOnFill <- classOn[k]
			} # end if
			modsList[[1]] <- mods # filling in the mods for the outer loop
			realClOnList[[1]] <- realClOnFill
		} else {
			modsList <- realClOnList <- testBranchList <- vector("list", length(splitList))
			if (!stnLoc$allSilent) { cat(paste0(" ",classOn[k])) }
			for (i in 1: length(splitList)) {	
				aa <- makeOuterSplitDatasets(dataset, splitList, testInd=i)	
				if (nlevels(aa$cv$header[,classOn[k]]) < 2) {
					mods <- NULL
					realClOnFill <- NULL
				} else {
					mods <- make_Xclass_models_inner(cvData=aa$cv, testData=aa$test, classFunc, classOn[k], md, apCl, idString, stnLoc, type)	##### CORE ######				
					realClOnFill <- classOn[k]
				}	
				modsList[[i]] <- mods$cvBranch # filling in the data and all the CV info from the outer loop
				cvBranchErrorsList[[i]] <- mods$cvBranch$errors # collect the cv errors and SDs (and correct classifications and their SDs, below) to average them
				cvBranchCorrClassList[[i]] <- mods$cvBranch$corrClass
				testBranchList[[i]] <- mods$testBranch
				realClOnList[[i]] <- realClOnFill
			} # end for i - going through the split list ###### end going through split list !!!! ###############
			#######
		} # end else (so we DO the outer loop, i.e. the CV of the test data)
		##########
		cvList[[k]] <- modsList
		methodList[[k]] <- mods$method
		#
		### from each run of the outer loop, i.e. the N rounds of producing crossvalidated models and testing them, average the cv errors and SDs, and the correct classification and their SDs. --> avg. of the inner loop avg.
		avgCvErrors <- calculateAverageOfTablesList(lapply(cvBranchErrorsList, function(x) x$avg), stnLoc) # first extract the element ($avg), then hand over to function to calculate averages of all tables. same below.
		avgCvSDs <- calculateAverageOfTablesList(lapply(cvBranchErrorsList, function(x) x$SD), stnLoc)
		avgAvgCvCorrectClass <- mean(unlist(lapply(cvBranchCorrClassList, function(x) x$avg))) # extract the elemtn ($avg) from the list, unlist and calculate mean. same below.
		avgSDsCvCorrectClass <- mean(unlist(lapply(cvBranchCorrClassList, function(x) x$SD)))
		summaryList[[k]] <- list(errors=list(avg=avgCvErrors, SD=avgCvSDs), corrClass=list(avg=avgAvgCvCorrectClass, SD=avgSDsCvCorrectClass))
		#
		### now, please, calculate the averages and SDs from the crossvalidated test data (that have been projected onto the N models from crossvalidation)
		testConfPercList <- lapply(testBranchList, function(x) x$testConfPerc) # extract the list element "testConfPerc"
		aa <- calculate_Avg_SDs_confPercList(testConfPercList, stnLoc)
		test_avgsTable <- aa$avgsTable
		test_SDsTable <- aa$SDsTable
		#
		aa <- unlist(lapply(testBranchList, function(x) x$testCorrClassPerc)) # extract the list element "testCorrClassPerc", gives out a vector
		testCorrClassAvg <- mean(aa)
		testCorrClassSD <- sd(aa)
		testList[[k]] <- list(testErrors=list(avg=test_avgsTable, SD=test_SDsTable), testCorrClass=list(avg=testCorrClassAvg, SD=testCorrClassSD))
		###
		outListRealClassOn[[k]] <- realClOnList
	} # end for k (cycling through the classOn values) ######### end going through the classOn values !!!! #################
	##############
	cvDualList <- list(cvSingle=cvList, cvSummary=summaryList, method=methodList)
	return(list(cvBranch=cvDualList, testBranch=testList, realClOn=realClOnList)) # output of function make_X_classif_models
} # EOF

make_X_classif_handoverType <- function(dataset, md, apCl, types, idString, priInfo, priTy="") { # going through types; called in cube_makeModels.r; the incoming dataset from the set
	stnLoc=.ap2$stn
	classOn <- apCl$classOn
	#
	if (length(types) == 1) {add <- ""} else {add <- ""}
	cvList <- testList <- outListRealClOn <- vector("list", length(types))
	if (!stnLoc$allSilent) {cat(paste0("      calc. ", priInfo, ":"))}
	for (i in 1: length(types)) {		
		classFunc <- getClassifierFunction(types[i]) ####### here select the desired classifier method !! ######
		if (!stnLoc$allSilent) {cat(paste0(" ", priTy[i], add, ":"))}
			###
			aa <- make_X_classif_models(dataset, classFunc, md, apCl, classOn, idString, stnLoc, type=types[i]) ##### make models ###### CORE ######	
			cvList[[i]] <- aa$cvBranch
			testList[[i]] <- aa$testBranch
			outListRealClOn[[i]] <- aa$realClOn
			###
	} # end for i
	if (!stnLoc$allSilent) {cat(" ok\n")}
	oid <- paste0(priInfo, "__", idString)
	return(list(cvBranch=cvList, testBranch=testList, realClassOn=NA, apCl=apCl, id=oid))
	#
	### the order of the cvBranch, from outer to inner: 
	# daTypes, classOn, outerLoop (=TestCV), CV inner loop, [then list element of individual models]
	#
	### the order of the testBranch, frou outer to inner:
	# daTypes, classOn; (then already the results: test errors, test CorrClass)
} # EOF

## next: make possible to have 0 percent test data / NOT do the outer loop in all combinations !
