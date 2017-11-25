# core classFunc definitions and general -----------
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

ctKeepData <- function(object, type, stnLoc) { # checks if for the current type, or generallay, we want to keep the data, if not returns NULL
	if (is.null(object)) {
		return(NULL)
	}
	alwaysKeepData <- stnLoc$cl_gen_alwaysKeepData
	desiredKeepers <- stnLoc$cl_gen_keepDataFor
	#
	if (alwaysKeepData) {
		return(object)
	}
	if (type %in% desiredKeepers) {
		return(object)
	} else {
		return(NULL)
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
		nrOuterLoops <- 1
	} else {
		nrOuterLoops <- round(100/percTest, 0)
	}
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
calculate_Avg_SDs_confPercList <- function(confPercList, stnLoc) { # the input is a list with a confusion table in percent in every single list element
	if (all(unlist(lapply(confPercList, is.null)))) {
		return(NULL)
	}	
	#
	rndAvg <- stnLoc$cl_gen_digitsRoundConfTablePerc
	rndSDs <- stnLoc$cl_gen_digitsRoundSDTablePerc
	#
	percDF <- plyr::ldply(confPercList, function(x) as.numeric(x)) # strings out each single confusion table into a singel row, then these rows rbind into a data frame
	aa <- confPercList[[1]] ; nr <- nrow(aa) ; nc <- ncol(aa); dn <- dimnames(aa) # just take the first one to get the dimensions
	avgsTable <- as.table(matrix(round(apply(percDF, 2, mean), rndAvg), nr, nc, dimnames=dn))
	SDsTable <- as.table(matrix(round(apply(percDF, 2, sd), rndSDs), nr, nc, dimnames=dn))
	return(list(avgsTable=avgsTable, SDsTable=SDsTable))
} # EOF

calculateAverageOfTablesList <- function(tableList, stnLoc) { # the input is a list with a table (avg or SD e.g.) in each list element
	if (all(unlist(lapply(tableList, is.null)))) {
		return(NULL)
	}	
	#
	rnd <- stnLoc$cl_gen_digitsRoundTableAverages
	#
	dataDF <- plyr::ldply(tableList, function(x) as.numeric(x)) # strings out each single table into a singel row, then these rows rbind into a data frame
	aa <- tableList[[1]] ; nr <- nrow(aa) ; nc <- ncol(aa); dn <- dimnames(aa) # just take the first one to get the dimensions
	avgOut <- as.table(matrix(round(apply(dataDF, 2, mean), rnd), nr, nc, dimnames=dn))
	return(avgOut)
} # EOF

calculateColwiseAvgOfNumericList <- function(numList, stnLoc) { # input is a list with a named numeric in each list element
	if (all(unlist(lapply(numList, is.null)))) {
		return(NA)
	}	
	#
	rnd <- stnLoc$cl_gen_digitsRoundNrObservations
	#
	dataDF <- plyr::ldply(numList, function(x) as.numeric(x))
	aa <- numList[[1]]; namesChar <- names(aa)
	avgOut <- round(apply(dataDF, 2, mean), rnd)
	names(avgOut) <- namesChar
	return(avgOut)
} # EOF

calculateMeanOfList <- function(liOb, stnLoc) { # wrapped in a function to be able to prevent the warnings in case of NULL input
	if (all(unlist(lapply(liOb, is.null)))) {
		return(NA)
	}
	rnd <- stnLoc$cl_gen_digitsRoundNrObservations
	# 
	out <- round(mean(unlist(liOb)), rnd)
	return(out)
} # EOF

calculateConfusionTableInPercent <- function(confTable, stnLoc) {
	if (is.null(confTable)) {
		return(NULL)
	}
	#
	rnd <- stnLoc$cl_gen_digitsRoundConfTablePerc
	#
	out <- round(sweep(confTable, 2, apply(confTable, 2, sum), "/") * 100, rnd)  #  calculate the confusion table in percent
	return(out)
} # EOF

calculateCorrectClassificationInPercent <- function(confTable, stnLoc) {
	if (is.null(confTable)) {
		return(NULL)
	}
	#
	rnd <- stnLoc$cl_gen_digitsRoundConfTablePerc
	#
	out <- round(sum(diag(confTable))/sum(confTable)*100, rnd)  # calculate the correct classification in percent
	return(out)	
} # EOF

performMajorityVote <- function(classList) {
	if (all(unlist(lapply(classList, is.null)))) {
		return(NULL)
	}
	predDF <- apply(apply(plyr::ldply(classList, function(x) as.character(x)), 2, sort), 2, rle) # make a nice data frame in character, sort the values and do rle (run length encoding), result is a list
	majorVote <- as.factor(unlist(lapply(predDF, function(x) x$values[which.max(x$lengths)]))) # get back the single character that appears most often in each column ! # XXX possibly here a random element when in a tie??
	return(majorVote)
} # EOF

makePcaVariableReduction <- function(dfTrain, dfPred=NULL, dfTest=NULL, apCl) { 
	nc <- apCl$pcaNComp # can be either "max" or the desired numbers
	if (any(nc == "max")) {
		nc <- 1: (nrow(dfTrain) - 1)
	} else { # so we only have numbers
		if (max(nc) >= nrow(dfTrain)) { # to prevent the user from manually providing more NCs than are available
			nc <- 1: (nrow(dfTrain) - 1)
		}
	} # end else
	pcaObTrain <- stats::prcomp(dfTrain$NIR) # the default is to not scale and only mean-center
	NIR <- pcaObTrain$x[,nc]
	dfTrain$NIR <- I(NIR)
	if (!is.null(dfPred)) {
		NIR <- predict(pcaObTrain, newdata=dfPred$NIR)[,nc] # get back the scores for the prediction data
		dfPred$NIR <- I(NIR)
	}
	# we also have, in this case, to transform the test data into PCA space
	if (!is.null(dfTest)) {
		NIR <- predict(pcaObTrain, newdata=dfTest$NIR)[,nc] # the results are collected and then later the majority vote is done
		dfTest$NIR <- I(NIR)
	}
	#########	# other version using ChemometricsWithR::PCA and its "project"
	#	pcaObTrain <- ChemometricsWithR::PCA(scale(dfTrain$NIR, scale=FALSE))
	#	NIR <- pcaObTrain$scores[,nc] # replace the NIR, select components via nc
	#	dfTrain$NIR <- I(NIR)
	#	NIR <- ChemometricsWithR::project(pcaObTrain, newdata=dfPred$NIR)[,nc]
	#	dfPred$NIR <- I(NIR)
	#########
	return(list(dfTrain=dfTrain, dfPred=dfPred, dfTest=dfTest, pcaObTrain=pcaObTrain))
} # EOF

makeConfusionTable <- function(pred, true) {
	if (is.null(pred)) {
		return(NULL)
	}
	return(mda::confusion(pred, true))
} # EOF

makeNamedGrpNumsAp2 <- function(header, classOn) { # the header input is from aquap2 dataformat header
	if (is.null(header)) {
		return(NULL)
	}
	#
	out <- unlist(lapply(lapply(split(header, header[,classOn]), function(x) x[,classOn]), length)) # gets back a named numeric with the numbers ob observations in each group
	return(out)
} # EOF


# universal looping outer and inner ---------
make_Xclass_model_CV_single <- function(trainDataset, predDataset, testData, classFunc, classOn, type, apCl, stnLoc) { # inside the single steps of the x-fold *CV*, single models and predictions (in CV)
	grpsCvPred <- unlist(lapply(lapply(split(predDataset$header, predDataset$header[,classOn]), function(x) x[,classOn]), length)) # gets back a named numeric with the numbers ob observations in each group
	dfTrain <- makeDataFrameForClassification(trainDataset, classOn) # ! is not flat
	dfPred <- makeDataFrameForClassification(predDataset, classOn) # ! is not flat
	dfTest <- pcaObTrain <- NULL # we can have 0 percent test data
	if (!is.null(testData)) {
		dfTest <- makeDataFrameForClassification(testData, classOn) # still not flat
	}
	#
	# possibly use PCA for data reduction
	if (apCl$pcaRed) {
		aa <- makePcaVariableReduction(dfTrain, dfPred, dfTest, apCl)
		dfTrain <- aa$dfTrain
		dfPred <- aa$dfPred
		dfTest <- aa$dfTest
		pcaObTrain <- aa$pcaObTrain
	} # end if pcaRed
	#
	mod <- classFunc(dfTrain, apCl)
	pred <- predict(mod, newdata=dfPred) # the prediction of the one left out segment in the model made from all the other segments
	conf <- makeConfusionTable(pred, dfPred$grouping)
	confPerc <- calculateConfusionTableInPercent(conf, stnLoc)
	corrClassPerc <- calculateCorrectClassificationInPercent(conf, stnLoc)
#	cat("\n"); print(confPerc); cat("\n"); print(corrClassPerc); cat("\n\n")
	cvBranch <- list(mod=mod, dfPred=dfPred, dfTest=dfTest, confPerc=confPerc, corrClassPerc=corrClassPerc, grpsCvPred=grpsCvPred, pcaObTrain=pcaObTrain)
	#
	testBranch <- NULL
	if (!is.null(dfTest)) {
		predTestClass <- predict(mod, newdata=dfTest) # here we keep the predictions of classes (the vector with the factors)
		testBranch <- list(predTestClass=predTestClass, trueTestClass=dfTest$grouping) # trueTestClass is always the same, as the test data is not changing down here
	}
	#
	return(list(cvBranch=cvBranch, testBranch=testBranch)) # output of function make_Xclass_model_CV_single
} # EOF

make_Xclass_models_CV_outer <- function(cvData, testData, classFunc, valid, classOn, type, apCl, stnLoc) { # inner loop via *CV*: making models and predictions
	doPar <- stnLoc$cl_CV_inParallel
	#
	if (valid == "LOO") {
		valid <- nrow(cvData)
	}
	segList <- createSegmentListForClassification(cvData$header, nrSegs=valid, classOn, stnLoc) # looks in each group individually
	if (doPar) {
		registerParallelBackend()
	} else {
		registerDoSEQ()
	}
	dopare <- checkHaveParallel()
	## #now, from this segList, use all except one list elements for training the model, and then the one for prediction --> collect models and predictions (errors)
	indPool <- 1: length(segList)
	parOutList <- foreach(i = 1: length(segList)) %dopar% {
		predInd <- i
		trainInd <- indPool[indPool != predInd]		###### CORE ###### CORE #######
		aa <- make_Xclass_model_CV_single(cvData[ unlist(segList[trainInd]) ], cvData[ unlist(segList[predInd]) ], testData, classFunc, classOn, type, apCl, stnLoc)  # the individual (X-fold) crossvalidation models
		cvBranch <- aa$cvBranch
		testBranch <- aa$testBranch
		singleRound <- list(mod=cvBranch$mod, dfPred=ctKeepData(cvBranch$dfPred, type, stnLoc), dfTest=ctKeepData(cvBranch$dfTest, type, stnLoc), confPerc=cvBranch$confPerc, corrClassPerc=cvBranch$corrClassPerc, predTestClass=testBranch$predTestClass, trueTestClass=testBranch$trueTestClass, grpsCvPred=cvBranch$grpsCvPred, pcaObTrain=cvBranch$pcaObTrain)
	} # end dopar
	###
	# now we have to re-sort the resulting list (parOutList)
	modList <- dfPredList <- dfTestList <- confPercList <- corrClassPercList <- predTestClassList <- trueTestClassList <- grpsCvPredList <- pcaObTrainList <- list()	
	for (i in 1: length(parOutList)) { # resort the list from parOutList
		modList <- c(modList, list(parOutList[[i]]$mod))
		dfPredList <- c(dfPredList, list(parOutList[[i]]$dfPred))
		dfTestList <- c(dfTestList, list(parOutList[[i]]$dfTest))
		confPercList <- c(confPercList, list(parOutList[[i]]$confPerc))
		corrClassPercList <- c(corrClassPercList, list(parOutList[[i]]$corrClassPerc))
		predTestClassList <- c(predTestClassList, list(parOutList[[i]]$predTestClass))		
		trueTestClassList <- c(trueTestClassList, list(parOutList[[i]]$trueTestClass))
		grpsCvPredList <- c(grpsCvPredList, list(parOutList[[i]]$grpsCvPred))
		pcaObTrainList <- c(pcaObTrainList, list(parOutList[[i]]$pcaObTrain))		
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
	#
	# calculate the colwise average of the groupings for the prediction data
	grpsCvPredAvg <- calculateColwiseAvgOfNumericList(grpsCvPredList, stnLoc)
	#
	cvBranch <- list(mods=modList, dfPreds=dfPredList, dfTest=dfTestList, pcaObTrain=pcaObTrainList, errors=list(avg=avgsTable, SD=SDsTable), corrClass=list(avg=corrClassAvg, SD=corrClassSD), grpsCvPredAvg=grpsCvPredAvg)
	testBranch <- list(predTestClassList=predTestClassList, trueTestClass= trueTestClassList[[1]]) # all the list elements are identical, as the testData did not change down there
	return(list(cvBranch=cvBranch, testBranch=testBranch))  # the return of function make_Xclass_models_CV_outer
} # EOF

make_Xclass_models_boot <- function(cvData, testData, classFunc, R, classOn, type, apCl, stnLoc) { # inner loop via *boot*: making  models
	doPar <- stnLoc$cl_boot_inParallel
	yPref <- stnLoc$p_yVarPref
	conSnCol <- stnLoc$p_conSNrCol
	snrCol <- stnLoc$p_sampleNrCol
	snrColname <- paste0(yPref, snrCol)
	#
	dfTrain <- makeDataFrameForClassification(cvData, classOn) # ! is not flat
	dfTest <- NULL # we can have 0 percent test data
	if (!is.null(testData)) {	
		dfTest <- makeDataFrameForClassification(testData, classOn) # still not flat
	}
	# we want to produce many models via boot, then use the OOB samples for CV, then apply the test data to all the models and make a majority vote
	# as we (or at least I) can not get out the models from within boot, we will use the boot function only to generate the indices, then do everything else manually.
	# when applying the strata argument, it shows that that out-ob-bag samples are usually around 1/3 (avg ratio 2.8) of the nrow of the dataset, with all subgroups equally present. (detailed code for that in innerWorkings)
	# we are here in a single step of the outer loop
	#
	# we want to always exclude resp. include the consecutive scans together
	cvDataCS1 <- ssc_s(cvData, paste0(yPref, conSnCol), 1) # get all the observations that have the cons. scan number 1
	dfCS1 <- makeDataFrameForClassification(cvDataCS1, classOn)
	innerWorkings <- function(dfT, ind) { # dfT is the dataset 
		if (FALSE) { # for devTesting only
			pool <- 1:nrow(dfT)
			oobInd <- pool[which(! pool %in% ind)]
			cat("\n")
			cat(paste0("Nr of samples drawn: ", length(ind), "; Nr or observations: ", nrow(dfT),"\n"))
	#		print(sort(ind)); print(sort(oobInd))
			a <- unlist(lapply(split(dfT[ind,], dfT[ind,]$grouping), nrow)); 	print(a)
			b <- unlist(lapply(split(dfT[oobInd,], dfT[oobInd,]$grouping), nrow)); print(b)
			cat(paste0("Nr of samples: ", length(ind), "\n")); cat(paste0("Nr oob: ", length(oobInd), "\n")); cat(paste0("Ratio In/oob = ", round(length(ind) / length(oobInd),1))) 
			cat("\n\n")
		} # end DEV
		return(matrix(ind, nrow=1))		
	} # EOIF
	thisR <- R
	bootResCS1 <- boot::boot(dfCS1, innerWorkings, R=thisR, strata=dfCS1$grouping, parallel="no")   	### using bootstrap to just get the indices of the dataset (before, dfTrain was used as data)
	snrs <- cvDataCS1$header[, snrColname] # get all the sample numbers from the dataset that was reduced to cons. scan nr.1 
	snrsInd <- t(apply(bootResCS1$t, 1, function(x, snrs) snrs[x], snrs=snrs)) # get back only those sample numbers that we want in the boot result (in the bag). The boot results are in $t.
	snrsAll <- cvData$header[, snrColname] # get all the sample numbers in all consecutive scans
	indListOuter <- vector("list", nrow(snrsInd))
	for (i in 1: nrow(snrsInd)) { # going through the rows, i.e. bootstrap replicates; this for-loop is to ensure that we only exclude resp. include all consecutive scans together
		le <- length(snrsInd[i,])
		indListInner <- vector("list", le)
		for (k in 1: le) { # within a single bootstrap replicate, ...
			indListInner[[k]] <- which(snrsAll == snrsInd[i,k]) # ... mapping back to the indices of the original dataset (have to do it one by one [i,k] to get all the repeated samples)
		} # end for k
		indListOuter[[i]] <- indListInner
	} # end for i
	indList <- lapply(indListOuter, function(x) sort(unlist(x))) # we now have a list with the length of the bootstrap replicates, in each list element the number of observations indices
	indMat <- matrix(unlist(indList), nrow=length(indList), byrow=TRUE)	# transform the list back to matrix; # now, in indMat, we have always excluded resp. included the consecutive scans together
	pool <- 1:ncol(indMat)
	oobIndList <- apply(indMat, 1, function(x) pool[which(! pool %in% x)]) # get the out-of-bag indices, have them in a list
	if (FALSE) { ## for DEV only
		ratioCollect <- vector("numeric", nrow(indMat))
		for (i in 1: nrow(indMat)) {
			cat("\n")
			cat(paste0("Nr of samples drawn: ", length(indMat[i,]), "; Nr or observations: ", nrow(dfTrain),"\n"))
			a <- unlist(lapply(split(dfTrain[indMat[i,],], dfTrain[indMat[i,],]$grouping), nrow)); 	print(a)
			b <- unlist(lapply(split(dfTrain[oobIndList[[i]],], dfTrain[oobIndList[[i]],]$grouping), nrow)); print(b)
			ratio <- length(indMat[i,]) / length(oobIndList[[i]])
			ratioCollect[i] <- ratio
			cat(paste0("Nr of samples: ", length(indMat[i,]), "\n")); cat(paste0("Nr oob: ", length(oobIndList[[i]]), "\n")); cat(paste0("Ratio In/oob = ", round(ratio,1))) 
			cat("\n\n")
		} # end for i
		cat(paste0("Average ratio In/oob: ", round(mean(ratioCollect),1), "\n"))		
	} # end DEV
	if (doPar) {
		registerParallelBackend()
	} else {
		registerDoSEQ()
	}
	dopare <- checkHaveParallel()
	parOutList <- foreach(i = 1:nrow(indMat)) %dopar% {   # make a model for every bootstrap iteration (one in every row), the result comes back in a list
		siDfTrain <- dfTrain[indMat[i,],] # get the in-the-bag data
		siDfPred <- dfTrain[oobIndList[[i]],] # get the out-of-bag data; around 1/3 of the original dfTrain
		grpsBootPred <- unlist(lapply(lapply(split(siDfPred$grouping, siDfPred$grouping), as.character), length))
		siDdfTest <- dfTest # no change yet
		pcaObTrain <- NULL
		if (apCl$pcaRed) {
			aa <- makePcaVariableReduction(siDfTrain, siDfPred, siDdfTest, apCl)
			siDfTrain <- aa$dfTrain
			siDfPred <- aa$dfPred
			siDdfTest <- aa$dfTest
			pcaObTrain <- aa$pcaObTrain
		} # end if pcaRed
		mod <- classFunc(siDfTrain, apCl)
		pred <- predict(mod, newdata=siDfPred) # the prediction of the left out observations in the model made from all the other observations
		conf <- makeConfusionTable(pred, siDfPred$grouping)
		confPerc <- calculateConfusionTableInPercent(conf, stnLoc)
		corrClassPerc <- calculateCorrectClassificationInPercent(conf, stnLoc)
		predTestClass <- NULL
		if (!is.null(siDdfTest)) {
			predTestClass <- predict(mod, newdata=siDdfTest)
		}
		singleRound <- list(mod=mod, dfPred=ctKeepData(siDfPred, type, stnLoc), dfTest=ctKeepData(siDdfTest, type, stnLoc), confPerc=confPerc, corrClassPerc=corrClassPerc, predTestClass=predTestClass, grpsBootPred=grpsBootPred, pcaObTrain=pcaObTrain)
	} # end dopar
	###
	# now we have to re-sort the resulting list (parOutList)
	modList <- dfPredList <- dfTestList <- confPercList <- corrClassPercList <- predTestClassList <- grpsBootPredList <- pcaObTrainList <- list()	
	for (i in 1: length(parOutList)) { # resort the list from parOutList
		modList <- c(modList, list(parOutList[[i]]$mod))
		dfPredList <- c(dfPredList, list(parOutList[[i]]$dfPred))
		dfTestList <- c(dfTestList, list(parOutList[[i]]$dfTest))
		confPercList <- c(confPercList, list(parOutList[[i]]$confPerc))
		corrClassPercList <- c(corrClassPercList, list(parOutList[[i]]$corrClassPerc))
		predTestClassList <- c(predTestClassList, list(parOutList[[i]]$predTestClass))	
		grpsBootPredList <- c(grpsBootPredList, list(parOutList[[i]]$grpsBootPred))	
		pcaObTrainList <- c(pcaObTrainList, list(parOutList[[i]]$pcaObTrain))	
	} # end for i	
	#
	# we are here in a single step of the outer loop
	# collect all the models, (all the dfPreds and dfTests), average the confusion tables of all bootstrap iterations
	#
	# get the single cell averages of the confusion in percent, use them to calculate the mean and SD
	aa <- calculate_Avg_SDs_confPercList(confPercList, stnLoc) # the bootstrap data from a single step of the outer loop
	avgsTable <- aa$avgsTable
	SDsTable <- aa$SDsTable
	#	
	# get the average of correct classifications in percent
	aa <- unlist(corrClassPercList)
	corrClassAvg <- mean(aa)
	corrClassSD <- sd(aa)
	#
	# calculate the colwise average of the groupings for the boot-styled prediction data
	grpsBootPredAvg <- calculateColwiseAvgOfNumericList(grpsBootPredList, stnLoc)
	#
	cvBranch <- list(mods=modList, dfPreds=dfPredList, dfTest=dfTestList, pcaObTrain=pcaObTrainList, errors=list(avg=avgsTable, SD=SDsTable), corrClass=list(avg=corrClassAvg, SD=corrClassSD), grpsCvPredAvg=grpsBootPredAvg)
	testBranch <- list(predTestClassList=predTestClassList, trueTestClass=dfTest$grouping) 
	return(list(cvBranch=cvBranch, testBranch=testBranch))  # the return of function make_Xclass_models_boot
} # EOF

make_Xclass_models_inner <- function(cvData, testData, classFunc, classOn, md, apCl, idString, stnLoc, type) { # in the inner loop: deciding if via boot or not AND evaluate the testData predictions
	cvBootCutoff <- apCl$bootCutoff
	cvBootFactor <- apCl$bootFactor # the factor used for multiplying the number of observations in the group, resulting in the bootR value
	cvValid <- round(apCl$valid, 0) # round just to be sure 
	nrDig <- stnLoc$cl_gen_digitsRoundConfTablePerc
	cvIndicator <- stnLoc$cl_gen_CvIndicator
	bootIndicator <- stnLoc$cl_gen_bootIndicator
	neverBoot <- stnLoc$cl_gen_neverBootstrapForCV
	enfCritVal <- stnLoc$cl_gen_enforceCriticalValue
	grpFac <- stnLoc$cl_gen_factorMinGrp	
	rndAno <- stnLoc$cl_gen_digitsRoundNrObservations
	#
	clPref <- stnLoc$p_ClassVarPref
	snColName <- stnLoc$p_sampleNrCol 
	snrCol <- paste0(clPref, snColName)
	#
	ind <- which(colnames(cvData$header) == snrCol)
	critLowerValue <- length(unique(cvData$header[,classOn])) * grpFac # the number of groups times the factor
	aa <- lapply(split(cvData$header, cvData$header[,classOn]), function(x) x[,ind]) # split into groups, then get the **sampleNr** column
	minNrow <-  min(unlist(lapply(aa, length))) #  returns the smallest nr of observations from all separate groups 
	minNrowInCV <- floor(minNrow / cvValid) # ... above, divided by the nr of crossvalidations
	innerList <- testBranch <- nrsCvTrain <- nrsCvPred <- NULL # in case neither CV nor boot below is not excecuted, i.e. the critical value is not reached
	method <- "" ; doThisCV <- TRUE; doThisBoot <- TRUE 
	#
	if (minNrowInCV >= cvBootCutoff | neverBoot) { # so we have enough for CV
		if (enfCritVal) { # if the critical value should be enforced or not
			if (critLowerValue > minNrowInCV ) {
				doThisCV <- FALSE
				message(paste0("Critical value (", critLowerValue, ") for crossvalidation not reached. Classification on `", classOn, "` not performed.\nTurn off this behaviour at `cl_gen_enforceCriticalValue` in the settings.r file."))
			}
		} # end if enfCritVal
		if (doThisCV) {
			if (!stnLoc$allSilent) {cat(cvIndicator)}
			innerList <- make_Xclass_models_CV_outer(cvData, testData, classFunc, valid=cvValid, classOn, type, apCl, stnLoc) ##### CORE ######
#			method <- paste0("CV.", cvValid, ", grp.min=", minNrowInCV)
			method <- paste0("cv.", cvValid)
			nrsCvTrain <- round((nrow(cvData)/cvValid)*(cvValid-1), rndAno)
			nrsCvPred <- round(nrow(cvData)/cvValid, rndAno)
		} # end doThisCV
	} else { # so we are going for boot
		if (enfCritVal) { # if the critical value should be enforced or not
			if (critLowerValue > floor(minNrow/3)) {
				doThisBoot <- FALSE
				message(paste0("Critical value (", critLowerValue, ") for boot-styled CV not reached. Classification on `", classOn, "` not performed.\nTurn off this behaviour at `cl_gen_enforceCriticalValue` in the settings.r file."))				
			}
		} # end if enfCritVal
		if (doThisBoot) {
			if (!stnLoc$allSilent) {cat(bootIndicator)}
			bootR <- round(minNrow*cvBootFactor, 0)
			innerList <- make_Xclass_models_boot(cvData, testData, classFunc, R=bootR, classOn, type, apCl, stnLoc) ##### CORE ######
#			method <- paste0("boot.", bootR, ", oob.min=", floor(minNrow / 3 ))  # it showed that via the bootstrap on average 1/3 of the data are kept out of the bag
			method <- paste0("boot.", bootR, " ", cvBootFactor, "x", minNrow) 
			nrsCvTrain <- nrow(cvData)
			nrsCvPred <- round(nrow(cvData)/3, rndAno)			
		} # end doThisBoot
	} # end else
	#
	# make the majority vote of the predicted test data, produce confusion table and calculate it into percent, pass it on uphill for averaging and SD there
	##### CORE #### ("averaging" the predictions of all available models (could come from CV or from boot))
	testBranch <- innerList$testBranch
	majorVote <- performMajorityVote(testBranch$predTestClassList)
	##### CORE #### 
	testConf <- makeConfusionTable(majorVote, testBranch$trueTestClass) #  all the list elements are identical, as the testData did not change down there
	testConfPerc <- calculateConfusionTableInPercent(testConf, stnLoc)
	testCorrClassPerc <- calculateCorrectClassificationInPercent(testConf, stnLoc)
#	cat("\n"); print(testConfPerc); cat("\n"); cat("\n"); 
	numbersList <- list(nrsCvTrain=nrsCvTrain, nrsCvPred=nrsCvPred, grpsCvPred=innerList$cvBranch$grpsCvPredAvg)
	return(list(cvBranch=innerList$cvBranch, testBranch=list(testConfPerc=testConfPerc, testCorrClassPerc=testCorrClassPerc), method=method, numbersCv=numbersList)) # handing the individual conf. tables and the correct classif. of the test data in percent up
	# output of function make_Xclass_models_inner
} # EOF

make_X_classif_models <- function(dataset, classFunc, md, apCl, classOn, idString=NULL, stnLoc, type) { # going 1) through the classOn, and 2) outerSplit
	doOuter <- apCl$testCV
	percTest <- apCl$percTest
	rndCC <- stnLoc$cl_gen_digitsRoundCorrClass
	#
	if (percTest <= 0 ) {
		doOuter <- FALSE
	}
#	cvList <- testList <- outListRealClassOn <- cvBranchErrorsList <- cvBranchCorrClassList <- summaryList <- methodList <-  vector("list", length(classOn))
	cvList <- testList <- outListRealClassOn <- summaryList <- methodList <- nrsCvTrainList <- nrsCvPredList <- grpsCvPredList <- nrsTestCvList <- nrsTestPredList <- grpsTestPredList <- vector("list", length(classOn))
	for (k in 1: length(classOn)) {
		splitList <- makeOuterSplitList(dataset, percTest, classOn[k], stnLoc) # new split in CV and TEST data for every classOn variable
		lengSplit <- length(splitList)
		if (!doOuter) {
			lengSplit <- 1
		}
		modsList <- realClOnList <- testBranchList <- cvBranchErrorsList <- cvBranchCorrClassList <- nrsCvTrainLi <- nrsCvPredLi <- grpsCvPredLi <-  nrsTestCvLi <- nrsTestPredLi <- grpsTestPredLi <- vector("list", lengSplit)
		if (!stnLoc$allSilent) { cat(paste0(" ",classOn[k])) }
		for (i in 1: lengSplit) { ### going through the split list (can possibly be of length 1) #######
			spDs <- makeOuterSplitDatasets(dataset, splitList, testInd=i)
			if (nlevels(spDs$cv$header[,classOn[k]]) < 2) {
				message(paste0("There is only one class when grouping by ", classOn[k], "."))
				mods <- NULL
				realClOnFill <- NULL
			} else {
				mods <- make_Xclass_models_inner(cvData=spDs$cv, testData=spDs$test, classFunc, classOn[k], md, apCl, idString, stnLoc, type)	##### CORE ######				
				realClOnFill <- classOn[k]
			}	
			modsList[[i]] <- mods$cvBranch # filling in the data and all the CV info from the outer loop
			cvBranchErrorsList[[i]] <- mods$cvBranch$errors # collect the cv errors and SDs (and correct classifications and their SDs, below) to average them
			cvBranchCorrClassList[[i]] <- mods$cvBranch$corrClass
			testBranchList[[i]] <- mods$testBranch
			realClOnList[[i]] <- realClOnFill
			#
			nrsCvTrainLi[[i]] <- mods$numbersCv$nrsCvTrain
			nrsCvPredLi[[i]] <- mods$numbersCv$nrsCvPred
			grpsCvPredLi[[i]] <- mods$numbersCv$grpsCvPred
			nrsTestCvLi[[i]] <- nrow(spDs$cv)
			nrsTestPredLi[[i]] <- nrow(spDs$test)
			grpsTestPredLi[[i]] <- makeNamedGrpNumsAp2(spDs$test$header, classOn[k])
		} # end for i - going through the split list ###### end going through split list !!!! ###############
		cvList[[k]] <- modsList
		methodList[[k]] <- mods$method
		nrsCvTrainList[[k]] <- calculateMeanOfList(nrsCvTrainLi, stnLoc) 	
		nrsCvPredList[[k]] <- calculateMeanOfList(nrsCvPredLi, stnLoc) 
		grpsCvPredList[[k]] <- calculateColwiseAvgOfNumericList(grpsCvPredLi, stnLoc) # calculate the average of either all the N crossvalidation steps, or of the R bootstrap replicates
		nrsTestCvList[[k]] <- calculateMeanOfList(nrsTestCvLi, stnLoc) 
		nrsTestPredList[[k]] <- calculateMeanOfList(nrsTestPredLi, stnLoc)
		grpsTestPredList[[k]] <- calculateColwiseAvgOfNumericList(grpsTestPredLi, stnLoc) # calculate the average of the X repeats of the outer loop
		#
		### from each run of the outer loop, i.e. the N rounds of producing crossvalidated models and testing them, average the cv errors and SDs, and the correct classification and their SDs. --> avg. of the inner loop avg.
		avgCvErrors <- calculateAverageOfTablesList(lapply(cvBranchErrorsList, function(x) x$avg), stnLoc) # first extract the element ($avg), then hand over to function to calculate averages of all tables. same below.
		avgCvSDs <- calculateAverageOfTablesList(lapply(cvBranchErrorsList, function(x) x$SD), stnLoc)
		avgAvgCvCorrectClass <- round(mean(unlist(lapply(cvBranchCorrClassList, function(x) x$avg))), rndCC) # extract the elemtn ($avg) from the list, unlist and calculate mean. same below. XXX
		avgSDsCvCorrectClass <- round(mean(unlist(lapply(cvBranchCorrClassList, function(x) x$SD))), rndCC)
		summaryList[[k]] <- list(errors=list(avg=avgCvErrors, SD=avgCvSDs), corrClass=list(avg=avgAvgCvCorrectClass, SD=avgSDsCvCorrectClass))
		#
		### now, please, calculate the averages and SDs from the crossvalidated test data (that have been projected onto the N models from crossvalidation)
		testConfPercList <- lapply(testBranchList, function(x) x$testConfPerc) # extract the list element "testConfPerc"
		aa <- calculate_Avg_SDs_confPercList(testConfPercList, stnLoc)
		test_avgsTable <- aa$avgsTable
		test_SDsTable <- aa$SDsTable
		#
		aa <- unlist(lapply(testBranchList, function(x) x$testCorrClassPerc)) # extract the list element "testCorrClassPerc", gives out a vector
		testCorrClassAvg <- if (!is.null(aa)) {round(mean(aa), rndCC)} else {NULL}
		testCorrClassSD <- if (!is.null(aa)) {round(sd(aa), rndCC)} else {NULL}
		testList[[k]] <- list(testErrors=list(avg=test_avgsTable, SD=test_SDsTable), testCorrClass=list(avg=testCorrClassAvg, SD=testCorrClassSD))
		###
		outListRealClassOn[[k]] <- realClOnList
	} # end for k (cycling through the classOn values) ######### end going through the classOn values !!!! #################
	##############
	cvDualList <- list(cvSingle=cvList, cvSummary=summaryList, method=methodList)
	numbersList <- list(nrsCvTrain=nrsCvTrainList, nrsCvPred=nrsCvPredList, grpsCvPred=grpsCvPredList, nrsTestCv=nrsTestCvList, nrsTestPred=nrsTestPredList, grpsTestPred=grpsTestPredList)
	return(list(cvBranch=cvDualList, testBranch=testList, realClOn=realClOnList, numbers=numbersList)) # output of function make_X_classif_models
} # EOF

make_X_classif_handoverType <- function(dataset, md, apCl, types, idString, priInfo, priTy="") { # going through types; called in cube_makeModels.r; the incoming dataset from the set
	stnLoc=.ap2$stn
	classOn <- apCl$classOn
	#
	if (length(types) == 1) {add <- ""} else {add <- ""}
	cvList <- testList <- outListRealClOn <- numbersList <- vector("list", length(types))
	if (!stnLoc$allSilent) {cat(paste0("      calc. ", priInfo, ":"))}
	for (i in 1: length(types)) {		
		classFunc <- getClassifierFunction(types[i]) ####### here select the desired classifier method !! ######
		if (!stnLoc$allSilent) {cat(paste0(" ", priTy[i], add, ":"))}
			###
			aa <- make_X_classif_models(dataset, classFunc, md, apCl, classOn, idString, stnLoc, type=types[i]) ##### make models ###### CORE ######	
			cvList[[i]] <- aa$cvBranch
			testList[[i]] <- aa$testBranch
			outListRealClOn[[i]] <- aa$realClOn
			numbersList[[i]] <- aa$numbers
			###
	} # end for i
	if (!stnLoc$allSilent) {cat(" ok\n")}
	oid <- paste0(priInfo, "__", idString)
	return(list(cvBranch=cvList, testBranch=testList, realClassOn=NA, numbers=numbersList, apCl=apCl, id=oid))
	#
	### the order of the cvBranch, from outer to inner: 
	# daTypes; FIX cvSingle, cvSummary, cvMethod --> each in: classOn, outerLoop (=TestCV), CV inner loop, [then list element of individual models]
	#
	### the order of the testBranch, frou outer to inner:
	# daTypes, classOn; (then already the results: test errors, test CorrClass)
} # EOF






# independent prediction ----------------
check_indXClassifPrediction_input <- function(indepDataset, cube, ccv, icv, cubeName, dsName, toxls, info, confirm, aps) { ## !! is assigning ccv, icv, toxls, info, confirm
	dataset <- indepDataset
	cPref <- .ap2$stn$p_ClassVarPref
	ap <- getAnproc(cube)
	#
	if (class(dataset) != "aquap_data") {
		stop("Please provide an object of class 'aquap_data' (as generated by the function 'gfd') to the argument 'indepDataset'.", call.=FALSE)
	}
	if (class(cube) != "aquap_cube") {
		stop("Please provide an object of class 'aquap_cube' (as generated by the function 'gdmm') to the argument 'cube'.", call.=FALSE)
	}
	checkDatasetVersion(dataset) # checks if it is too old
	#
	checkForTestData <- function(singleSet, slotName, message) {
		if (!is.null(slot(singleSet, slotName))) {
			if (length(slot(singleSet, slotName)$cvBranch[[1]]$cvSingle[[1]]) != 1) {
				stop(paste0("Sorry, it seems that for the classification method `", slotName, "` there are test data set aside. Re-run gdmm() without test data resp. check the analysis procedure at the ", message, " section."), call.=FALSE)
			}
		}
	} # EOIF
	checkForTestData(cube[[1]], "xda", "discriminant analysis")
	checkForTestData(cube[[1]], "rnf", "randomforst")
	checkForTestData(cube[[1]], "xda", "support vector machine")
	checkForTestData(cube[[1]], "ann", "artificial neural networks")
	
	# check if we have some models !!
	cle <- function(sName, obj) { # cle: check list element
		return(all(unlist(lapply(obj, function(x) is.null(slot(x, sName)))))) 
	} # EOIF	
	if (cle("xda", cube) & cle("rnf", cube) & cle("svm", cube) & cle("ann", cube) ) { # so we have no classification model!
		stop(paste0("Sorry, it appears that in the provided cube '", cubeName, "' there is not a single classification model. \nPlease check your input or re-run 'gdmm' with e.g. a modified analysis procedure."), call.=FALSE)
	}
	if (is.null(ap$classif)) { # just to be sure - if the above does not stop, this should pass as well
		stop("The presence of (a) classification model(s) in the cube and the embedded analysis procedure (with no classification information) do not match. Please check your input or re-run 'gdmm' to produce an other cube.", call.=FALSE)
	}
	###
	### check (assign) the ccv
	if (is.null(ccv)) { # so we want them all
		ccv <- unlist(lapply(ap$classif, function(x) x$classOn))
		assign("ccv", ccv, pos=parent.frame(n=1))
	}
	if (!all(is.character(ccv)) ) {
		stop("Please provide a character vector to the argument 'ccv'.", call.=FALSE)
	}
#	cns <- colnames(getDataset(cube[[1]])$header) # all datasets must have the same header structure, so just take the first
	classOn <- unlist(lapply(ap$classif, function(x) x$classOn)) # we are only interested in those class variables on which models have been calculated, we are collectin them all across all different methods (xda, svm, rnf, ann)
	if ( !all(ccv %in% classOn) ) { # so we have a manual input for ccv and now we have to check it
	#	possYvarTotal <- cns[grep(yPref, cns)]
		indNo <- which(!ccv %in% classOn)
		if (length(indNo) == 1) {ad1 <- ""; ad2 <- "s"; ad3 <- "has" } else {ad1 <- "s"; ad2 <- ""; ad3 <- "have"}
		stop(paste0("Sorry, the class variable", ad1, " '", paste(ccv[indNo], collapse="', '"), "' seem", ad2, " not to be present in the cube '", cubeName, "' or ", ad3, " not been used to calculate a classification model. \nPlease check your input at the argument 'ccv'.\nPossible values for class variables on which a classification model has been calculated:\n   ", paste(unique(classOn), collapse=", ")), call.=FALSE)
	}
	ccv <- unique(ccv)
	assign("ccv", ccv, pos=parent.frame(n=1))
	###
	### check (assign) icv
	cns <- colnames(dataset$header) # the colnames of the independent dataset
	CvarCns <- cns[grep(cPref, cns)]
	if (is.null(icv)) {
		icv <- rep(NA, length(ccv))
		for (i in 1:length(ccv)) {
			ind <- which(CvarCns == ccv[i])
			if (length(ind) == 1) {
				icv[i] <- CvarCns[ind]
			}
		}
		assign("icv", icv, pos=parent.frame(n=1))	
	}
	if (!any(is.na(icv)) ) {
		if (!all(is.character(icv)) | length(icv) != length(ccv) ) {
			stop(paste0("Please provide a character length ", length(ccv), " to the argument 'icv' or leave at the default 'NULL'."), call.=FALSE)
		}
		if (!all(icv %in% CvarCns)) {
			indNo <- which(!icv %in% CvarCns)
			if (length(indNo) == 1) {ad1 <- ""; ad2 <- "s"; ad3 <- "has" } else {ad1 <- "s"; ad2 <- ""; ad3 <- "have"}
			stop(paste0("Sorry, the class variable", ad1, " '", paste(icv[indNo], collapse="', '"), "' seem", ad2, " not to be present in the independent dataset '", dsName, "'. Please check your input at the argument 'icv' or leave at the default NULL.\nPossible values are: '", paste(CvarCns, collapse="', '"), "'."), call.=FALSE)
		}
		options(warn=-1)
		indNo <- which(sapply(icv, function(x) any(!is.na(as.numeric(as.character(dataset$header[,x]))))))
		options(warn=0)
		if (length(indNo) > 0) {
			if (length(indNo) == 1) {ad1 <- ""; ad2 <- "s"; ad3 <- "has" } else {ad1 <- "s"; ad2 <- ""; ad3 <- "have"}
			stop(paste0("Sorry, the provided variable", ad1, " '", paste(icv[indNo], collapse="', '"), "' at the independent dataset '", dsName, "' seem", ad2, " not to be entirely categorical.\nPlease check (or re-import) the dataset '", dsName, "'."), call.=FALSE)
		}
	} # end !is.null(icv)
	###
	if (all(toxls == "def")) {
		toxls <- .ap2$stn$cl_indepPred_exportToExcel
	}
	if (!all(is.logical(toxls)) | length(toxls) != 1) {
		stop("Please provide either TRUE or FALSE to the argument 'toxls' resp. to the argument 'cl_indepPred_exportToExcel' in the settings file.", call.=FALSE)
	}
	assign("toxls", toxls, pos=parent.frame(n=1))
	#
	if (all(info == "def")) {
		info <- .ap2$stn$cl_indepPred_showInfo
	}
	if (!all(is.logical(info)) | length(info) != 1) {
		stop("Please provide either TRUE or FALSE to the argument 'info' resp. to the argument 'cl_indepPred_showInfo' in the settings file.", call.=FALSE)
	}
	assign("info", info, pos=parent.frame(n=1))	
	#
	if (all(confirm == "def")) {
		confirm <- .ap2$stn$cl_indepPred_confirm
	}
	if (!all(is.logical(confirm)) | length(confirm) != 1) {
		stop("Please provide either TRUE or FALSE to the argument 'confirm' resp. to the argument 'cl_indepPred_confirm' in the settings file.", call.=FALSE)
	}
	assign("confirm", confirm, pos=parent.frame(n=1))	
	#
} # EOF

printMessagePairing_classif <- function(ccv, icv, info=TRUE, confirm=TRUE) {
	if (info) {
		cat(paste0("The following class variables are used for prediction and the corresponding validation:\n\n"))
		textDF <- data.frame(ccv, icv)
		colnames(textDF) <- c("Prediction (model)", "    Validation (independent data)")
		print(textDF)
		if (confirm) {
			cat("\n\nPress enter to continue or escape to abort:")		
			scan(file = "", n = 1, quiet = TRUE)
		}
	}
	return(invisible(NULL))
} # EOF

calculateIndepClassifXPrediction_inner <- function(slotName, siType, siClassOn, indepDataset, ccv, icv, mods, pcaObTrain, apCl, stnLoc, idString, colorLegValues, method, nrCvTrain, nrCvPred) { # mods comes in as a list
	nrRndSd <- stnLoc$cl_gen_digitsRoundSDTablePerc
	#
	nc <- apCl$pcaNComp # can be either "max" or the desired numbers
	if (any(nc == "max")) {
		nc <- 1: (nrow(indepDataset) - 1)
	} else { # so we only have numbers
		if (max(nc) >= nrow(indepDataset)) { # to prevent the user from manually providing more NCs than are available
			nc <- 1: (nrow(indepDataset) - 1)
		}
	} # end else
	# which class variable of the independent dataset to take for validation?
	validationClass <- icv[which(ccv == siClassOn)] # siClassOn is subselected from ccv
	#
	dfIndep <- dfIndepUse <- makeDataFrameForClassification(indepDataset, siClassOn) # ! is not flat
	dfIndepValid <- makeDataFrameForClassification(indepDataset, validationClass) # as we can use a class variable named differently for validation
	#
	predList <- confPercList <- corrClassPercList <- vector("list", length=length(mods))
	for (i in 1: length(mods)) {
		if (apCl$pcaRed) { 
			NIR <- predict(pcaObTrain[[i]], newdata=dfIndep$NIR)[,nc] # get back the scores for the prediction data
			dfIndepUse$NIR <- I(NIR)
		}
		pred <- predict(mods[[i]], newdata=dfIndepUse)
		predList[[i]] <- pred
		conf <- makeConfusionTable(pred, dfIndepValid$grouping)
		confPercList[[i]] <- calculateConfusionTableInPercent(conf, stnLoc)
		corrClassPercList[[i]] <- calculateCorrectClassificationInPercent(conf, stnLoc)
	} # end for i
	# get the single cell averages of the confusion in percent, use them to calculate the mean and SD
	aa <- calculate_Avg_SDs_confPercList(confPercList, stnLoc)
#	avgsTable <- aa$avgsTable
	SDsTable <- aa$SDsTable
	#	
	# get the average of correct classifications in percent
	aa <- unlist(corrClassPercList)
	corrClassAvg <- mean(aa) # is the same as indepCorrClassPerc
	corrClassSD <- round(sd(aa), nrRndSd)
	#
	majorVote <- performMajorityVote(predList)   	##### CORE #### 
	indepConf <- makeConfusionTable(majorVote, dfIndepValid$grouping)
	indepConfPerc <- calculateConfusionTableInPercent(indepConf, stnLoc) # would be the same as avgsTable
	indepCorrClassPerc <- calculateCorrectClassificationInPercent(indepConf, stnLoc) # is the same as corrClassAvg
	grpIndPred <- unlist(lapply(lapply(split(indepDataset$header, indepDataset$header[,siClassOn]), function(x) x[,siClassOn]), length))
	#
	indPredSummaryList <- list(errors=list(avg=indepConfPerc, SD=SDsTable), corrClass=list(avg=indepCorrClassPerc, SD=corrClassSD)) 
	meta <- list(slotName=slotName, type=siType, classOn=siClassOn, idString=idString, colorLegValues=colorLegValues, apCl=apCl, method=method) 
	numbers <- list(nrCvTrain=nrCvTrain, nrCvPred=nrCvPred, grpIndPred=grpIndPred)
	#
	return(list(indPredSummaryList=indPredSummaryList, meta=meta, numbers=numbers))
} # EOF

apply_dpt_ssc_dpt_toIndepDataset <- function(indepDataset, ap, classes, values) { # siClass and siValues are coming in as data.frame from the corresponding row in the splitVars from the cpt
	# apply the data pretreatment modules within ap to indepDataset
	allDpt <- ap$dpt$dptModules
	dptPre <- allDpt$dptPre
	dptPost <- allDpt$dptPost
	if (!is.null(dptPre)) {
		indepDataset <- do_dptSeq(indepDataset, dptPre)
		if (nrow(indepDataset) == 0) {
			stop(paste0("Sorry, the combination `", paste(classes, values, collapse=", "), "` yielded not results."), call.=FALSE)
		}
	}
	indepDataset <- ssc_s(indepDataset, classes, values)
	if (!is.null(dptPost)) {
		indepDataset <- do_dptSeq(indepDataset, dptPost)		
	}
	return(indepDataset)
} #  EOF

calculateIndepClassifXPrediction <- function(indepDataset, cube, ccv, icv, ap, cubeID) { # this is called from plot_classifX_indepPred
	stnLoc <- .ap2$stn
	#
	resultList <- vector("list", length=length(cube))
	clNames <- c("xda", "rnf", "svm", "ann")
	splitVars <- cube@cpt@splitVars
	for (i in 1: length(cube)) { # going through the cube
	#	cutIndepData <- adaptIndepDatasetForWls(indepDataset, getDataset(cube[[i]])) # returns NULL if independent dataset is out of range of cube-dataset
		cutIndepData <- indepDataset
		cutIndepData <- apply_dpt_ssc_dpt_toIndepDataset(indepDataset, ap, splitVars$classes[i,,drop=FALSE], splitVars$values[i,,drop=FALSE]) # is applying the dpt.pre, then any possible splitting, then the dpt.post
		idString <- adaptIdStringForDpt(ap, getIdString(cube[[i]]))
		setSlotResList <- vector("list", length=4); names(setSlotResList) <- clNames
		set <- cube[[i]]
		for ( k in 1 : 4) { # we have four slots for classifiers in the set; going through the four slots
			sl <- slot(set, clNames[k]) #### extract the current slot ###### SLOT #####
			if (!is.null(sl)) {
				apCl <- sl$apCl
				type <- apCl$type
				thisClassOn <- apCl$classOn
				ind <- which(ccv %in% thisClassOn)
				classOn <- ccv[ind] ## !!! test this !!! --> to leave out all the classOns that we do have models on, but that we do not want, as they are not included in ccv
				typeList <- vector("list", length=length(type)); names(typeList) <- type
				classOnList <- vector("list", length=length(classOn)); names(classOnList) <- classOn
				for (ty in 1: length(type)) {
					for (co in 1: length(classOn)) {
						colorLegValues <- extractColorLegendValues(cutIndepData, classOn[co])
						classOnList[[co]] <- calculateIndepClassifXPrediction_inner(clNames[k], type[ty], classOn[co], cutIndepData, ccv, icv, 
											mods=sl$cvBranch[[ty]]$cvSingle[[co]][[1]]$mods, # the last "1" is because we are sure to have only a single set of models (no testing!)
											pcaObTrain=sl$cvBranch[[ty]]$cvSingle[[co]][[1]]$pcaObTrain, 
											apCl, stnLoc, idString, colorLegValues, 
											method = sl$cvBranch[[ty]]$method[[co]],
											nrCvTrain = sl$numbers[[ty]]$nrsCvTrain[[co]],
											nrCvPred = sl$numbers[[ty]]$nrsCvPred[[co]] )  
					} # end for co: through classOn
					typeList[[ty]] <- classOnList
				} # end for ty: through type
				setSlotResList[[k]] <- typeList
			} # end if !is.null(slot..)
		} # end for k (the four slots)
		resultList[[i]] <- setSlotResList # moving over the cube elements		
	} # end for i (going through the cube)
	return(new("aquap_ipl", anproc=getAnproc(cube), metadata=getMetadata(cube), cubeID=cubeID, resultList))
} # EOF




# next:
# max in pcaRed is not working


# future:
# have chisqu.test
