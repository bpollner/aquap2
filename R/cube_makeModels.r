
calculatePCA <- function(dataset, md, ap) {
	stn <- getstn()
	if (is.null(ap$pca)) {
		return(NULL)
	}
	if (!stn$allSilent) {cat("      calc. PCA...")}
	pcaModel <- rw_PCA(scale(dataset$NIR, scale=FALSE))
	if (!stn$allSilent) {cat(" ok\n")}
	return(list(model=pcaModel))
} # EOF

calculatePLSR <- function(dataset, md, ap) {
	stn <- getstn()
	if (is.null(ap$plsr)) {
		return(NULL)
	}
	if (!stn$allSilent) {cat("      calc. PLSR, ")}
		aa <- makePLSRModels(dataset, md, ap)
#		print(str(aa)); wait()
	if (!stn$allSilent) {cat(" ok\n")}
	return(list(model=aa$plsr, modelPlus=aa$plsrPlus, regrOn=aa$regrOn, valid=aa$valid, usedDS=aa$usedDS, exOut=aa$exOut))
} # EOF

calculateSIMCA <- function(dataset, md, ap) { # is working on a single set. i.e. element of a cube, with this specific dataset
	stn <- getstn()
	if (is.null(ap$simca)) {
		return(NULL)
	}
	assign(".gs", " ok", pos=gl_ap2GD)
	assign(".charCollect", NULL, pos=gl_ap2GD)
	mods_cv <- preds_cv <- NULL
	simcaVersion <- stn$simca_version
#	simcaClasses <- ap$simca$simcOn # comes in already checked, so it is a character vector of at least length one
	simcaClasses <- correctSimcaGroupingForDataset(dataset, groupingVector=ap$simca$simcOn) # will return NULL if there is no appropriate grouping / nr. of spectra; see below
	if (is.null(simcaClasses)) {
		return(NULL) 	# because it is possible that with some split-data sets we find no grouping variable that gives us at least two groups with at least the minimum number of spectra in each group
	}
	simca_k <- ap$simca$simcK
	SC <- paste(simcaClasses, collapse=", ")
	SC <- gsub(stn$p_ClassVarPref, "", SC) # delete all the "C_" to save space
	le <- length(simcaClasses)
	if (!stn$allSilent) {cat(paste("      calc. ", le, " SIMCA (", SC, "): ", sep=""))}
	#
	mods <-  makeSimcaModels(dataset, groupingVector=simcaClasses, k=simca_k, simcaVersion) # returns a list with one model for each grouping
	preds <-  makeSimcaPredictions(SimcaModelList=mods, newFlatData=NULL, newCorrectGrouping=NULL)
	icDists <- calculateInterclassDistances(mods)
	#
	if (stn$simca_tablesToTxt) {
		percNew <- stn$simca_percNewData_CV
		sampling <- stn$simca_sampling
		indPool <- 1: nrow(dataset)
		nrNew <- round(nrow(dataset)*(percNew/100), 0)
		nrTrain <- nrow(dataset) - nrNew
		indNew <- sample(indPool, nrNew)
		indTrain <- sample(indPool[-indNew], nrTrain)
		if (percNew == 50 & sampling == pv_simca_calc_sampling[2]) {     # a global variable: c("random", "interleaved")
			indNew <- seq(2, nrow(dataset), by=2)
			indTrain <- seq(1, nrow(dataset), by=2)
		}
	#	if (!stn$allSilent) {cat(paste("    --- ", percNew, "% new data - CV Models: \n", sep="") )}
		trainingData <- dataset[indTrain]
		modsTry <- try(makeSimcaModels(trainingData, groupingVector=simcaClasses, k=simca_k, simcaVersion, inCV=TRUE))
		if (is.character(modsTry)) {
			mods_cv <- NULL
			preds_cv <- NULL
		} else {
			mods_cv <- modsTry
			preds_cv <- makeSimcaPredictions(SimcaModelList=mods_cv, newFlatData=dataset, newCorrectGrouping=simcaClasses, indNew, inCV=TRUE) #see one level below !		
		}
	} # end if tables to text
	#
	whatErr <- gsub(stn$p_ClassVarPref, "", get(".charCollect", pos=gl_ap2GD))
	whatErr <- substr(whatErr, 2, nchar(whatErr)) # to cut away the first comma
	if (!stn$allSilent) {cat(paste(get(".gs", pos=gl_ap2GD), whatErr, "\n", sep="")) }
	assign(".charCollect", NULL, pos=gl_ap2GD)
	assign(".gs", " ok", pos=gl_ap2GD)
	return(list(mods=mods, preds=preds, mods_cv=mods_cv, preds_cv=preds_cv, icDists=icDists, groupingVector=simcaClasses))
} # EOF

calculateAquagram <- function(dataset, md, ap, idString, tempFile) {
	stn <- getstn()
	if (is.null(ap$aquagr)) {
		return(NULL)
	}
#	apLoc <- list()
#	apLoc$stn <- stn # add the "stn" before the values, so simulate the old dotAp2 structure
#	aq_loadGlobalAquagramCalibData()
	if (ap$aquagr$bootCI) {
		registerParallelBackend()  ## will be used in the calculation of confidence intervals
		bootTxtCorr <- "\n"
		bootTxtClosingAdd <- "      Aquagrams ok.\n" 	
	} else {
		registerDoSEQ() # XXX new !
		bootTxtCorr <- ""
		bootTxtClosingAdd <- " " 	
	}
 	if (is.character(ap$aquagr$spectra)) { message <- "      calc. Aquagrams & spectra... "	} else { message <- "      calc. Aquagrams... "	}
	message <- paste(message, bootTxtCorr)
	if (!stn$allSilent) {cat(message)}
	ap <- aq_getTCalibRange(ap, tempFile) 	# checks with the calibration file if the temperature range is ok
	if (!haveClassicAqg(ap)) {
		aq_makeGlobals(TCalib=ap$aquagr$TCalib, Texp=ap$aquagr$Texp, ot=getOvertoneCut(stn$aqg_OT), smoothN=stn$aqg_smoothCalib, tempFile) ## generate the global variables with TCalib and Texp
	}
	##
	vars <- ap$aquagr$vars
	minus <- ap$aquagr$minus
	aquCalcRes  <- list()
	length(aquCalcRes) <- lec <- length(vars)
	for (i in 1: length(vars)) {
#		aquCalcRes[[i]] <- calcAquagramSingle(dataset, md, ap, vars[i], minus[i], idString, apLoc)
		aquCalcRes[[i]] <- calcAquagramSingle(dataset, md, ap, vars[i], minus[i], idString, stn)
	} # end for i
	
	if (!stn$allSilent) {cat(bootTxtClosingAdd)}
	return(aquCalcRes)
} # EOF

calculate_XDA <- function(dataset, md, ap, idString) {
	if (is.null(ap$classif$da)) {
		return(NULL)
	}
	apCl <- ap$classif$da
	daTypes <- apCl$type
	pri <- "DA"
	priTy <- daTypes
	return(make_X_classif_handoverType(dataset, md, apCl, types=daTypes, idString, priInfo=pri, priTy))
} # EOF

calculate_RNF <- function(dataset, md, ap, idString) {
	if (is.null(ap$classif$rnf)) {
		return(NULL)
	}	
	apCl <- ap$classif$rnf
	types <- pv_nonDAClassifiers[1]  # pv_nonDAClassifiers <- c("rndforest", "svm", "nnet")
	apCl$type <- types
	return(make_X_classif_handoverType(dataset, md, apCl, types, idString, priInfo="RNF", priTy=types))
} # EOF

calculate_SVM <- function(dataset, md, ap, idString) {
	if (is.null(ap$classif$svm)) {
		return(NULL)
	}
	apCl <- ap$classif$svm
	types <- pv_nonDAClassifiers[2]  # pv_nonDAClassifiers <- c("rndforest", "svm", "nnet")
	apCl$type <- types
	return(make_X_classif_handoverType(dataset, md, apCl, types, idString, priInfo="SVM", priTy=types))
} # EOF

calculate_ANN <- function(dataset, md, ap, idString) {
	if (is.null(ap$classif$nnet)) {
		return(NULL)
	}
	apCl <- ap$classif$nnet
	types <- pv_nonDAClassifiers[3]  # pv_nonDAClassifiers <- c("rndforest", "svm", "nnet")
	apCl$type <- types	
	return(make_X_classif_handoverType(dataset, md, apCl, types, idString, priInfo="NNET", priTy=types))
} # EOF


# works on a single element of the list in the cube
makeAllModels <- function(set, md, ap, tempFile) {
#	dataset <- set@dataset
	newSet <- new("aquap_set")
	newSet@pca <- calculatePCA(getDataset(set), md, ap)
	newSet@plsr <- calculatePLSR(getDataset(set), md, ap)
	newSet@simca <- calculateSIMCA(getDataset(set), md, ap)
	newSet@aquagr <- calculateAquagram(getDataset(set), md, ap, getIdString(set), tempFile)
	newSet@dataset <- getDataset(set)
	newSet@idString <- set@idString
	newSet@extraModels <- set@extraModels
	# classifiers
	newSet@xda <- calculate_XDA(getDataset(set), md, ap, getIdString(set))
	newSet@rnf <- calculate_RNF(getDataset(set), md, ap, getIdString(set))
	newSet@svm <- calculate_SVM(getDataset(set), md, ap, getIdString(set))
	newSet@ann <- calculate_ANN(getDataset(set), md, ap, getIdString(set))
	#
	return(newSet)
} # EOF
