
calculatePCA <- function(dataset, md, ap) {
	if (is.null(ap$pca)) {
		return(NULL)
	}
	if (!.ap2$stn$allSilent) {cat("      calc. PCA...")}
	pcaModel <- ChemometricsWithR::PCA(scale(dataset$NIR, scale=FALSE))
	if (!.ap2$stn$allSilent) {cat(" ok\n")}
	return(list(model=pcaModel))
} # EOF

calculatePLSR <- function(dataset, md, ap) {
	if (is.null(ap$plsr)) {
		return(NULL)
	}
	if (!.ap2$stn$allSilent) {cat("      calc. PLSR, ")}
		aa <- makePLSRModels(dataset, md, ap)
#		print(str(aa)); wait()
	if (!.ap2$stn$allSilent) {cat(" ok\n")}
	return(list(model=aa$plsr, modelPlus=aa$plsrPlus, regrOn=aa$regrOn))
} # EOF

calculateSIMCA <- function(dataset, md, ap) { # is working on a single set. i.e. element of a cube, with this specific dataset
	if (is.null(ap$simca)) {
		return(NULL)
	}
	.ap2$.gs <- " ok";  .ap2$.charCollect <- NULL;  mods_cv <- preds_cv <- NULL
	simcaVersion <- .ap2$stn$simca_version
#	simcaClasses <- ap$simca$simcOn # comes in already checked, so it is a character vector of at least length one
	simcaClasses <- correctSimcaGroupingForDataset(dataset, groupingVector=ap$simca$simcOn) # will return NULL if there is no appropriate grouping / nr. of spectra; see below
	if (is.null(simcaClasses)) {
		return(NULL) 	# because it is possible that with some split-data sets we find no grouping variable that gives us at least two groups with at least the minimum number of spectra in each group
	}
	simca_k <- ap$simca$simcK
	SC <- paste(simcaClasses, collapse=", ")
	SC <- gsub(.ap2$stn$p_ClassVarPref, "", SC) # delete all the "C_" to save space
	le <- length(simcaClasses)
	if (!.ap2$stn$allSilent) {cat(paste("      calc. ", le, " SIMCA (", SC, "): ", sep=""))}
	#
	mods <-  makeSimcaModels(dataset, groupingVector=simcaClasses, k=simca_k, simcaVersion) # returns a list with one model for each grouping
	preds <-  makeSimcaPredictions(SimcaModelList=mods, newFlatData=NULL, newCorrectGrouping=NULL)
	icDists <- calculateInterclassDistances(mods)
	#
	if (.ap2$stn$simca_tablesToTxt) {
		percNew <- .ap2$stn$simca_percNewData_CV
		sampling <- .ap2$stn$simca_sampling
		indPool <- 1: nrow(dataset)
		nrNew <- round(nrow(dataset)*(percNew/100), 0)
		nrTrain <- nrow(dataset) - nrNew
		indNew <- sample(indPool, nrNew)
		indTrain <- sample(indPool[-indNew], nrTrain)
		if (percNew == 50 & sampling == pv_simca_calc_sampling[2]) {     # a global variable: c("random", "interleaved")
			indNew <- seq(2, nrow(dataset), by=2)
			indTrain <- seq(1, nrow(dataset), by=2)
		}
	#	if (!.ap2$stn$allSilent) {cat(paste("    --- ", percNew, "% new data - CV Models: \n", sep="") )}
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
	whatErr <- gsub(.ap2$stn$p_ClassVarPref, "", .ap2$.charCollect)
	whatErr <- substr(whatErr, 2, nchar(whatErr)) # to cut away the first comma
	if (!.ap2$stn$allSilent) {cat(paste(.ap2$.gs, whatErr, "\n", sep="")) }
	.ap2$.charCollect <- NULL
	.ap2$.gs <- " ok"
	return(list(mods=mods, preds=preds, mods_cv=mods_cv, preds_cv=preds_cv, icDists=icDists, groupingVector=simcaClasses))
} # EOF

calculateAquagram <- function(dataset, md, ap, idString) {
	if (is.null(ap$aquagr)) {
		return(NULL)
	}
	aq_loadGlobalAquagramCalibData()
	if (is.character(ap$aquagr$spectra)) { message <- "      calc. Aquagrams & spectra... "	} else { message <- "      calc. Aquagrams... "	}
	if (ap$aquagr$bootCI) {bootTxtCorr <- "\n"; bootTxtClosingAdd <- "      Aquagrams ok.\n" } else {bootTxtCorr <- ""; bootTxtClosingAdd <- "ok\n"}
	message <- paste(message, bootTxtCorr)
	if (!.ap2$stn$allSilent) {cat(message)}
	ap <- aq_getTCalibRange(ap) 	# checks with the calibration file if the temperature range is ok
	aq_makeGlobals(.ap2$tcd, TCalib=ap$aquagr$TCalib, Texp=ap$aquagr$Texp, ot=getOvertoneCut(.ap2$stn$aqg_OT), smoothN=.ap2$stn$aqg_smoothCalib) ## generate the global variables with TCalib and Texp
	##
	vars <- ap$aquagr$vars
	aquCalcRes  <- list()
	length(aquCalcRes) <- lec <- length(vars)
	if (ap$aquagr$bootCI) {
		registerParallelBackend()  ## will be used in the calculation of confidence intervals
	}
	for (i in 1: length(vars)) {
		aquCalcRes[[i]] <- calcAquagramSingle(dataset, md, ap, vars[i], idString)
	} # end for i
	
	if (!.ap2$stn$allSilent) {cat(bootTxtClosingAdd)}
	return(aquCalcRes)
} # EOF

# works on a single element of the list in the cube
makeAllModels <- function(set, md, ap) {
#	dataset <- set@dataset
	newSet <- new("aquap_set")
	newSet@pca <- calculatePCA(getDataset(set), md, ap)
	newSet@plsr <- calculatePLSR(getDataset(set), md, ap)
	newSet@simca <- calculateSIMCA(getDataset(set), md, ap)
	newSet@aquagr <- calculateAquagram(getDataset(set), md, ap, getIdString(set))
	newSet@dataset <- getDataset(set)
	newSet@idString <- set@idString
	return(newSet)
} # EOF
