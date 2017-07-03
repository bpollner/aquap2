make_fda_model_CV_single <- function(dataset, classOn) {
	fdf <- makeFlatDataFrame(dataset, classOn)
	mod <- mda::fda(grouping ~ ., data=fdf)
} # EOF


make_fda_models_CV_outer <- function(cvData, valid=10, classOn) {
	if (valid == "LOO") {
		valid <- nrow(cvData)
	}
	segList <- createSegmentListForClassification(cvData$header, nrSegs=valid, classOn, stnLoc=.ap2$stn) # looks in each group individually
	modList <- vector("list", length(segList))
	for (i in 1: length(segList)) {
		modList[[i]] <- make_fda_model_CV_single(cvData[ segList[[i]] ], classOn)
	} # end for i
	return(modList)
} # EOF

make_fda_models_boot <- function(cvData, R, classOn) {
	
} # EOF


make_fda_models_inner <- function(cvData, testData, classOn, md, ap, idString) {
	cvBootCutoff <- 1 # 400
	cvBootFactor <- 1 # the factor used for multiplying the number of observations in the group, resulting in the bootR value
	cvValid <- 10
	#
	clPref <- .ap2$stn$p_ClassVarPref
	snColName <- .ap2$stn$p_sampleNrCol 
	snrCol <- paste0(clPref, snColName)
	#
	ind <- which(colnames(cvData$header) == snrCol)
	aa <- lapply(split(cvData$header, cvData$header[,classOn]), function(x) x[,ind]) # split into groups, then get the sampleNr column
#	print(str(aa)); print(lapply(aa, length))
	minNrow <-  min(unlist(lapply(aa, length))) #  returns the smallest nr of observations from all separate groups
#	minNrow <- min(unlist(lapply(lapply(aa, unique), length))) #  returns the smallest nr of observations from all separate groups, reduced by consecutive scans!
	if (minNrow >= cvBootCutoff) {
		mods <- make_fda_models_CV_outer(cvData, valid=cvValid, classOn)
	} else {
		mods <- make_fda_models_boot(cvData, R=minNrow*cvBootFactor, classOn)
	}
	return(mods)
} # EOF

make_fda_models <- function(dataset, md, ap=NULL, idString=NULL) { # the incoming dataset from the set -- everything here still
	# will come from the ap
	doOuter <- FALSE
	percTest <- 30
	classOn <- "C_Group"
	#
	splitList <- makeOuterSplitList(dataset, percTest, classOn, stnLoc=.ap2$stn)
	if (!doOuter) {
		modsList <- vector("list", 1)
		aa <- makeOuterSplitDatasets(dataset, splitList, testInd=1) # returns a list:  $test and $cv
		mods <- make_fda_models_inner(cvData=aa$cv, testData=aa$test, classOn, md, ap, idString)
		modsList[[1]] <- mods 
	} else {
		modsList <- vector("list", length(splitList))
		for (i in 1: length(splitList)) {	
			aa <- makeOuterSplitDatasets(dataset, splitList, testInd=i)	
			mods <- make_fda_models_inner(cvData=aa$cv, testData=aa$test, classOn, md, ap, idString)
			modsList[[i]] <- mods 
		} # end for i
	} # end else
	return(modsList)
} # EOF
