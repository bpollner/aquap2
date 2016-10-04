createSegmentList <- function(header, nrSegs=10) { # gives back a list that can be given to "segments" in the plsr
	yPref <- .ap2$stn$p_yVarPref
	snCol <- .ap2$stn$p_sampleNrCol
	csnCol <- .ap2$stn$p_conSNrCol
	nrSegsSwitch <- 2
	## first see how many consecutive scans there are within each sample number (some (outlier) could have been removed)	
	snName <- paste0(yPref, snCol)
	snInd <- which(colnames(header) == snName)
	conSnInd <- which(colnames(header) == paste0(yPref, csnCol))
	headerDim <- header[,c(snInd, conSnInd)]
	res <- plyr::ddply(headerDim, snName, nrow) 		## data frame with two columns, has the number of rows in in the second column
	segList <- NULL
	currInd <- 1
	for (i in 1: nrow(res)) {
		indices <- currInd:( (currInd-1) + res[i,2] )
		currInd <- max(indices)+1
		segList <- c(segList, list(indices))  ## gives segList with all the same sample numbers (different cons. scans from the same sample number) in the same list element
	} # end for i
	if (length(segList) <= nrSegsSwitch) {
		return(nrow(header))
	}
	## randomize
	if (nrSegs > length(segList)) {
		nrSegs <-  length(segList)
	}	
	le <- length(segList)
	pool <- 1: le
	nrFull <- floor(le / nrSegs)
	indList <- NULL
	for (i in 1: nrSegs) {
		res <- sample(pool, nrFull)
		pool <- pool[-(which(pool %in% res))]
		indList <- c(indList, list(res))
	} # end for i
	if (length(pool) > 0) {
		for (i in 1: length(pool)) {
			indList[[i]] <- c(indList[[i]], pool[i]) # add the rest of the values to the seg list
		}
	}
	## map back to the segment list
	out <- lapply(indList, function(x) unlist(segList[x]))
	return(out) 		
} # EOF

createPlsrSegments <- function(header, valid=10) { # gives back a number or a list
	if (valid == nrow(header)) {
		return(valid)
	} else { 
		if (valid > nrow(header)) {
			return(nrow(header))
		}
		out <- valid
		if (.ap2$stn$plsr_calc_CV_consecsTogether) {
			out <- createSegmentList(header, nrSegs=valid)
		}
		return(out)
	} # end else
} # EOF

makePLSRModel_inner <- function(dataset, Y_Class, niter=5, ncomp=NULL, valid, stnLoc) {
	nrSwitch <- stnLoc$plsr_nrCompsSwitchToNrObserv		## from the local settings
	addComps <- stnLoc$plsr_addComps
	acb <- stnLoc$plsr_addCompsBoundaries
	facObs <- stnLoc$plsr_percentObservAsMaxNcomp / 100
	header <- getHeader(dataset)
	dataset <- data.frame(yvar=header[,Y_Class], allData=dataset$NIR ) ### here make a new "flat" dataset
	typeValid <- "CV"
	if (valid == "LOO") {
		valid <- nrow(dataset)
	}
	if (niter == 1) {niter <- 2} # otherwise we never get out of the while loop !!!
	if (is.null(ncomp)) {
		allOneTime <- TRUE
		while (allOneTime) {
			minErrorVec <- rep(NA, niter)
			maxNcomp <- round( (nrow(header)*facObs), 0)
			if (maxNcomp == nrow(header)) {
				maxNcomp <- nrow(header) -1
			}
			if (maxNcomp <= nrSwitch) {			## so if we have very few rows -- now we do not want to give any nr of comps at all
				for (i in 1: niter) {
					segms <- createPlsrSegments(header, valid)
					testModel <- pls::plsr(yvar ~ allData, data=dataset, validation=typeValid, segments=segms)
					a <- pls::RMSEP(testModel, intercept=FALSE, estimate="adjCV")$val
					ind <- which.min(a)
		#			if ( (!allow1C) & (ind == 1)) { ind <- order(a)[2] }
					minErrorVec[i] <-  ind
				} # end for i niter		
			} else {						## so if we have a lot of rows !!!
				for (i in 1: niter) {
					segms <- createPlsrSegments(header, valid)
					testModel <- pls::plsr(yvar ~ allData, data=dataset, validation=typeValid, ncomp=maxNcomp, segments=segms)
					a <- pls::RMSEP(testModel, intercept=FALSE, estimate="adjCV")$val
					ind <- which.min(a)
		#			if ( (!allow1C) & (ind == 1)) { ind <- order(a)[2] }
					minErrorVec[i] <-  ind
				} # end for i niter
			} # end else
			maxInd <- as.numeric(which.max(table(minErrorVec)))
			bestNC <- as.numeric(names(table(minErrorVec)))[maxInd] # because the values are coded as names (the countings are coded as numeric, but we want the values!!)
			allOneTime <- all(unique(table(minErrorVec))==1)  # we only get out of the while loop, if there is at least one value more than once present
		} # end while allOneTime
	} else {		## so if a value for ncomp was provided
		bestNC <- ncomp
	} # end if is.null(ncomp)
	segms <- createPlsrSegments(header, valid)
	plsModelCorrect <- pls::plsr(yvar ~ allData, ncomp=bestNC, data=dataset, validation=typeValid, segments=segms)	#### here it happens !!
	bn <- bestNC
	if(bn <= acb[1] ){
		add <- addComps[1]
	}
	if (bn > acb[1] & bn <= acb[2]) {
		add <- addComps[2]
	}
	if (bn > acb[2]) {
		add <- addComps[3]
	} 
	plsModelPlus <-  pls::plsr(yvar ~ allData, ncomp=bestNC+add, data=dataset, validation=typeValid, segments=segms)	#### here it happens !!
	out <- list(modelsCorr=plsModelCorrect, modelsPlus=plsModelPlus, regrOn=Y_Class)
	return(out)
} # EOF

makePLSRModels <- function(dataset, md, ap) {
	i <- NULL # to avoid complaints in the checking
	doPar <- wantPar <- .ap2$stn$plsr_calcInParallel		## from the settings
	niter <- .ap2$stn$plsr_nrTestIterations
	#
	regrOnList <- ap$plsr$regressOn				## from the analysis procedure
	ncomp <- ap$plsr$ncomp
	valid <- ap$plsr$valid
	colorBy <- ap$plsr$colorBy
	#
	leng <- length(regrOnList)
	parMsg <- ""
	if (leng == 1) { # to avoid confusing messages like "doing 1 model in parallel"
		doPar <- FALSE
		parMsg  <- maybeAssignPlsrClusterToPlsrFunction()
	} else {
		maybeStopPlsrCluster() # just to be sure; could still be running if we manually stopped within gdmm so could not stop the cluster there
		pls::pls.options(parallel = NULL) # we do not want the pls-onboard parallel, but the foreach parallel if leng > 1.
	}
	stnLoc <- .ap2$stn
	if (doPar) {
		registerParallelBackend()
	} else {
		registerDoSEQ()
	}
	dopare <- checkHaveParallel()
	if (leng > 1) {moa <- "s"} else {moa <- ""}
	if (dopare) {
		if (!.ap2$stn$allSilent) {cat(paste(leng, " model", moa, " (", paste(regrOnList, collapse=", "), ") in parallel... ", sep="")) }
	}
	if (!dopare) { if (!.ap2$stn$allSilent) { cat(paste(leng, " model", moa, parMsg, ": ", sep="")) }}
	modelList <- foreach(i= 1:leng) %dopar% { ### !!!! in parallel !!!! (possibly)
		if (!dopare) {	
			if (i == leng) {coa <- ". "} else {coa <- ", "}
			if (!.ap2$stn$allSilent) {cat(paste(regrOnList[i]), coa, sep="" )}
		}
		out <- makePLSRModel_inner(dataset, regrOnList[i], niter, ncomp, valid, stnLoc)	 ####### CORE ###### CORE !!!!
	} # end foreach i
	modCorr <- modPlus <- regrOn <- list()
	for (i in 1: length(modelList)) { # resort the list from modelList
		modCorr <- c(modCorr, list(modelList[[i]]$modelsCorr))
		modPlus <- c(modPlus, list(modelList[[i]]$modelsPlus))
		regrOn <- c(regrOn, list(modelList[[i]]$regrOn))
	} # end for i	
	out <- list(plsr=modCorr, plsrPlus=modPlus, regrOn=regrOn)
	return(out)
} # EOF

maybeAssignPlsrClusterToPlsrFunction <- function() {
	if (.ap2$stn$plsr_calcInParallel) {
		if (exists(".plsrClust", where=.ap2)) {
			pls::pls.options(parallel = .ap2$.plsrClust)
			out <- " (par.)"
		} else {
			pls::pls.options(parallel = NULL)
			out <- ""
		}
	} else {
		pls::pls.options(parallel = NULL)
		out <- ""
	}
	return(out)
} # EOF

maybeGeneratePlsrCluster <- function(ap, nc=4) { # this is happening in gdmm before going into the cube
	if (.ap2$stn$plsr_calcInParallel & !is.null(ap$plsr)) {
		if (length(ap$plsr$regressOn) == 1) {
			if (!exists(".plsrClust", where=.ap2)) {
				assign(".plsrClust", parallel::makePSOCKcluster(nc), pos=.ap2)
			}	
		}
	}
} # EOF

maybeStopPlsrCluster <- function() { # this is happening in gdmm after going through the cube
	if (exists(".plsrClust", where=.ap2)) {
		try(parallel::stopCluster(.ap2$.plsrClust), silent=TRUE)
		rm(".plsrClust", pos=.ap2)
	}
} # EOF

dev_collectNcomps <- function(cube) {
	out <- vector("list", length(cube))
	a <- NULL
	for (i in 1: length(cube)) {
		mod <- getcm(cube, i, "plsr")$model
		for (k in 1: length(mod)) {
			a <- c(a, mod[[k]]$ncomp)
		}
		out[[i]] <- a
		a <- NULL
	}
	return(out) 
} # EOF

# independent validation ------------------------

check_indPlsPrediction_input <- function(indepDataset, cube, cnv=NULL, inv=NULL, cubeName, dsName, toxls, psd) { 
	dataset <- indepDataset
	yPref <- .ap2$stn$p_yVarPref
	ap <- getAnproc(cube)
	#
	if (class(dataset) != "aquap_data") {
		stop("Please provide an object of class 'aquap_data' (as generated by the function 'gfd') to the argument 'indepDataset'.", call.=FALSE)
	}
	if (class(cube) != "aquap_cube") {
		stop("Please provide an object of class 'aquap_cube' (as generated by the function 'gdmm') to the argument 'cube'.", call.=FALSE)
	}
	checkDatasetVersion(dataset) # checks if it is too old
	# check if we have some plsr models !!
	cle <- function(sName, obj) { # cle: check list element
		return(all(unlist(lapply(obj, function(x) is.null(slot(x, sName)))))) 
	} # EOIF
	if (cle("plsr", cube)) { # so we have no plsr model!
		stop(paste0("Sorry, it appears that in the provided cube '", cubeName, "' there is not a single plsr model. \nPlease check your input or re-run 'gdmm' with e.g. a modified analysis procedure."), call.=FALSE)
	}
	if (is.null(ap$plsr)) { # just to be sure - if the above does not stop, this should pass as well
		stop("The presence of (a) plsr model(s) in the cube and the embedded analysis procedure (with no plsr information) do not match. Please check your input or re-run 'gdmm' to produce an other cube.", call.=FALSE)
	}
	###
	### check (assign) the cnv
	if (is.null(cnv)) { # so we want them all
		cnv <- ap$plsr$regressOn
		assign("cnv", cnv, pos=parent.frame(n=1))
	}
	if (!all(is.character(cnv)) ) {
		stop("Please provide a character vector to the argument 'cnv'.", call.=FALSE)
	}
#	cns <- colnames(getDataset(cube[[1]])$header) # all datasets must have the same header structure, so just take the first
	regOn <- ap$plsr$regressOn # we are only interested in those numeric variables on which models have been calculated
	if ( !all(cnv %in% regOn) ) {
	#	possYvarTotal <- cns[grep(yPref, cns)]
		indNo <- which(!cnv %in% regOn)
		if (length(indNo) == 1) {ad1 <- ""; ad2 <- "s"; ad3 <- "has" } else {ad1 <- "s"; ad2 <- ""; ad3 <- "have"}
		stop(paste0("Sorry, the numerical variable", ad1, " '", paste(cnv[indNo], collapse="', '"), "' seem", ad2, " not to be present in the cube '", cubeName, "' or ", ad3, " not been used to calculate a plsr model. \nPlease check your input at the argument 'cnv'.\nPossible values for numerical variables on which a plsr-model has been calculated:\n   ", paste(regOn, collapse=", ")), call.=FALSE)
	}
	cnv <- unique(cnv)
	assign("cnv", cnv, pos=parent.frame(n=1))
	###
	### check (assign) inv
	cns <- colnames(dataset$header) # the colnames of the independent dataset
	YvarCns <- cns[grep(yPref, cns)]
	if (is.null(inv)) {
		inv <- rep(NA, length(cnv))
		for (i in 1:length(cnv)) {
			ind <- which(YvarCns == cnv[i])
			if (length(ind) == 1) {
				inv[i] <- YvarCns[ind]
			}
		}
		assign("inv", inv, pos=parent.frame(n=1))	
	}
	if (!any(is.na(inv)) ) {
		if (!all(is.character(inv)) | length(inv) != length(cnv) ) {
			stop(paste0("Please provide a character length ", length(cnv), " to the argument 'inv' or leave at the default 'NULL'."), call.=FALSE)
		}
		if (!all(inv %in% YvarCns)) {
			indNo <- which(!inv %in% YvarCns)
			if (length(indNo) == 1) {ad1 <- ""; ad2 <- "s"; ad3 <- "has" } else {ad1 <- "s"; ad2 <- ""; ad3 <- "have"}
			stop(paste0("Sorry, the numerical variable", ad1, " '", paste(inv[indNo], collapse="', '"), "' seem", ad2, " not to be present in the independent dataset '", dsName, "'. Please check your input at the argument 'inv' of leave at the default NULL.\nPossible values are: '", paste(YvarCns, collapse="', '"), "'."), call.=FALSE)
		}
		indNo <- which(sapply(inv, function(x) !all(is.numeric(dataset$header[,x])) ))
		if (length(indNo) > 0) {
			if (length(indNo) == 1) {ad1 <- ""; ad2 <- "s"; ad3 <- "has" } else {ad1 <- "s"; ad2 <- ""; ad3 <- "have"}
			stop(paste0("Sorry, the provided variable", ad1, " '", paste(inv[indNo], collapse="', '"), "' at the independent dataset '", dsName, "' seem", ad2, " not to be entirely numerical.\nPlease check (or re-import) the dataset '", dsName, "'."), call.=FALSE)
		}
	} # end !is.null(inv)
	###
	if (all(toxls == "def")) {
		toxls <- .ap2$stn$plsr_indepPred_exportToExcel
	}
	if (!all(is.logical(toxls)) | length(toxls) != 1) {
		stop("Please provide either TRUE or FALSE to the argument 'toxls' resp. to the argument 'plsr_indepPred_exportToExcel' in the settings file.", call.=FALSE)
	}
	assign("toxls", toxls, pos=parent.frame(n=1))
	###
	if (all(psd == "def")) {
		psd <- .ap2$stn$plsr_plot_secondaryData
	}
	if (!all(is.logical(psd)) | length(psd) != 1) {
		stop("Please provide either TRUE or FALSE to the argument 'psd' resp. to the argument 'plsr_plot_secondaryData' in the settings file.", call.=FALSE)
	}
	assign("psd", psd, pos=parent.frame(n=1))
	###	
} # EOF

printMessagePairing <- function(cnv, inv) {
	cat(paste0("The following numeric variables are used for prediction and the corresponding validation:\n\n"))
	textDF <- data.frame(cnv, inv)
	colnames(textDF) <- c("Prediction (model)", "    Validation (indep.data)")
	print(textDF)
	cat("\n")
	return(invisible(NULL))
} # EOF

adaptIndepDatasetForWls <- function(indepDataset, cubeDataset) {
	# the cube must rule !! as here is the model: we have to cut down the independent dataset to the measure of the cube, if not all (?tolerance?) of the cube is also within the independent dataset, we can not do it.
	wlsInd <- getWavelengths(indepDataset)
	wlsCube <- getWavelengths(cubeDataset)
#	nmTol <- .ap2$stn$plsr_indepPred_tolerance_nm
#	minOut <- maxOut <- FALSE
#	if (min(wlsCube) < (min(wlsInd) - nmTol) ) {
#		minOut <- TRUE
#		# do something ?
#	}
#	if (max(wlsCube) > (max(wlsInd) + nmTol)) {
#		maxOut <- TRUE
#		# do something ?
#	}
#	if (minOut | maxOut) {
#		return(NULL)
#	}
#	sel <- which(wlsInd >= min(wlsCube) & wlsInd <= max(wlsCube)) # get the indices in the wlsInd of those wavelength that are within the cube (if out of tolerance we get out NULL, above)
#	if (length(sel) < length(wlsCube)) {
#		sel <- sel[1:blablabla] ## have interpolation please !!! XXX
#	}
	if (!all(wlsCube %in% wlsInd)) { # we must have an exact match of the wavelengths
		return(NULL)
	}
	sel <- which(wlsInd %in% wlsCube) # for an exact match !!
	## we would need some nice wavelength interpolation for those cases that do not have exact matching wavelengths!! XXX
	indepDataset$NIR <- indepDataset$NIR[,sel]
	return(indepDataset)
} # EOF

calculateIndepPlsPrediction <- function(indepDataset, cube, cnv, inv, ap) {
	resultList <- vector("list", length=length(cube))
	for (i in 1: length(cube)) {
		idString <- adaptIdStringForDpt(ap, getIdString(cube[[i]]))
		plsModels <- getPlsrModels(cube[[i]])
		regrOnList <- getPlsrRegrOnList(cube[[i]])
		cutIndepData <- adaptIndepDatasetForWls(indepDataset, getDataset(cube[[i]])) # returns NULL if independent dataset is out of range of cube-dataset
		#
		if (!all(cnv %in% regrOnList)) { stop("An error has occured. We are truly sorry.") } # This theoretically should never happen.
		#
		if (!is.null(cutIndepData)) { # so we get a dataset, meaning that our independent dataset is within the wavelengths of the current cube dataset
			innerList <- vector("list", length=length(cnv))
			for (k in 1: length(cnv)) { # go through all the cube numeric variables and their possible validation 
				ind <- which(regrOnList == cnv[k])
				singleModel <- plsModels[[ind]]
				modRegrOn <- regrOnList[[ind]] # same as cnv[k]
				indVal <- indepValid <- inv[k] # the independent validation: now is NA or a character naming a numerical variable
				if (is.na(indVal)) {
					indVal <- rep(NA, nrow(cutIndepData))
				} else { # so it must be a character naming an existing column
					indVal <- cutIndepData$header[,indVal] # now we have the numeric values in indVal
				}
				newData <- data.frame(yvar=indVal, allData=cutIndepData$NIR)
				nc <- singleModel$ncomp 
			 	predResult <- predict(singleModel, comps=1:nc, newdata = newData)
				R2pr <- as.numeric(pls::R2(singleModel, estimate = "test", newdata = newData, ncomp = nc, intercept = FALSE)$val) # returns NA if we do not have data in the yvar
				RMSEP <- as.numeric(pls::RMSEP(singleModel, estimate = "test", newdata = newData, ncomp = nc, intercept = FALSE)$val) # returns NaN (which still is.na=TRUE)
				innerList[[k]] <- list(modRegrOn=modRegrOn, indepValid=indepValid, idString=idString, predResult=predResult, indepValue=indVal, R2pr=R2pr, RMSEP=RMSEP, ncomp=nc)
			} # end for k
			resultList[[i]] <- innerList
		} else { # so cutIndepData comes in as NULL, meaning we are not within the wavelengths of the current cube datset
			resultList[[i]] <- NULL			
		} # end if !is.null(cutIndepData)
	} # end for i
	return(resultList)
} # EOF

maybeMakeSwarm <- function(ind, DF, cols=1, plSwarm=.ap2$stn$plsr_plotDataInSwarm, prio="density", swCex=.ap2$stn$plsr_cexForSwarm, xsi=NULL) {
	if (length(cols)==1) {
		pwColor <- rep(cols, nrow(DF))
	} else {
		pwColor <- cols
	}
#	if (length(ind) == 0) {stop("Swarm Error")} # 
	if (length(ind) > 1) {
		if (plSwarm) {
			if (is.null(xsi)) {
				xsiUse <- 0.08
			} else {
				xsiUse <- xsi
			}
			locDF <- beeswarm::swarmx(DF[ind, "xval"], DF[ind, "yval"], priority=prio, cex=swCex, xsize=xinch(xsiUse))		
			locDF$color <- pwColor[ind]
		#	locDF <- data.frame(x=DF[ind, "xval"], y=DF[ind, "yval"], color=pwColor[ind]) # for debugging
		} else {
			locDF <- data.frame(x=DF[ind, "xval"], y=DF[ind, "yval"], color=pwColor[ind], stringsAsFactors=FALSE)
		}
	} else {
		locDF <- data.frame(x=DF[ind, "xval"], y=DF[ind, "yval"], color=pwColor[ind], stringsAsFactors=FALSE)				
	}
	return(locDF)
} # EOF

plotPlsrErrorPoints <- function(DF, colors, xlab=NULL, ylab=NULL, mainText=NULL, subText=NULL, ppch, pointsOnly=FALSE, ...) {
	if (all(is.numeric(colors))) {
		class(colors) <- "numeric"
	}
	usePureRangeX <- .ap2$stn$plsr_usePureRangeX
	#
	uniX <- unique(DF[,"xval"])
	xStep <- abs(min(diff(sort(uniX))))
	if (xStep < 1) {
		xStep <- 0.001 # trial and errror !! (?)
	}
	if (usePureRangeX) { # if TRUE, then we use the default xsize from the package beeswarm (default in the settings is FALSE)
		xStep <- NULL
	}
	indMin <- which(DF[,"xval"] == min(uniX)) 
	minX <- min(maybeMakeSwarm(indMin, DF, xsi=xStep)$x) # get min range
	indMax <- which(DF[,"xval"] == max(uniX))
	maxX <- max(maybeMakeSwarm(indMax, DF, xsi=xStep)$x) # get max range
	xRange <- c(minX, maxX)
	if (!pointsOnly) {
		plot(NA, xlim=xRange, ylim=range(DF[, "yval"]), xlab=xlab, ylab=ylab, main=mainText, sub=subText, ...) # make an empty plot
	}
	plotList <- vector("list", length=length(uniX))
	for (m in 1: length(uniX)) {
		ind <- which(DF[,"xval"] == uniX[m])
		plotList[[m]] <- maybeMakeSwarm(ind, DF, cols=colors)
	} # end for m
		lapply(plotList, function(le) {
			points(x=le$x, y=le$y, col=le$color, pch=ppch)
		})
	#
	return(invisible(plotList))
} # EOF	
				
makeIndepPlsrValidationPlots_inner <- function(cube, indepDataset, predResults, ap, onMain, onSub, where, inRDP, colorBy, dsName, psd) {
	# psd: logical for plot secondary data or not
	colLmTrain <- .ap2$stn$plsr_color_lm_training
	colLmCV <- .ap2$stn$plsr_color_lm_crossvalid
	colLmPred <- .ap2$stn$plsr_color_lm_indepPred
	ltTarg <- .ap2$stn$plsr_linetypeTargetLine
	ltLm <- .ap2$stn$plsr_linetypeLinearModel
	rnd <- .ap2$stn$plsr_nrDigitsRMSEx
	plotSwarm <- .ap2$stn$plsr_plotDataInSwarm
	secAlpha <- .ap2$stn$plsr_color_alpha_secondaryData
	pchPrim <- .ap2$stn$plsr_color_pch_primaryData
	pchSec <- .ap2$stn$plsr_color_pch_secondaryData
	#
	dataset <- indepDataset
	header <- getHeader(dataset)
	if (is.null(colorBy)) {
		color <- 1
		colorMsg <- ""
		colLegend <- FALSE
	} else {
		clv <- extractColorLegendValues(dataset, groupBy=colorBy) # returns a list: color_data, color_unique, color_legend, txt, txtE, sumPart, dataGrouping, pch_data, pch_legend
		color <- clv$color_data
		colorMsg <- " color by: "
		colLegend <- TRUE
	} # end else
	for (i in 1: length(predResults)) { # has the same length as the cube; we could take the cube here instead
		plsModels <- getPlsrModels(cube[[i]])
		regrOnList <- getPlsrRegrOnList(cube[[i]])
		for (k in 1: length(predResults[[i]])) {
			# prediction
			pred <- predResults[[i]][[k]]
			R2pr <- round(pred$R2pr, rnd)
			RMSEP <- round(pred$RMSEP, rnd)
			ncomp <- pred$ncomp
			# calibration
			ind <- which(regrOnList == pred$modRegrOn) # we have to be careful with the sorting, as the user input might have a different order !!
			singleModel <- plsModels[[ind]]
			RMSEC <- getRMSEC(singleModel)
			RMSEC_rdp <- convertToRDP(RMSEC, pred$modRegrOn, header)
			R2C <- getR2C(singleModel)	
			# crossvalidation
			RMSECV <- getRMSECV(singleModel)
			RMSECV_rdp <- convertToRDP(RMSECV, pred$modRegrOn, header)
			R2CV <- getR2CV(singleModel)
			#
			yvar <- singleModel$model$yvar
			yvarFittedCalib <- singleModel$fitted.values[ , , ncomp]
			yvarFittedCV <- singleModel$validation$pred[ , , ncomp]	
			datCV <- data.frame(xval=yvar, yval=yvarFittedCV)
			#
			if (all(is.na(pred$indepValue))) { # so we had NO validation data
				# do nothing for the moment
			} else {
				RMSEP_rdp <- convertToRDP(RMSEP, pred$indepValid, header)
				mainText <- paste0("plsr mod. (", pred$modRegrOn, ") from: ", onMain, pred$idString)
				regrOnMsg <- paste0("   indep. data from '", dsName, "', predicted value: ", pred$indepValid, "   ")
				ncompMsg <- paste0("   ", ncomp, " comps.")
				Nmsg <- paste0("   N=", nrow(header))
				subText <- paste0(onSub, regrOnMsg, colorMsg, colorBy, ncompMsg, Nmsg)
				#
				datPred <- data.frame(xval=as.numeric(pred$indepValue), yval=as.numeric(pred$predResult))
				xlab <- "measured value"
				ylab <- "independent predicted value"
				######
				plotPlsrErrorPoints(DF=datPred, colors=color, xlab, ylab, mainText, cex.main=0.9, subText, ppch=pchPrim) ### CORE ### CORE	
				legPchSec <- NA
				if (psd) {
					plotPlsrErrorPoints(DF=datCV, colors=makeColorsTransparent(color, secAlpha), ppch=pchSec, pointsOnly=TRUE) 
					legPchSec <- pchSec
				}
				######
				abline(0,1, col="gray", lty=ltTarg, lwd=1)
				abline(lm(yvarFittedCalib ~ yvar), lty=ltLm, lwd=1, col=colLmTrain) 
				abline(lm(yvarFittedCV ~ yvar), lty=ltLm, lwd=1, col=colLmCV) 	
				if (length(unique(pred$indepValue)) > 1) {
					abline(lm(pred$predResult ~ pred$indepValue), lty=ltLm, lwd=1, col=colLmPred) # fitting linear model
				}
				#
				nrComps <- paste0("# comps.: ", ncomp)
				rmsec <- paste0("RMSEC: ", RMSEC)
				rmsec_rdp <- paste0("RMSEC[RDP]: ", RMSEC_rdp)
				r2c <- paste0("R2C: ", R2C)
				rmsecv <- paste0("RMSECV: ", RMSECV)
				rmsecv_rdp <- paste0("RMSECV[RDP]: ", RMSECV_rdp)
				r2cv <- paste0("R2CV: ", R2CV)	
				rmsep <- paste0("RMSEP: ", RMSEP)
				rmsep_rdp <- paste0("RMSEP[RDP]: ", RMSEP_rdp)
				r2pr <- paste0("R2pr: ", R2pr)
				if (inRDP) {
					legendText <- c(nrComps, rmsec, rmsec_rdp, r2c, rmsecv, rmsecv_rdp, r2cv, rmsep, rmsep_rdp, r2pr)
					legTxtCol <- c("black", rep(colLmTrain, 3), rep(colLmCV, 3), rep(colLmPred, 3))
					legPch <- c(rep(NA, 4), rep(legPchSec, 3), rep(pchPrim, 3))
				} else {
					legendText <- c(nrComps, rmsec, r2c, rmsecv, r2cv, rmsep, r2pr)
					legTxtCol <- c("black", rep(colLmTrain, 2), rep(colLmCV, 2), rep(colLmPred, 2))
					legPch <- c(rep(NA, 3), rep(legPchSec, 2), rep(pchPrim, 2))
				}
				legBgCol <- rgb(255,255,255, alpha=.ap2$stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings	
				legCex <- 0.8
				legend("topleft", legend=legendText, text.col=legTxtCol, bg=legBgCol, cex=legCex, pch=legPch)	
				if (colLegend) {
					legend("bottomright", legend=clv$txtE, col=clv$color_legend, pch=pchPrim, bg=legBgCol, cex=legCex)
				}
			} # end else
		} # end for k
	} # end for i
} # EOF

makeIndepPlsrValidationPlots <- function(cube, indepDataset, predResults, anp2, dsName, psd) {
	ap <- anp2
	#
	where <- ap$genPlot$where
	onMain <- ap$genPlot$onMain
	onSub <- ap$genPlot$onSub
	fns <- ap$genPlot$fns
  	inRDP <- ap$plsr$inRdp
  	colorBy <- ap$plsr$colorBy
	#
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("Plotting PLSR error plots... ")}
	expName <- getExpName(cube)
	height <-.ap2$stn$pdf_Height_sq
	width <- .ap2$stn$pdf_Width_sq
	path <- .ap2$stn$fn_results
	suffix <- "plsIndepPred"
	message <- "PLSR Preds"
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	onMain <- paste(expName, onMain, sep=" ")
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
	if (where != "pdf" & Sys.getenv("RSTUDIO") != 1) {dev.new(height=height, width=width)}	
	makeIndepPlsrValidationPlots_inner(cube, indepDataset, predResults, ap, onMain, onSub, where, inRDP, colorBy, dsName, psd) ### HERE ###
	if (where == "pdf") { dev.off() }
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF

exportPredListToExcel <- function(cube, predList, anp2) {
	ap <- anp2
	#
	fns <- ap$genPlot$fns
	onMain <- ap$genPlot$onMain
	expName <- getExpName(cube)
	path <- .ap2$stn$fn_results
	cPref <- .ap2$stn$p_yVarPref
	suffix <- "plsIndepPred"
	filename <- paste(expName, suffix, sep="_")
	filename <- paste0(path, "/", filename, fns, ".xlsx")
	onMain <- paste(expName, onMain, sep=" ")
	#
	wb <- openxlsx::createWorkbook(Sys.getenv("USER"))
	cnt <- 1
	for (i in 1: length(predList)) {
		for (k in 1: length(predList[[i]])) {
			pr <- predList[[i]][[k]]
			mod <- pr$modRegrOn
			mod <- substr(mod, nchar(cPref)+1, nchar(mod)) # cut away the C_
			val <- pr$indepValid
			if (!is.na(val)) {
				val <- substr(val, nchar(cPref)+1, nchar(val)) # cut away the C_
				if (mod == val) {
					add <- mod
				} else {
					add <- paste(mod, val, sep=".")
				}
			} else {
				add <- paste0(mod, ".NA")
			}
			wsname <- paste0("# ",i, "-", k, " ", add)
			if (nchar(wsname) > 31 ) {
				wsname <- substr(wsname, 1, 31)
			}
			openxlsx::addWorksheet(wb, wsname)
			dataHeader <- data.frame(pr$idString, pr$modRegrOn, pr$indepValid, pr$R2pr, pr$RMSEP, pr$ncomp)
			colnames(dataHeader) <- c("Cube Object", "model regr. on", "indep. validation", "R2 pred.", "RMSEP", "nr. comps.")
			openxlsx::writeData(wb, sheet=cnt, dataHeader, keepNA=TRUE)		
			dataObject <- data.frame(rownames(pr$predResult), pr$predResult)
			colnames(dataObject) <- c("row name", "predicted value")
			openxlsx::writeData(wb, sheet=cnt, dataObject, startRow=5)
			cnt <- cnt + 1
		} # end for k
	} # end for i
	openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
} # EOF

pls_pred_checkColorBy <- function(indepDataset, anp2, dsName) {
	cPref <- .ap2$stn$p_ClassVarPref
	colorBy <- anp2$plsr$colorBy
	if (!is.null(colorBy)) {
		if (!all(is.character(colorBy)) | length(colorBy) != 1) {
			stop(paste0("Please provide a length one character for the argument 'pls.colorBy' in the analysis procedure resp. in your input."), call.=FALSE)
		}
		cns <- colnames(indepDataset$header)
		cnsC <- cns[grep(cPref, cns)]
		if (!colorBy %in% cnsC) {
			stop(paste0("Sorry, the class-variable '", colorBy, "' seems not to exist in the independent dataset '", dsName, "'.\nPlease check your input at the argument 'pls.colorBy' resp. there in the analysis procedure.\nPossible values are: '", paste(cnsC, collapse="', '"), "'."), call.=FALSE)
		}
	}
} # EOF


#' @title Independent plsr prediction
#' @description Use independent data for predictions in the pls-models within 
#' the cube and plot those predictions and, if available, the validation data.
#' @details Please see the documentation for the single parameters (\code{cnv} 
#' and \code{inv}) to see how the selection of the available models and the 
#' selection of possibly available data for validation is handled. For coloring, 
#' the argument code{pls.colorBy} of the analysis procedure can be used, but 
#' here the data for the colors are selected from the \strong{independent} 
#' dataset. If the parameter \code{plsr_indepPred_exportToExcel} in the 
#' settings file is set to TRUE, the results are exported to an excel file in the 
#' results folder as well.
#' @param indepDataset The dataset containing the independent data. An object 
#' of class 'aquap_data' as produced by \code{\link{gfd}}. 
#' @param cube An object of class 'aquap_cube' as produced by \code{\link{gdmm}}. 
#' It is an error to have no plsr models in the cube.
#' @param cnv "Cube numeric variable", character vector or NULL. The names of 
#' one or more numeric variables in the cube on which models have been 
#' calculated. Leave at the default NULL to use \strong{all} of the numeric 
#' variables on which a plsr model has been calculated, or provide a character 
#' vector with valid variable names for a sub-selection. For the selected 
#' variables, predictions from the data in the independent dataset will be made. 
#' If argument \code{inv} is left at its default NULL, numerical variables with 
#' exactly the same name are looked for in the independent dataset and, if 
#' present, are used for validating the predictions.
#' @param inv "Independent numeric variable", character vector or NULL. The names 
#' of numeric variables in the independent dataset. If left at the default 
#' NULL, numerical variables in the independent dataset with exactly the same 
#' name(s) as specified in argument \code{cnv} are looked for and, if present, 
#' are used for validating the predictions. If a character vector is provided, 
#' it has to have the same length as the one in \code{cnv}, and those variables 
#' will be used, in the given sequence, for validating the predictions.
#' @param psd 'plot secondary data'; either character 'def' or logical. If 
#' secondary (i.e.crossvalidation data) should be plotted as well. Leave at the 
#' default 'def' to take the value from the parameter 
#' \code{plsr_plot_secondaryData} in the settings file, or provide TRUE or FALSE.
#' The alpha level for the secondary data can be set in parameter 
#' \code{plsr_color_alpha_secondaryData} in the settings file.
#' @param aps Character length one. The default way to obtain the analysis 
#' procedure. Defaults to "def". Possible values are:
#' \describe{
#' \item{"def"}{The default from the settings.r file is taken. (Argument 
#' \code{gen_plot_anprocSource})}
#' \item{"cube"}{Take the analysis procedure from within the cube, i.e. the 
#' analysis procedure that was used when creating the cube via \code{\link{gdmm}}
#' is used.}
#' \item{"defFile"}{Use the analysis procedure with the default filename as 
#' specified in the settings.r file in \code{fn_anProcDefFile}.}
#' \item{Custom filename}{Provide any valid filename for an analysis procedure to 
#' use as input for specifying the plotting options.}
#' }
#' @param pl Logical, defaults to TRUE. If predicted data should be plotted 
#' at all. If FALSE, only the calculation and the (possible) export to an excel 
#' file (see details) will be performed.
#' @param toxls 'def' or logical. If left at the default 'def' the value from 
#' \code{plsr_indepPred_exportToExcel} in the settings file is used. Set to TRUE 
#' or FALSE to directly control if export of predicted data to excel should be 
#' performed or not.
#' @param ... Arguments for overriding one or more of the plotting parameters 
#' from the analysis procedure, please see \code{\link{plot_pls_args}}.
#' @return An (invisible) list containing the numerical results of the 
#' predictions, and if parameter \code{plsr_indepPred_exportToExcel} in the 
#' settings file is set to TRUE, these data are exported to an excel file in the 
#' results folder as well.
#' @examples
#' \dontrun{
#' fd <- gfd()
#' cu <- gdmm(fd) # assumes that you have plsr enabled in the analysis procedure
#' fdIndep <- fd # !!!pretend!!! that fdIndep is now an other, independent dataset
#' predList <- plot_pls_indepPred(fdIndep, cu)
#' predList <- plot_pls_indepPred(fdIndep, cu, cnv="Y_Temp") # to only use the 
#' # models regressed on 'Y_Temp'
#' predList <- plot_pls_indepPred(fdIndep, cu, cnv="Y_Temp", inv="Y_fooBar")
#' # to only use the models regressed on 'Y_Temp', and use the numeric variable 
#' # 'Y_fooBar' from the independent dataset for validation
#' predList <- plot_pls_indepPred(fdIndep, cu, aps="fooBar.r") # use the values 
#' from the analysis procedure file 'fooBar.r' for plotting
#' predList <- plot_pls_indepPred(fdIndep, cu, pl=FALSE) # no plotting, just 
#' # calculation and possible export to excel.
#' predList <- plot_pls_indepPred(fdIndep, cu, pls.colorBy="C_Group") # use the 
#' # class variable 'C_Group' from the independent dataset for coloring
#' predList <- plot_pls_indepPred(fdIndep, cu, pg.where="pdf", pg.fns="_fooBar") 
#' # add the string '_fooBar' to the generated pdfs.
#' predList <- plot_pls_indepPred(fdIndep, cu, toxls=FALSE) # no exporting to xls
#' }
#' @family PLSR documentation
#' @family Plot functions
#' @export
plot_pls_indepPred <- function(indepDataset, cube, cnv=NULL, inv=NULL, psd="def", aps="def", pl=TRUE, toxls="def", ...) { 
	autoUpS()
	dsName <- deparse(substitute(indepDataset))
	cubeName <- deparse(substitute(cube))
	check_indPlsPrediction_input(indepDataset, cube, cnv, inv, cubeName, dsName, toxls, psd) ## !! is assigning cnv, inv, toxls, psd
	ap1 <- getap(.lafw_fromWhere="cube", cube=cube)
	anp2 <- doApsTrick(aps, cube, ...)
	pls_pred_checkColorBy(indepDataset, anp2, dsName)
#	anp2 <- ap_cleanZeroValuesCheckExistenceDefaults(anp2, dataset=indepDataset, haveExc=FALSE) # notto gutto
	if (!.ap2$stn$allSilent & .ap2$stn$plsr_indepPred_printPairingMsg) {
		printMessagePairing(cnv, inv)
	}
	if (length(cube) == 1) {ad1 <- ""} else {ad1 <- "s"};  	if (length(cnv) == 1) {ad2 <- ""} else {ad2 <- "s"}
	if (!.ap2$stn$allSilent) {cat(paste0("Calc. plsr predictions for ", length(cube), " cube element", ad1, " (", length(cnv), " model", ad2, " each)... "))}
	predList <- calculateIndepPlsPrediction(indepDataset, cube, cnv, inv, ap1) #### CORE #### calculation 
	if (!.ap2$stn$allSilent) {cat("ok\n")}
	###
	if (toxls) {
		if (!.ap2$stn$allSilent) {cat(paste0("Exporting predicted data to excel file... "))}
		exportPredListToExcel(cube, predList, anp2) #### export to excel ####
		if (!.ap2$stn$allSilent) {cat("ok\n")}
	} # end predToXls
	###
	# now plot, please (use the ap2 now!)
	if (pl) {
		makeIndepPlsrValidationPlots(cube, indepDataset, predResults=predList, anp2, dsName, psd) #### plot the results ####
	}
	###
	return(invisible(predList))
} # EOF
