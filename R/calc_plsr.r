makePLSRModel_inner <- function(dataset, Y_Class, niter=5, ncomp=NULL, valid="CV", stnLoc) {
	nrSwitch <- stnLoc$plsr_nrCompsSwitchToNrObserv		## from the local settings
	addComps <- stnLoc$plsr_addComps
	acb <- stnLoc$plsr_addCompsBoundaries
	facObs <- stnLoc$plsr_percentObservAsMaxNcomp / 100
	header <- getHeader(dataset)
	dataset <- data.frame(yvar=header[,Y_Class], allData=dataset$NIR ) ### here make a new "flat" dataset
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
					testModel <- pls::plsr(yvar ~ allData, data=dataset, validation=valid)
					a <- pls::RMSEP(testModel, intercept=FALSE, estimate="CV")$val
					ind <- which.min(a)
		#			if ( (!allow1C) & (ind == 1)) { ind <- order(a)[2] }
					minErrorVec[i] <-  ind
				} # end for i niter		
			} else {						## so if we have a lot of rows !!!
				for (i in 1: niter) {
					testModel <- pls::plsr(yvar ~ allData, data=dataset, validation=valid, ncomp=maxNcomp)
					a <- pls::RMSEP(testModel, intercept=FALSE, estimate="CV")$val
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
	plsModelCorrect <- pls::plsr(yvar ~ allData, ncomp=bestNC, data=dataset, validation=valid)	#### here it happens !!
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
	plsModelPlus <-  pls::plsr(yvar ~ allData, ncomp=bestNC+add, data=dataset, validation=valid)	#### here it happens !!
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
	if (leng == 1) { # to avoid confusing messages like "doing 1 model in parallel"
		doPar <- FALSE
		parMsg  <- maybeAssignPlsrClusterToPlsrFunction()
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
		parallel::stopCluster(.ap2$.plsrClust)
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

check_indPlsPrediction_input <- function(indepDataset, cube, cubeNumVar, indepNumVar=NULL) { 
	dataset <- indepDataset
	yPref <- .ap2$stn$p_yVarPref
	dsName <- deparse(substitute(indepDataset))
	cubeName <- deparse(substitute(cube))
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
	#
	if (!all(is.character(cubeNumVar)) | length(cubeNumVar) != 1) {
		stop("Please provide a character length one to the argument 'cubeNumVar'.", call.=FALSE)
	}
	cns <- colnames(getDataset(cube[[1]])$header) # all datasets must have the same header structure, so just take the first
	regOn <- getAnproc(cube)$plsr$regressOn
	if (!cubeNumVar %in% cns | !cubeNumVar %in% regOn) {
	#	possYvarTotal <- cns[grep(yPref, cns)]
		stop(paste0("Sorry, the numerical variable '", cubeNumVar, "' seems not to be present in the cube '", cubeName, "' or has not been used to calculate a plsr model. \nPlease check your input at the argument 'cubeNumVar'.\nPossible values for numerical variables on which a plsr-model has been calculated:\n   ", paste(regOn, collapse=", ")), call.=FALSE)
	}
	###
	if (!is.null(indepNumVar)) { # so we must have a character: check for existence in the independent dataset
		if (!all(is.character(indepNumVar)) | length(indepNumVar) != 1) {
			stop("Please provide a character length one to the argument 'indepNumVar' or leave at the default 'NULL'.", call.=FALSE)
		}
		cns <- colnames(dataset$header)
		YvarCns <- cns[grep(yPref, cns)]
		if (!indepNumVar %in% YvarCns) {
			stop(paste0("Sorry, the numerical variable '", indepNumVar, "' seems not to be present in the independent dataset '", dsName, "'. Please check your input at the argument 'indepNumVar'.\nPossible values are: ", paste(YvarCns, collapse=", ")), call.=FALSE)
		}
		if (!all(is.numeric(dataset$header[,indepNumVar])) ) {
		stop(paste0("Sorry, the provided variable '", indepNumVar, "' at the independent dataset '", dsName,"' seems not to be an entirely numerical variable.\nPlease check (or re-import) the dataset '", dsName, "'."), call.=FALSE)
		}
	} # end if !is.null(indepNumVar) 
	###
} # EOF


#' @title Independent plsr prediction
#' @description Use independent data for predictions in the pls-models within 
#' the cube
#' @details XXX
#' @param indepDataset The dataset containing the independent data. An object 
#' of class 'aquap_data' as produced by \code{\link{gfd}}. 
#' @param cube An object of class 'aquap_cube' as produced by \code{\link{gdmm}}.
#' @param cubeNumVar Character length one. The name of a numeric variable in the 
#' cube on which the models have been calculated.
#' @param indepNumVar Character length one or NULL. The name of a numeric 
#' variable in the independent dataset. If left at the default NULL, first a 
#' numeric variable with the same name as specified in the argument 'cubeNumVar' 
#' is looked for in the independent dataset and, if this is found, is used for 
#' validation. If no numeric variable with the same name as specified in argument 
#' 'cubeNumVar' is found in the independent dataset, the data from the independent 
#' dataset are used for predictions with the model(s) from the cube, and no 
#' validation is calculated. If a valid name of a numerical variable in the 
#' independent dataset is provided, this variable is used for validation.
#' @param ... For overriding one or more of the plotting parameters, 
#' please see \code{\link{plot_pls_args}}.
#' @examples
#' \dontrun{
#' XXX	
#' }
#' @export
indepPlsPred <- function(indepDataset, cube, cubeNumVar, indepNumVar=NULL, ...) { 
	autoUpS()
	check_indPlsPrediction_input(indepDataset, cube, cubeNumVar, indepNumVar)
	ap <- getap(.lafw_fromWhere="cube", cube=cube, ...)
	if (is.null(indepNumVar)) { # look for the variable in the cube -- if not, we do not have validation
	
	}
	
	
	
	if (!is.null(indepNumVar)) {
	
	}



} # EOF
