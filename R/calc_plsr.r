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
	parMsg <- ""
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

check_indPlsPrediction_input <- function(indepDataset, cube, cnv=NULL, inv=NULL, cubeName, dsName) { 
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


templateForBelow <- function(plsModel, dataset, regrOn, classFCol, onMain="", onSub="",  inRDP=FALSE) {
	header <- getHeader(dataset)
	if (is.null(classFCol)) {
		color <- 1
		colorMsg <- ""
		colLegend <- FALSE
	} else {
		clv <- extractColorLegendValues(dataset, groupBy=classFCol) # returns a list: color_data, color_unique, color_legend, txt, txtE, sumPart, dataGrouping, pch_data, pch_legend
		color <- clv$color_data
		colorMsg <- " color by: "
		colLegend <- TRUE
	}
	RMSEC <- getRMSEC(plsModel)
	RMSEC_rdp <- convertToRDP(RMSEC, regrOn, header)
	R2C <- getR2C(plsModel)
	regrOnMsg <- paste("   regr. on: ", regrOn, "   ",sep="")
	ncompMsg <- paste("   ", plsModel$ncomp, " comps.", sep="")
	Nmsg <- paste("   N=", nrow(header), sep="")
	subText <- paste(onSub, regrOnMsg, colorMsg, classFCol, ncompMsg, Nmsg, sep="")
	pls::predplot(plsModel, which="train" , main=paste(onMain, "- Training"), sub=subText, col=color)
	abline(0,1,col="gray")
	if (inRDP) {
		legendText <- paste("RMSEC: ", RMSEC, "\nRMSEC[RDP]: ", RMSEC_rdp, "\nR2C: ", R2C, "\n\n ", sep="")
	} else {
		legendText <- paste("RMSEC: ", RMSEC, "\nR2C: ", R2C, sep="")
	}
	legend("topleft", legend=legendText)
	if (colLegend) {
		legBgCol <- rgb(255,255,255, alpha=.ap2$stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings
		legend("bottomright", legend=clv$txtE, col=clv$color_legend, pch=16, bg=legBgCol)
	}
} # EOF


makeIndepPlsrValidationPlots_inner <- function(cube, predResults, ap, onMain, onSub, where, inRDP) {

} # EOF


makeIndepPlsrValidationPlots <- function(cube, predResults, ap2) {
	ap <- ap2
	#
	where <- ap$genPlot$where
	onMain <- ap$genPlot$onMain
	onSub <- ap$genPlot$onSub
	fns <- ap$genPlot$fns
  	inRDP <- ap$plsr$inRdp
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
	makeIndepPlsrValidationPlots_inner(cube, predResults, ap, onMain, onSub, where, inRDP) ### HERE ###
	if (where == "pdf") { dev.off() }
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF


#' @title Independent plsr prediction
#' @description Use independent data for predictions in the pls-models within 
#' the cube and plot those predictions and, if available, the validation data.
#' @details Please see the documentation for the single parameters to see how 
#' the selection of the available models and the selection of possibly available 
#' data for validation is handled.
#' @param indepDataset The dataset containing the independent data. An object 
#' of class 'aquap_data' as produced by \code{\link{gfd}}. 
#' @param cube An object of class 'aquap_cube' as produced by \code{\link{gdmm}}. 
#' It is an error to have no plsr models in the cube.
#' @param cnv Character vector or NULL. The names of one or more numeric 
#' variables in the cube on which models have been calculated. Leave at the 
#' default NULL to use \strong{all} of the numeric variables on which a plsr model 
#' has been calculated, or provide a character vector with valid variable names 
#' for a sub-selection. For the selected variables, predictions from the data in 
#' the independent dataset will be made. If argument \code{inv} is left 
#' at its default NULL, numerical variables with exactly the same name are looked 
#' for in the independent dataset and, if present, are used for validating the 
#' predictions.
#' @param inv Character vector or NULL. The names of numeric 
#' variables in the independent dataset. If left at the default NULL, numerical 
#' variables in the independent dataset with exactly the same name(s) as specified 
#' in argument \code{cnv} are looked for and, if present, are used for 
#' validating the predictions. If a character vector is provided, it has to have 
#' the same length as the one in \code{cnv}, and those variables will be 
#' used, in the given sequence, for validating the predictions.
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
#' @param ... For overriding one or more of the plotting parameters, 
#' please see \code{\link{plot_pls_args}}.
#' @return An (invisible) list containing the numerical results of the 
#' predictions.
#' @examples
#' \dontrun{
#' XXX	
#' }
#' @family PLSR documentation
#' @export
plot_pls_indepPred <- function(indepDataset, cube, cnv=NULL, inv=NULL, aps="def", ...) { 
	autoUpS()
	dsName <- deparse(substitute(indepDataset))
	cubeName <- deparse(substitute(cube))
	check_indPlsPrediction_input(indepDataset, cube, cnv, inv, cubeName, dsName) ## !! is assigning cnv, inv
	ap1 <- getap(.lafw_fromWhere="cube", cube=cube)
	ap2 <- doApsTrick(aps, cube, ...)
	if (!.ap2$stn$allSilent) {printMessagePairing(cnv, inv)}
	if (length(cube) == 1) {ad1 <- ""} else {ad1 <- "s"};  	if (length(cnv) == 1) {ad2 <- ""} else {ad2 <- "s"}
	if (!.ap2$stn$allSilent) {cat(paste0("Calc. plsr predictions for ", length(cube), " cube element", ad1, " (", length(cnv), " model", ad2, " each)... "))}
	predList <- calculateIndepPlsPrediction(indepDataset, cube, cnv, inv, ap1) #### CORE #### calculation 
	if (!.ap2$stn$allSilent) {cat("ok\n")}
	###
	# now plot, please (use the ap2 now!)
	makeIndepPlsrValidationPlots(cube, predResults=predList, ap2)
	###
	return(invisible(predList))
} # EOF
