makeSimcaModel_inner <- function(dataset, groupBy, k=0, version, stnLoc) {
	tol <- stnLoc$simca_tolerance
	kmax <- stnLoc$simca_kMax
	flatDf <- makeFlatDataFrame(dataset, groupBy)
	X <- flatDf[,-1] # all the NIR data
	grp <- flatDf[,1] # only the grouping
	if (version == "classic") {
		if (k == 0) {
			simcaMod <- rrcovHD::CSimca(grouping ~ ., data=flatDf, kmax=kmax, tol=tol) ## k=0 does not work, kmax does not work
#			simcaMod <- rrcovHD::CSimca(X, grp, kmax=kmax, tol=tol)  ## k=0 does not work, kmax does not work
		} else {
			simcaMod <- rrcovHD::CSimca(grouping ~ ., data=flatDf, k=k, kmax=kmax, tol=tol)
#			simcaMod <- rrcovHD::CSimca(X, grp, k=k, kmax=kmax, tol=tol) 		
		}
	} else {
		if (version == "robust") {
			if (k == 0) {	
				simcaMod <- rrcovHD::RSimca(grouping ~ ., data=flatDf, kmax=kmax, tol=tol)  ## k=0 does not work, but still calculating k
#				simcaMod <- rrcovHD::RSimca(X, grp, kmax=kmax, tol=tol)   ## k=0 does not work, but still calculating k
			} else {
				simcaMod <- rrcovHD::RSimca(grouping ~ ., data=flatDf, k=k, kmax=kmax, tol=tol)
#				simcaMod <- rrcovHD::RSimca(X, grp, k=k, kmax=kmax, tol=tol)  			
			}
		} else {
			stop("Please check the settings for SIMCA. One of 'classic' or 'robust' has to be supplied.", call.=FALSE)
		}
	}
	return(simcaMod)
} # EOF

makeSimcaModels <- function(dataset, groupingVector, k=0, version) {
	i <- NULL; 	modelList <- list()
	doPar <- .ap2$stn$gen_useParallel
	leng <- length(groupingVector)
	if (doPar) { registerParallelBackend() } else { registerDoSEQ() }
	dopare <- checkHaveParallel()
	if (dopare) {
		if (!.ap2$stn$allSilent) {cat(paste("Calculating ", leng, " SIMCA models in parallel...\n", sep="")) }
	}
	stnLoc <- .ap2$stn
	modelList <- foreach(i = 1:leng) %dopar% {
		if (!dopare) {
			if (!.ap2$stn$allSilent) {cat(paste("    Calc. model #", i, " of ", leng, "\n", sep="") )}
		}
		feout <- makeSimcaModel_inner(dataset, groupingVector[i], k, version, stnLoc)		
	} # end foreach i
	return(modelList)
} # EOF

makeSimcaPrediction_inner <- function(SimcaModel, newFlatData, newCorrectGrouping) {
	if (is.null(newFlatData)) {
		pred <- predict(SimcaModel, method=2)
	} else {
		pred <- predict(SimcaModel, newFlatData, method=2)
#		print(pred); print(str(pred)); print(length(pred)); wait()
#		print(newCorrectGrouping); print(length(newCorrectGrouping)); wait()
		predTable <- rrcov::mtxconfusion(newCorrectGrouping, pred@classification, prior = NULL, printit=FALSE)
		pred@ct <- predTable
	}
	return(pred)
} # EOF

makeSimcaPredictions <- function(SimcaModelList, newFlatData=NULL, newCorrectGrouping=NULL, indNew=NULL) {
	predictionsList <- list()
	leng <- length(SimcaModelList)
	msg <- "predictions"
	featObj <- NULL
	if (class(newFlatData) == "aquap_data") { 
		msg <- "CV-predictions"
		dataset <- newFlatData 	
		simcClasses <- newCorrectGrouping
		header <- getHeader(dataset)
		dataset <- getAllData(dataset)
	}	
	for (i in 1: leng) {
		if (class(dataset) == "aquap_data") { 
			newCorrectGrouping <- header[, simcClasses[i]][indNew]		## otherwise they both stay at "NULL"
			newFlatData <- dataset[indNew,]
		}
		if (!.dem$stn$allSilent) {cat(paste("    Calc. ", msg, " #", i, " of ", leng, "\n", sep="") )}
		pred <- makeSimcaPrediction_inner(SimcaModelList[[i]], newFlatData, newCorrectGrouping)
		predictionsList <- c(predictionsList, list(pred))
	} # end for i
	return(predictionsList)
} # EOF
####
calc_interclassResid <- function(XSimcaObject) {
	simOb <- XSimcaObject
	counts <- simOb@counts
	nrGroups <- length(counts)
	Ks <- simOb@k
	rawData <- simOb@X
	groups <- simOb@grp
	resMat <- matrix(NA, nrow=nrGroups, ncol=nrGroups)
	for (ii in 1: nrGroups) {
		for (pp in 1: nrGroups) {
#			ind <- as.numeric(names(simOb@pcaobj[[ii]]@od))
			ind <- which(groups == levels(groups)[ii])
			Center1 <- simOb@pcaobj[[ii]]@center
			Center2 <- simOb@pcaobj[[pp]]@center
#			X <- scale(as.matrix(rawData[ind,]), center=T, scale=F)
			X <- scale(as.matrix(rawData[ind,]), center=Center2, scale=F)
			L1 <- simOb@pcaobj[[ii]]@loadings
			L2 <- simOb@pcaobj[[pp]]@loadings			
			X_mod <- X %*% L2 %*% t(L2)
			E <- X_mod - X										## Residuals matrix
			Esum <- sum(t(apply(E, 1, function(x) x*t(x))))
			n <- counts[ii] 									## the number of samples in the ii-Group
			k <- Ks[pp] 										## the number of factor in the pp-Group
			m <- ncol(X)										## the number of variables in the ii-Group
			a <- 1/((m-k)*n)
			resMat[ii,pp] <- (a * Esum)^0.5			
		} # end for pp
	} # end for ii
	grpChar <- levels(groups)
	rownames(resMat) <- grpChar
	colnames(resMat) <- paste(grpChar, "@", Ks, sep="")
	out <- resMat
} # EOF

calc_interclassDist_mat <- function(residMatrix) {
	a <- nrow(residMatrix)
	resMat <- matrix(NA, a, a)
	mat <- residMatrix
#	for (i in 1: (a-1)) {
#		for (k in (i+1): a) {
#			dist <- ((mat[]^2 + mat[]^2) /(mat[i,i]^2 + mat[]^2))^0.5 - 1
#		} # end k				
#	} # end i

	for (i in 1: (a)) {
		for (k in (1): a) {
			distance <- ((mat[i,k]^2 + mat[k,i]^2) /(mat[i,i]^2 + mat[k,k]^2))^0.5 - 1
			resMat[i,k] <- distance
		} # end k				
	} # end i
	colnames(resMat) <- colnames(mat)
	rownames(resMat) <- rownames(mat)
	return(resMat)
} # EOF
## Core ##
calc_interclassDist <- function(XSimcaObject) {
	resids <- calc_interclassResid(XSimcaObject)
	out <- calc_interclassDist_mat(resids)
} # EOF
####
calculateInterclassDistances <- function(modelList) {
	distList <- list()
	leng <- length(modelList)
	for (i in 1: leng) {
		if (!.dem$stn$allSilent) {cat(paste("    Calc. interclass distances #", i, " of ", leng, "\n", sep="") )}
		x <- calc_interclassDist(modelList[[i]])
		distList <- c(distList, list(x))	
	} # end for i
	return(distList)
} # EOF
