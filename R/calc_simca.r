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

makeSimcaModels <- function(dataset, groupingVector, k=0, version, inCV=FALSE) {
	i <- NULL; 	modelList <- list()
	leng <- length(groupingVector)
	stnLoc <- .ap2$stn
	modMsg <- " mod"
	if (inCV) {
		modMsg <- " CV"
	}
	if (!.ap2$stn$allSilent) {cat(paste(modMsg, "...", sep="")) }
#	doPar <- .ap2$stn$gen_useParallel
#	if (doPar) { registerParallelBackend() } else { registerDoSEQ() }
#	if ( checkHaveParallel() ) { parFill <- ".par" }
#	modelList <- foreach(i = 1:leng) %dopar% {
#		a <- try(makeSimcaModel_inner(dataset, groupingVector[i], k, version, stnLoc), silent=TRUE)
#		if (class(a) == "try-error") {
#			.ap2$.gs <- "*error*"
#			.ap2$.charCollect <- c(.ap2$.charCollect, groupingVector[i])
#			out <- NULL
#		} else {
#			out <- a
#		}
#		out # that gets returned
#	} # end foreach i
	#
	for (i in 1: leng) {
	#	mod <- makeSimcaModel_inner(dataset, groupingVector[i], k, version, stnLoc)
		a <- try(makeSimcaModel_inner(dataset, groupingVector[i], k, version, stnLoc), silent=TRUE)
		if (class(a) == "try-error") {
			.ap2$.gs <- "\n         ***error***:"
			if (inCV) {aa <- "cv."} else {aa <- ""}
			add <- paste(aa, groupingVector[i], sep="")
			.ap2$.charCollect <- paste(.ap2$.charCollect, add, sep=", ")
			modelList <- c(modelList, list(NULL))			
		} else {
			modelList <- c(modelList, list(a))
		}
	#	modelList <- c(modelList, list(mod))
	} # end for i
	#
	return(modelList)
} # EOF

makeSimcaPrediction_inner <- function(SimcaModel, newFlatData, newCorrectGrouping) {
	if (is.null(newFlatData)) {
		pred <- rrcov::predict(SimcaModel, method=2)
	} else {
		pred <- rrcov::predict(SimcaModel, newFlatData, method=2)
#		print(pred); print(str(pred)); print(length(pred)); wait()
#		print(newCorrectGrouping); print(length(newCorrectGrouping)); wait()
		predTable <- rrcov::mtxconfusion(newCorrectGrouping, pred@classification, prior = NULL, printit=FALSE)
		pred@ct <- predTable
	}
	return(pred)
} # EOF

makeSimcaPredictions <- function(SimcaModelList, newFlatData=NULL, newCorrectGrouping=NULL, indNew=NULL, inCV=FALSE) {
	predictionsList <- list()
	leng <- length(SimcaModelList)
	msg <- "predictions"
	dataset <- NULL
	if (class(newFlatData) == "aquap_data") { 
		msg <- "CV-predictions"
		dataset <- newFlatData 	
		simcClasses <- newCorrectGrouping
	}
	if (!.ap2$stn$allSilent & !inCV) {cat(paste(" pred...", sep="") )}
	for (i in 1: leng) {
		if (class(dataset) == "aquap_data") { 
			a <- makeFlatDataFrame(dataset, simcClasses[i])
			newCorrectGrouping <- a[indNew,1]  # the grouping is in the first column  # otherwise they both stay at "NULL"
			newFlatData <- a[indNew,-1] # the NIR is in everything except the first column
		}
		if (is.null(SimcaModelList[[i]])) {
			predictionsList <- c(predictionsList, list(NULL))		
		} else {
			pred <- makeSimcaPrediction_inner(SimcaModelList[[i]], newFlatData, newCorrectGrouping)
			predictionsList <- c(predictionsList, list(pred))
		}
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
calculateInterclassDistances <- function(modelList, inCV=FALSE) {
	distList <- list()
	leng <- length(modelList)
	if (!.ap2$stn$allSilent & !inCV) {cat(paste(" dist...", sep="") )}
	for (i in 1: leng) {
		if (is.null(modelList[[i]])) {
			distList <- c(distList, list(NULL))
		} else {
			x <- calc_interclassDist(modelList[[i]])
			distList <- c(distList, list(x))	
		}
	} # end for i
	return(distList)
} # EOF

correctSimcaGroupingForDataset <- function(dataset, groupingVector) {
	# we have to find at least 2 groups of data when using each element of the grouping vector on the dataset
	# within these groups, there must be at least the minium amount of spectra as defined int he settings available.
	# those elements of the grouping vector that do not meet these requirements will be excluded
	goodGrps <- NULL
	minSpec <- .ap2$stn$simca_minSpectraEachGroup
	if (minSpec < 3) {
		minSpec <- 3
	}
	for (i in 1: length(groupingVector)) {
		ind <- which(colnames(dataset$header) == groupingVector[i])
		values <- dataset$header[,ind]
		nrLevels <- nlevels(values)
		if (nrLevels > 1) { # so we have at least two groups in the dataset
			# now check for the number of spectra in each single group
			RLE <- rle(as.character(sort(values))) # counts the number of occurence of every unique value
			lengths <- RLE$lengths
	#		print(groupingVector[i]); print(RLE); wait()
			if (min(lengths) >= minSpec) {	
				goodGrps <- c(goodGrps, groupingVector[i])
			}
		}
	} # end for i
	return(goodGrps) # will return NULL if nothing fits the requirements
} # EOF
