#########################  Merge Datasets #######################
# functions to merge datasets together, i.e. kind of "rbind" them. --------

# setClass("aquap_mergeLabels", slots=c(numVec="integer", varNames="character", varTypes="character", values="list", dsNames="character"), contains="data.frame")

genMergeLabels_checkInput <- function(ds1, ds2, varNames, varTypes, values, numVec) {	
	if (length(varTypes) == 1) {
		varTypes <- rep(varTypes, length(varNames))
	}
	if (class(ds1) == "aquap_data" & is.null(ds2)) {
		stop("Please provide a dataset to the argument 'ds2'.", call.=FALSE)
	} # end if
	if (!is.character(varNames) | !is.character(varTypes)) {
		stop("Please provide a character vector to the argument varNames / varTypes", call.=FALSE)
	} # end if
	if (! all(varTypes %in% c("c", "n"))) {
		stop("Please provide only 'c' or 'n' in the character vector at argument 'varTypes'", call.=FALSE)
	}
	if (length(varNames) != length(varTypes)) {
		stop("Please provide character vectors of equal lengths for the arguments 'varNames' and 'varTyes'", call.=FALSE)
	}# end if
	if (! is.null(values) & length(values) != length(varNames)) {
		stop("Please provide a list with the same length as number of new variables to the argument 'values'", call.=FALSE)
	}
	if (!all(sapply(values, length) == length(numVec))) {
		stop("Please make sure that in each list-element of the list provided at the argument 'values' there are same number of elements as there are datasets to merge together", call.=FALSE)
	}
	return(varTypes)
} # EOF

genMergeLabels_checkType <- function(valueList, varTypes, varNames) { # checks if the provided valueList matches the variable types as specified in varTypes (varTypes overrules)
	for (i in 1: length(valueList)) {
		if (varTypes[i] == "c") {
			if (!all(is.character(valueList[[i]]))) {stop(paste0("Please provide only characters in the values list-element #", i, " (values for the variable named '", varNames[i], "')"), call.=FALSE)}
		} else { # so varType[i] must be "n"
			if (!all(is.numeric(valueList[[i]]))) {stop(paste0("Please provide only numerics in the values list-element #", i, " (values for the variable named '", varNames[i], "')"), call.=FALSE)}
		}
	} # end for i	
} # EOF

###### method (documentation below) #######
generateMergeLabels_sys <- function(ds1, ds2=NULL, varNames, varTypes, values=NULL) {
	clpref <- .ap2$stn$p_ClassVarPref
	ypref <- .ap2$stn$p_yVarPref
	#
	if (class(ds1) == "list") {
		numVec <- sapply(ds1, nrow)			### numVec is a numeric vector, giving the number of repeats for each value, the length of numVec is the number of datasets to be merged
		dsList <- ds1
		dsNameVec <- names(dsList)
		if (is.null(dsNameVec)) { # so no named list was provided
			dsNameVec <- paste0("dataset_", 1:length(dsList))
		}
	} else { # so we get in two datasets via ds1 and ds2
		numVec <- c(nrow(ds1), nrow(ds2))
		dsList <- list(ds1, ds2)
#		dsNameVec <- c(deparse(substitute(ds1)), deparse(substitute(ds2))) # get the names of the provided objects ##  does NOT work.
		dsNameVec <- paste0("dataset_", c(1,2))
	} # end else
	#
	varTypes <- genMergeLabels_checkInput(ds1, ds2, varNames, varTypes, values, numVec) # general checking, and here varTypes gets possible recycled i.e. length-adapted
	#
	prefs <- rep(clpref, length(varNames))  # extend variable names with prefixes
	indY <- which(varTypes == "n")
	prefs[indY] <- ypref
	varNames <- paste0(prefs, varNames)
	aa <- as.data.frame(matrix(NA, nrow=length(numVec)+1, ncol=length(varNames)+1)) # each+1 because we want the nr of repeats (rows in the singel ds) displayed in the data frame in the columns, and the type in the rows
	rownames(aa) <- c("Type", dsNameVec)
	colnames(aa) <- c(varNames, "nr_obs")
	aa[1,] <- c(varTypes, "--") # eye candy
	aa[2:nrow(aa), ncol(aa)] <- numVec # write the numeric vector in the last column over all rows except the first one
	aa <- aa[-1,] # was nice for development... but no... 
	valueList <- list(NULL);  length(valueList) <- length(varTypes) # here no values are provided by the user
	if (!is.null(values)) {
		valueList <- values  # is checked above, should be good here
		for (i in 1: (ncol(aa)-1)) { # fill in the display-data frame; -1 because the nr repeats are in the last column
			aa[,i] <- valueList[[i]] # fill in the data frame column-wise !
		} # end for i
		genMergeLabels_checkType(valueList, varTypes, varNames)
	} # end if
	names(valueList) <- varNames
	newLabels <- new("aquap_mergeLabels", aa, numVec=numVec, varNames=varNames, varTypes=varTypes, values=valueList, dsNames=dsNameVec) ## aa is filling the default data frame at @.DAta
	return(newLabels)
} # EOF

merge_checkLabels <- function(mergeLabels) {
	a <- any(sapply(mergeLabels@values, is.null))
	b <- any(sapply(mergeLabels@values, is.na))
	if (a | b) {
		char <- deparse(substitute(mergeLabels))
		stop(paste0("Please provide a complete set of values in the mergeLabel object '", char, "'"), call.=FALSE)
	}
} # EOF

merge_makeNewLabelBlock <- function(mergeLabels) {   # here creating the repeats
	merge_checkLabels(mergeLabels)
	mela <- mergeLabels # to have it shorter
	colNames <- mela@names[1:(length(mela@names)-1)]	
	outdf <- as.data.frame(matrix(NA, nrow=sum(mela@numVec) , ncol=length(mela@values))) # pre-assign the thing
	colnames(outdf) <- colNames
	for (i in 1: ncol(outdf)) {
		outdf[, i] <- rep(mela@values[[i]], mela@numVec) #### CORE #####    # with i, we go through the columns (the list-items in the @values)
	} # end for i
	for (i in 1: ncol(outdf)) {
		if (any(is.character(outdf[,i]))) {
			outdf[,i] <- factor(outdf[,i])
		} # end if
	} # end for i
	return(outdf)	
} # EOF
######
mergeDatasets_two <- function(ds1, ds2, mergeLabels, noMatchH=get(".ap2$stn$gen_merge_noMatchH"), noMatchW=get(".ap2$stn$gen_merge_noMatchW"), dol=get(".ap2$stn$gen_merge_detectOutliers")) { # newLabels can come in as NULL
	dsList <- list(ds1, ds2)
#	names(dsList) <- c(deparse(substitute(ds1)), deparse(substitute(ds2))) # get the names of the provided objects ### does NOT work
	names(dsList) <- paste0("dataset_", c(1,2))
	return(mergeDatasets_list(dsList, mergeLabels, noMatchH, noMatchW, dol))
} # EOF
#####
mergeLabelObj_toList <- function(mergeLabels) {  ## used in the end of mergeDatasets_list() to put the mergeLabel object into the @metadata slot
	char <- "from mergeLabels"
	if (is.null(mergeLabels)) {
		return(list(note="Merged dataset with no merge labels provided"))
	}
	ml <- mergeLabels
#	char <- "These are not the metadata, but the info generated from the mergeLabels"
	out <- list(note=char, numVec=ml@numVec, varNames=ml@varNames, varTypes=ml@varTypes, values=ml@values, dsNames=ml@dsNames)
	return(out)	
} # EOF  ## used in the end of mergeDatasets_list()

merge_checknoMatchChar <- function(noMatchX, pvals, what) {
	#	pv_noMatchHChar <- c("ask", "delete", "rename", "stop")
	if (! all(is.character(noMatchX)) | length(noMatchX) > 1) {
		stop(paste0("Please provide a character length one to the argument 'noMatch", what, "'"), call.=FALSE)
	}
	if (! noMatchX %in% pvals) {
		stop(paste0("Please provide one of '", paste(pvals, collapse="', '"), "' to the argumet 'noMatch", what, "'"), call.=FALSE) 
	}
	return(noMatchX)
} # EOF

merge_readInChoice <- function(xx) {
	maxTries <- 3
	ask <- TRUE
	aa <- 1
	if (xx == 3) {
		txt <- "Please type in either 1, 2, or 3."
	}
	if (xx == 5) {
		txt <- "Please type in either 1, 2, 3, 4 or 5."
	}
	vec <- 1:xx
	#
	checkChoice <- function(choice) {
		if (length(choice) == 0) {
			message(txt)
			return(TRUE)			
		}
		if (! choice %in% vec) {
			message(txt)
			return(TRUE)
		}
		return(FALSE)
	} # EOiF
	##
	while (ask) {
#		print(paste0("we are in try #", aa))
		options(warn=-1)
		choice <- as.numeric(scan(file = "", n = 1, quiet = TRUE, what="integer"))
		ask <- checkChoice(choice)
		options(warn=0)
		aa <- aa+1
		if (aa > maxTries & ask == TRUE) {
			ask <- FALSE
			stop(paste0("Please try again"), call.=FALSE)
		}
	} # end while ask
	return(choice)	
} # EOF

merge_makeMissVisual <- function(headerList, dsNames) {
	charMiss <- "no" # the characters used in the missVisual
	charPresent <- "yes"
	#
	missVisual <- NULL
	aa <- as.vector(unlist(sapply(headerList, colnames)))   	# first collect all column names into a single vector
	cnsTab <- table(aa) # gives a named numeric
	tabMissing <- sort(cnsTab[which(cnsTab < length(headerList))], decreasing = TRUE) # gives back a named numeric
	if (length(tabMissing) != 0) { # so we do have non-matching column names
		namesMissing <- sort(names(tabMissing))  #  gives the names of those columns that are NOT in all datasets
		missVisual <- as.data.frame(matrix(charPresent, nrow=length(headerList), ncol=length(tabMissing)))
		rownames(missVisual) <- dsNames
		colnames(missVisual) <- namesMissing
		for (i in 1: length(headerList)) { # the rows in missVisual
			for (k in 1: length(namesMissing)) { # the columns in missVisual
				haveIt <- namesMissing[k] %in% colnames(headerList[[i]])
				if (!haveIt) {
					missVisual[i,k] <- charMiss
				} # end if
			} # end for k (going through the missing names)
		} # end for i (going through the datasets)
	} # end if  length(tabMissing) != 0
	return(missVisual)
} # EOF

merge_makeMissVisualNIR <- function(wlsList, dsNames) {
	charMiss <- 	"    -- no --    " # the characters used in the missVisual
	charPresent <-	"      yes       "
	rndDelta <- 20
	mdC <- "*" # the character to add when we have more then one deltas in one dataset
	#
	combLists <- function(la, lb) {
		ol <- list(NULL); length(ol) <- length(la)
		for (i in 1: length(la)) {
			ol[[i]] <- c(la[[i]], lb[[i]])
		}
		return(ol)
	} # EOIF
	#
	missVisual <- NULL
	ranges <- lapply(wlsList, range) # gives back a list
	ransUn <- unique(ranges) # gives back a list
	deltas <- lapply(wlsList, function(x) unique(round(diff(x), rndDelta))) # rounding seems to be necessary... somehow the delta can come up with long decimals...
	ranDeltaComb <- combLists(ranges, deltas)
	ranDeltaUni <- unique(ranDeltaComb) # now has the ranges combined with the deltas. The length of that is the numbers of different datasets that we have
	#
	singleDeltaChar <- rep("", length(deltas))
	singleDeltaNum <- numeric(length(deltas))
	for (i in 1: length(deltas)) { # prepare a character vector telling us whether we have more than one delta wls
		if (length(deltas[[i]]) == 1) {
			singleDeltaNum[i] <- deltas[[i]]
		} else {
			singleDeltaNum[i] <- as.numeric(names(sort(table(deltas[[i]]), TRUE))[1]) # gives back the delta that comes up most often
			singleDeltaChar[i] <- mdC
		} # end else
	} # end for i	
	print(singleDeltaChar); print(singleDeltaNum)
	#
	missVisual <- as.data.frame(matrix(charMiss, nrow=length(wlsList), ncol=length(ranDeltaUni)))
	for (i in 1: length(ranDeltaUni)) { # go through each present range & delta (the columns)
		for (k in 1: length(wlsList)) { # go through each dataset (the rows)
			if (identical(ranDeltaUni[[i]], ranDeltaComb[[k]])) {
				missVisual[k,i] <- charPresent
			} # end if
		} # end for k (the datasets)
	} # end for i (the ranges)
	rownames(missVisual) <- dsNames
	cns <- sapply(ranDeltaUni, function(x) paste0(x[1], " - ", x[2], ", ∆=", x[3]))
	cns <- paste0("   ", cns,"   ")
	colnames(missVisual) <- cns
	newO <- order(sapply(apply(missVisual,2, function(x) which(grepl("yes",x))), length), decreasing = TRUE) # re-order the miss-visual, most participants on the left
	missVisual <- missVisual[,newO]
	return(missVisual)
} # EOF

merge_checkListInput <- function(dsList) {
	if (class(dsList) == "aquap_data") {
		stop(paste0("Wrong or missing input. Please refer to the manual (?mergeDatasets) for possible options."), call.=FALSE)
	} # end if
} # EOF

merge_checkLabelObject <- function(dsList, mergeLabels) {
	if (!is.null(mergeLabels)) {
		aa <- length(dsList)
		bb <- length(mergeLabels@dsNames)
		if (aa != bb) {
			stop(paste0("The number of datasets to be merged (", aa, " datasets) does not match the parameters provided in the 'mergeLabel' object (", bb, " datasets) ."), call.=FALSE)
		} # end if length
	} # end if !is.null mergeLabels	
} # EOF

merge_labelObjectToSlot <- function(dsList, mergeLabels) { 
	if (is.null(mergeLabels)) {
		numVec <- unlist(lapply(dsList, nrow))
		dsNames <- names(dsList)
		vn <- ""
		vt <- ""
		return(new("aquap_mergeLabels", numVec=numVec, varNames=vn, varTypes=vt, values=NULL, dsNames=dsNames))
	} else {
		return(mergeLabels)
	}
} # EOF
# setClass("aquap_mergeLabels", slots=c(numVec="integer", varNames="character", varTypes="character", values="list", dsNames="character"), contains="data.frame")

merge_checkWlsActual <- function(wlsList, newMasterWls, dsNames, what) {
	for (i in 1: length(wlsList)) {
		if (what == "fill") {
			if (!all(wlsList[[i]] %in% newMasterWls)) {
					stop(paste0("Sorry, without resampling the wavelengths from dataset '", dsNames[i], "' can not be merged."), call.=FALSE)
			} # end if		
		} # end if fill
		#
		if (what == "cut") {
			if (!all(newMasterWls %in% wlsList[[i]])) {
					stop(paste0("Sorry, without resampling the wavelengths from dataset '", dsNames[i], "' can not be merged."), call.=FALSE)
			} # end if		
		} # end if fill		
	} # end for i
	return(invisible(NULL))
} # EOF

merge_checkWlsDeltas <- function(wlsList) {
	deltas <- lapply(wlsList, function(x) unique(diff(x)))
	deltaUn <- unique(unlist(deltas))
	if (length(deltaUn) != 1) {
		stop(paste0("Not all datasets have the same delta wavelength. Please consider resampling the datasets by providing '", pv_noMatchWChar[4], "' or '", pv_noMatchWChar[5], "' to the argument 'noMatchW'"), call.=FALSE)
	} # end if 
	return(deltaUn)
} # EOF

merge_cutNirOutsiders <- function(dsList, wlsList, dsNames) {
	# check for equal delta
	deltaWls <- merge_checkWlsDeltas(wlsList)
	#
	rans <- lapply(wlsList, range)
	newLower <- max(sapply(rans, function(x) x[1])) # get the new lower limit for all wavelengths
	newUpper <- min(sapply(rans, function(x) x[2])) # get the new upper limit for all wavelengths
	#
	newMasterWls <- seq(newLower, newUpper, by=deltaWls) # just for checking
	merge_checkWlsActual(wlsList, newMasterWls, dsNames, "cut")
	#
	new_dsList <- lapply(dsList, function(x, lower, upper) selectWls(x, lower, upper), lower=newLower, upper=newUpper) # haha
	# now collect back
	nirList <- lapply(new_dsList, getNIR)
	wlsList <- lapply(new_dsList, getWavelengths)
	return(list(nirList=nirList, wlsList=wlsList, newRange=c(newLower, newUpper)))
} # EOF

merge_fillInNirOutsiders <- function(nirList, wlsList, dsNames) {
	# check for equal delta
	deltaWls <- merge_checkWlsDeltas(wlsList)
	#
	rans <- lapply(wlsList, range)
	newLower <- min(sapply(rans, function(x) x[1])) # get the new lower limit for all wavelengths
	newUpper <- max(sapply(rans, function(x) x[2])) # get the new upper limit for all wavelengths
	newRange <- c(newLower, newUpper)
	# find out the ncpwl of the first dataset (yes, we could bring it. But we can check as well.)
	wlW <- unlist(strsplit(	colnames(nirList[[1]])[1], "")) # the wavelength with the character
	wlWo <- unlist(strsplit(as.character(wlsList[[1]][1]), ""))
	cpwl <- setdiff(wlW, wlWo) # the character(s) before the wavelength
	# now we have to operate on the wlsList, as the ncpwl in the other wavelengths could be different (would be stupid - but it is possible)
	newMaWls <- seq(newLower, newUpper, by=deltaWls) # the new master wavelength
	#
	merge_checkWlsActual(wlsList, newMaWls, dsNames, "fill")
	#
	for (i in 1: length(nirList)) { # going through the nirList
		if (!identical(range(wlsList[[i]]), newRange)) {
			thisNewNir <- as.data.frame(matrix(NA, nrow=nrow(nirList[[i]]), ncol=length(newMaWls)))
			colnames(thisNewNir) <- paste0(cpwl, newMaWls)
			rownames(thisNewNir) <- rownames(nirList[[i]])
			inds <- which(newMaWls %in% wlsList[[i]])
			thisNewNir[,inds] <- nirList[[i]][,]
			nirList[[i]] <- thisNewNir 
			wlsList[[i]] <- newMaWls
		} # end if not identical
	} # end for i (nirList)
	return(list(nirList=nirList, wlsList=wlsList, newRange=newRange, cpwl=cpwl))
} # EOF

###### CORE #########
mergeDatasets_list <- function(dsList, mergeLabels, noMatchH=get(".ap2$stn$gen_merge_noMatchH"), noMatchW=get(".ap2$stn$gen_merge_noMatchW"), dol=get(".ap2$stn$gen_merge_detectOutliers")) { # newLabels can come in as NULL   ####CORE####
	autoUpS()
	stn <- get("stn", envir=.ap2)
	clpref <- stn$p_ClassVarPref
	ypref <- stn$p_yVarPref
	olc <- stn$p_outlierCol
	thisSilent <- stn$allSilent
	snrColName_y <- paste0(ypref, stn$p_sampleNrCol)
	snrColName_c <- paste0(clpref, stn$p_sampleNrCol)
	#
	# intern functions 
	checkForSameness <- function(xList, char=NULL) {
		add <- ""
		cns1 <- colnames(xList[[1]])
		for (i in 2: length(xList)) {
			cnsi <- colnames(xList[[i]])
			if (!identical(cns1, cnsi)) { # compares the first one with all the others
				if (char == "header") {
					apoChar <- "\nWe are terribly sorry, but apparently there was an error while deleting / filling in header columns.\n"
				} # end if
				if (char == "wavelength") {
					apoChar <- "\nWe are terribly sorry, but apparently there was an error while resampling / cutting / filling in wavelengths.\n"
				} # end if
				stop(paste0(apoChar), call.=FALSE)
				return(FALSE)
			} # end if
		} # end for i
	} # EOIF
	getRLEs_num <- function(x, cn=snrColName_y) {
		return(rle(as.numeric(x$header[,cn])))
	} # EOIF
	printTxtMissVisual <- function(thisSilent, weComeFromAsking, txtGen, missVisualNIR, txt) {
		if (!thisSilent) {
			if (!weComeFromAsking) {
				message(txtGen)
				print(missVisualNIR); cat("\n")			
			} # end if !weComeFromAsking
			cat(txt)
		} # end if not silent
	} # EOIF
	#
	#
	#### entry-checking ######
	merge_checkListInput(dsList)
	noMatchH <- merge_checknoMatchChar(noMatchH, pv_noMatchHChar, "H") # check if input is valid
	noMatchW <- merge_checknoMatchChar(noMatchW, pv_noMatchWChar, "W") # check if input is valid
	merge_checkLabelObject(dsList, mergeLabels)
	#
	#########  make the newLabel columns ################
	newLabelBlock <- NULL
	numVec <- sapply(dsList, nrow)
	dsNames <- names(dsList)
	if (is.null(dsNames)) {
		dsNames <- paste0("dataset#", 1:length(dsList))
	} # end if
	if (!is.null(mergeLabels)) {
		newLabelBlock <- merge_makeNewLabelBlock(mergeLabels)
		numVec <- mergeLabels@numVec # not really necessary
		dsNames <- mergeLabels@dsNames
	} # end if
	##
	##### collect header and NIR together #########
	headerList <- lapply(dsList, getHeader)
	nirList <- lapply(dsList, getNIR)
	wlsList <- lapply(dsList, getWavelengths)
	rleList_sn <- lapply(dsList, getRLEs_num) # the default is to get the rle for the sample numbers
	##
	
	######### check for duplicates within a singel dataset ########
	for (i in 1: length(headerList)) {  # check for duplicate colnames within each single dataset
		cns <- c(colnames(headerList[[i]]), colnames(newLabelBlock)) # within a single dataset + the new names
		ind <- which(duplicated(cns))
		if (length(ind) != 0) { # so we have a duplicate
			stop(paste0("There are duplicate column names ('", paste0(cns[ind], collapse=", "), "') in dataset #", i, " ('", dsNames[i], "').\n(Possibly coming in via the 'mergeLabel' object.)"), call.=FALSE)
		} # end if
	} # end for i
	##
			
	############## check for non-matches in header columns, handle them ################
	aa <- as.vector(unlist(sapply(headerList, colnames)))   	# first collect all column names into a single vector
	cnsTab <- table(aa) # gives a named numeric
	namesComplete <- names(which(cnsTab == length(headerList))) # gives the names of those columns that are present in all datasets
	tabMissing <- sort(cnsTab[which(cnsTab < length(headerList))], decreasing = TRUE) # gives back a named numeric
	if (length(tabMissing) != 0) { # so we have a non-match
		namesMissing <- names(tabMissing)  #  gives the names of those columns that are NOT in all datasets
		missVisual <- merge_makeMissVisual(headerList, dsNames) 
		txtGen <- paste0("\nThere were ", length(namesMissing), " non-matching header columns detected.\n")
		#		
		if (noMatchH == pv_noMatchHChar[1]) { # "ask"   # yes, I know.  I tried a "switch". Could not make it work. Shame.
			message(txtGen)
			print(missVisual)
			cat(paste0("\nShould the non-matching header columns be:\n1 - deleted,\n2 - filled in, or\n3 - should the merging of datasets be stopped?\n"))
			choice <- merge_readInChoice(xx=3)
			if (choice == 1) {
				if (!thisSilent) {
					cat(paste0("Deleting header-columns... \n"))
				} # end if
				noMatchH <- pv_noMatchHChar[2] ## so we go into the delete below
				thisSilent <- TRUE
			} # end if
			if (choice == 2) {
				if (!thisSilent) {
					cat(paste0("Filling in header-columns... \n"))
				} # end if
				noMatchH <- pv_noMatchHChar[3] ## so we go into the filling in below
				thisSilent <- TRUE
			} # end if
			if (choice == 3) {
				noMatchH <- pv_noMatchHChar[4] ## so we go into the stop below
				thisSilent <- TRUE
			} # end if
		} # end "ask"
		##
		if (noMatchH == pv_noMatchHChar[2]) { # "delete"
			if (!thisSilent) {
				message(txtGen)
				cat(paste0("From all datasets to be merged (in the rows below), the following header-columns will be deleted in (in the columns below):\n\n"))
				print(missVisual); cat("\n")
			} # end if not silent
			for (i in 1: length(headerList)) {
#				print(colnames(headerList[[i]]))
				for (k in 1: length(namesMissing)) {
#					cat("\n"); print(namesMissing[k])
					haveIt <- namesMissing[k] %in% colnames(headerList[[i]]) # comes back as single logical
#					cat(paste0("We have it? ", haveIt, "\n"))
					if (haveIt) {
						ind <- which(colnames(headerList[[i]]) == namesMissing[k])
#						cat(paste0("The index is: ", ind, "\n"))
						headerList[[i]] <- headerList[[i]][,-ind] # only works via numbers
					} # end if not haveIt
#					wait()
				} # end for k
				headerList[[i]] <- headerList[[i]][,namesComplete] # re-order the header
			} # end for i
		} # end "delete"
		##
		if (noMatchH == pv_noMatchHChar[3]) { # "fill in"
			if (!thisSilent) {
				message(txtGen)
				cat(paste0("For all datasets to be merged (in the rows below), the following header-columns will be filled in (in the columns below):\n\n"))
				print(missVisual); cat("\n")		
			} # end if this silent
			# now rename them
			for (i in 1: length(headerList)) {
				cns <- colnames(headerList[[i]])
				for (k in 1: length(namesMissing)) {
					haveIt <- namesMissing[k] %in% colnames(headerList[[i]]) # comes back as single logical
					if (!haveIt) { # so we do not have a column with the name "namesMissing[k]" in the dataset
						rnrs <- nrow(headerList[[i]])
						newFrame <- as.data.frame(matrix(NA, nrow=rnrs, ncol=1))
						colnames(newFrame) <- namesMissing[k]
						headerList[[i]] <- cbind(headerList[[i]], newFrame) # cbind the new frame to the old header in the headerList
					} # end if not haveIt
				} # end for k (going through the missing colnames)
				newOrder <- c(namesComplete, namesMissing)
				headerList[[i]] <- headerList[[i]][,newOrder] # re-order the header in each dataset by "colname-indexing"
			} # end for i (going through the header list)		
		} # end "filling in"
		##
		if (noMatchH == pv_noMatchHChar[4]) { # "stop"
			if (!thisSilent) {
				message(txtGen)
				print(missVisual); cat("\n")
			} # end if not silent
			stop(paste0("Merging of datasets is stopped. See ?mergeDatasets for options."), call.=FALSE)
		} # end "stop"			
		##
	} else { # se we do NOT have a non-match, i.e. all the columns are overlapping
		for (i in 1: length(headerList)) {
			headerList[[i]] <- headerList[[i]][,namesComplete] # re-order the header ######## important !! --> as before, we only checked for the "presence" of the column name, the order was irrelevant. Now everything has to have same order.
		} # end for i	
	} ######### end if (we have a non-match #########
	##		
	checkForSameness(headerList, "header")   ## after the corrections above, this never should give a no-match. Leave it here just to check.
	##
	##
	########## check for not-same wavelengths, deal with them #############  
	cpwl <- NULL # might be modified later
	nirSame <- sapply(wlsList, function(x, aa) identical(x, aa), aa=wlsList[[1]])
	allNirSame <- all(nirSame)
	if (!allNirSame) {
		ransUn <- unique(lapply(wlsList, range)) # gives back a list
		nrs <- length(ransUn)
		txtGen <- paste0("There were ", nrs, " sets of wavelengths detected in the datasets to be merged.\n")
		cutText <- "The wavelengths in all datasets to be merged (in the rows) were cut to the following common range:"
		fillText <- "The wavelengths in all datasets to be merged (in the rows) were filled in to the following maximum range:"
		cutResaAdd <- "wavelengths resampled  yes no ??"
		fillResaAdd <- "wavelengths resampled  yes no ??"
		missVisualNIR <- merge_makeMissVisualNIR(wlsList, dsNames)
		weComeFromAsking <- FALSE
		if (noMatchW == pv_noMatchWChar[1]) { # so we want to ask
			message(txtGen)
			print(missVisualNIR)
			cat(paste0("\nShould the non-matching wavelengths be:\n1 - cut: outsides will be cut off,\n2 - filled in: outsides will be filled in with NAs, \n3 - cut off (outsides) and (possibly) resampled, \n4 - filled in (outsides) and (possibly) resampled, or \n5 - should the merging of datasets be stopped?\n"))
			choice <- merge_readInChoice(xx=5)
			if (choice == 1) { # cut away wavelengthsf
				noMatchW <- pv_noMatchWChar[2] ## so we go into the cutting outsiders below
				weComeFromAsking <- TRUE
			} # end if
			if (choice == 2) { # fill in
				noMatchW <- pv_noMatchWChar[3] ## so we go into the filling in below
				weComeFromAsking <- TRUE
			} # end if
			if (choice == 3) {			
				noMatchW <- pv_noMatchWChar[4] ## so we go into cutting and resampling below
				weComeFromAsking <- TRUE
			} # end if
			if (choice == 4) {			
				noMatchW <- pv_noMatchWChar[5] ## so we go into filling in and resampling below
				weComeFromAsking <- TRUE
			} # end if
			if (choice == 5) {
				noMatchW <- pv_noMatchWChar[6] ## so we go into the stop below
				weComeFromAsking <- TRUE
			} # end if						
		} # end if "ask"
		##
		# pv_noMatchWChar <- c("ask", "cut", "fill", "cutresa", "fillresa", "stop")		
		if (noMatchW == pv_noMatchWChar[2]) { # "cut"
			aaa <- merge_cutNirOutsiders(dsList, wlsList, dsNames)
				nirList <- aaa$nirList
				wlsList <- aaa$wlsList
				newRange <- aaa$newRange
			printTxtMissVisual(thisSilent, weComeFromAsking, txtGen, missVisualNIR, txt=paste0(cutText, "\n", paste0(newRange, collapse=" - "), "\n\n"))		
		} # end "cut"	
		##	
		if (noMatchW == pv_noMatchWChar[3]) { # "fill"
			aaa <- merge_fillInNirOutsiders(nirList, wlsList, dsNames)
				nirList <- aaa$nirList
				wlsList <- aaa$wlsList
				newRange <- aaa$newRange
				cpwl <- aaa$cpwl
				#
			printTxtMissVisual(thisSilent, weComeFromAsking, txtGen, missVisualNIR, txt=paste0(fillText, "\n", paste0(newRange, collapse=" - "), "\n\n"))		
		} # end "fill"	
		##	
		if (noMatchW == pv_noMatchWChar[4]) { # "cutresa"
			# do it
			
			printTxtMissVisual(thisSilent, weComeFromAsking, txtGen, missVisualNIR, txt=paste0(cutText, "\n", paste0(newRange, collapse=" - "), "\n", cutResaAdd, "\n\n"))
		} # end "cutresa"	
		##
		if (noMatchW == pv_noMatchWChar[5]) { # "fillresa"
			# do it
				
			printTxtMissVisual(thisSilent, weComeFromAsking, txtGen, missVisualNIR, txt=paste0(cutText, "\n", paste0(newRange, collapse=" - "), "\n", fillResaAdd, "\n\n"))
		} # end "fillresa"	
		##						
		if (noMatchW == pv_noMatchWChar[6]) { # "stop"
			printTxtMissVisual(thisSilent, weComeFromAsking, txtGen, missVisualNIR, txt=NULL)
			stop(paste0("Merging of datasets is stopped. See ?mergeDatasets for options."), call.=FALSE)
		} # end "stop"	
		##		
	} # end if !allNirSame
	#
	checkForSameness(nirList, "wavelength")  #### after the corrections above, this never should give a no-match. Leave it here just to check.
	##
	##
	##
	
	########## prepare special columns (Y_SampleNr) ##########
	# prepare for the modified Y_SampleNr
	totalNrsNeeded <- sum(unlist(lapply(rleList_sn, function(x) length(x$lengths)))) # the sum of how many numbers in each dataset occuring
	runningHowMany <- unlist(lapply(rleList_sn, function(x) x$lengths)) # a vector, the running number saying how many of the same values (ranging from 1:totalNrsNeeded)  are needed
	newSNrVec <- numeric(0)
	for (i in 1: totalNrsNeeded) { # length(runningHowMany) would be the same
		newSNrVec <- c(newSNrVec, rep(i, runningHowMany[i])) # will be pasted into the ready-made fused header below
	} # end for i
	##
	
	######## prepare for and fuse the datasets together, also take care of special columns (Y_SampleNr) #########
	header <- as.data.frame(matrix(NA, nrow=sum(numVec) , ncol=ncol(headerList[[1]]) )) # just take the first one, by now all columns should be the same
	NIR <- as.data.frame(matrix(NA, nrow=sum(numVec) , ncol=ncol(nirList[[1]]) )) # at the moment, all NIR MUST be the same wavelength
	colRep <- as.data.frame(matrix(NA, nrow=sum(numVec) , ncol=2 )) # will be re-colored anyway
	riVec <- c(0, cumsum(numVec))+1
	if (!stn$allSilent) {
		cat(paste0("Merging ", length(dsList), " datasets... "))
	}
	for (k in 1: (length(riVec)-1)) {
		addChar <- ", "
		if (k == length(headerList)) {addChar <- ". "}
		cat(paste0(k, addChar))
		ris <- riVec[k]
		rie <- riVec[k+1]-1
		for (i in 1: ncol(header)) {
			aa <- headerList[[k]][,i]
			if (is.factor(aa)) {
				aa <- as.character(aa)	# otherwise we destroy the numeric-information
			}
			header[ris:rie,i] <- aa  # colwise			### CORE ###
			NIR[ris:rie,] <- nirList[[k]]				### CORE ###
		} # end for i
	} # end for k #### CORE fuse together ######
	newHeaderColnames <- colnames(headerList[[1]])
	if (!is.null(newLabelBlock)) {
		header <- cbind(header, newLabelBlock)	
		newHeaderColnames <- c(colnames(headerList[[1]]), colnames(newLabelBlock)) # take from #1 as now all header names should be the same
	} # end if
	colnames(header) <- newHeaderColnames
	# replace some values 
	header[,which(colnames(header) == snrColName_y)] <- newSNrVec	  	# paste in the new values for the sample number
	header[,which(colnames(header) == snrColName_c)] <- as.character(newSNrVec)	  	# paste in the new values for the sample number as character
	#
	ind <- which(colnames(header) == paste0(clpref, olc, "_all"))
	if (length(ind) != 0) {
		colnames(header)[ind] <- paste0(clpref, olc, "_single_all")	
	} # end if
	colnames(NIR) <- colnames(nirList[[1]])
	rownames(header) <- paste0("r", 1:nrow(header))
	rownames(NIR) <- paste0("r", 1:nrow(NIR))
	NIR <- as.matrix(NIR)
	#

	###### possibly detect outliers ###########
	if (dol) {
		cat("ok.\n") # denoting the end of merging the dataset
		outliers <- flagOutliers_allScope(NIR, detectOutliers=TRUE) # comes back as a very nice 1-column data frame
		header <- cbind(header, outliers)
	} # end if
	#
	
	####### put all together, make nice #########
	fd <- data.frame(I(header), I(colRep), I(NIR))
	names(fd) <- names(dsList[[1]])
	fd <- new("aquap_data", fd)
	rownames(fd) <- rownames(fd$colRep) <- paste0("r", 1:nrow(fd))
	for (i in 1:ncol(fd$header)) {
       if (any(is.character(fd$header[, i]))) {
           fd$header[i] <- factor(fd$header[, i])
       } # end if
    } # end for i
    fd <- reColor(fd)
    fd@metadata <- getmd()
    fd@ncpwl <- getNcpwl(dsList[[1]]) # just take the first one
    fd@mergeInfo <- merge_labelObjectToSlot(dsList, mergeLabels)
    fd@version <- pv_versionDataset
    if (!stn$allSilent & !dol) {
		cat(paste0("ok. \n"))
	}
	return(fd)
} # EOF
#########################  End Merge Datasets #######################



########################
combine_checkEntry <- function(dataset, sourceVars, name, sep) {
	checkForCharacter <- function(char, argName, len=1) {
		if (!all(is.character(char)) | length(char) != len) {
			stop(paste0("Please provide a character length ", len, " in the argument '", argName, "'"), call.=FALSE)
		} 
	} # EOIF
	##
	if (class(dataset) != "aquap_data") {
		stop("Please provide an object of class 'aquap_data' in the argument 'dataset'", call.=FALSE)
	} # end if
	if (is.null(sourceVars)) {
		stop("You must provide two or more valid variable / column names in the argument 'sourceVars'", call.=FALSE)
	} # end if
	if (is.null(name)) {
		stop("You must provide a name for the new variable in the argument 'name'", call.=FALSE)
	} # end if
	checkForCharacter(name, "name", len=1)
	checkForCharacter(sep, "sep", len=1)
	# check the source vars
	if (!all(is.character(sourceVars))) {
		stop("Please provide only characters in the argument 'sourceVars'", call.=FALSE)
	} # end if
	cns <- colnames(getHeader(dataset))
	ind <- which(!sourceVars %in% cns) # check for existence of source vars
	if (length(ind) != 0) { # so we have source vars that are NOT present in the header
		charMiss <- sourceVars[ind]
		if (length(ind)>1) {add <- "s"; w <- "are"} else {add <- ""; w <- "is"}
		stop(paste0("The source-variable", add, " '", paste(charMiss, collapse=", "), "' ", w, " not present in the provided dataset"), call.=FALSE)
	} # end if
	

} # EOF

#' @title Combine Variable
#' @description Combine values from several variables into a single value in 
#' a mew variable.
#' @details The resulting new variable can only be a class-variable. 
#' @param dataset The standard dataset as produced by \code{\link{gfd}}.
#' @param sourceVars Character length two or more. The name of the variables where 
#' values should be copied from.
#' @param name Character length one. The name of the new variable.
#' @param sep Character length one. A character to separate the individual 
#' values in the new variable. Set to '""' (empty string) to have no separation 
#' between values.
#' @return An object of class 'aquap_data' with a new variable in the header.
#' @examples
#' \dontrun{
#' fd <- gfd() # load a dataset
#' fdNew <- combineVariable(fd, c("C_Foo", "C_Bar"), "FooBar")
#' fdNew <- combineVariable(fd, c("C_Foo", "C_Bar"), "FooBar", sep="*")
#' 	# is combining the values of the two variables 'C_Foo' and 'C_Bar' into a new 
#'  # variable called 'C_FooBar'.
#' }
#' @seealso calculateVariable
#' @family dataset modification functions
#' @export
combineVariable <- function(dataset, sourceVars=NULL, name=NULL, sep="_") {
	autoUpS()
	stn <- get("stn", envir=.ap2)
	clpref <- stn$p_ClassVarPref
	#
	combine_checkEntry(dataset, sourceVars, name, sep) # checks all input, makes sure that all provided source variables are present in the dataset
	#
	header <- getHeader(dataset)
	sourceHeader <- header[,sourceVars]
	newDF <- data.frame(factor(apply(sourceHeader, 1, function(x) paste(x, collapse=sep))))
	colnames(newDF) <- paste0(clpref, name)
	header <- cbind(header, newDF)
	dataset$header <- I(header)
	dataset <- reColor(dataset)
	return(dataset)
} # EOF

calculate_checkEntry <- function(dataset, cexpr, name, type) {
	checkForCharacter <- function(char, argName, len=1) {
		if (!all(is.character(char)) | length(char) != len) {
			stop(paste0("Please provide a character length ", len, " in the argument '", argName, "'"), call.=FALSE)
		} 
	} # EOIF
	##
	if (class(dataset) != "aquap_data") {
		stop("Please provide an object of class 'aquap_data' in the argument 'dataset'", call.=FALSE)
	} # end if
	if (is.null(name)) {
		stop("You must provide a name for the new variable in the argument 'name'", call.=FALSE)
	} # end if
	if (!is.expression(cexpr)) {
		stop("You must provide an expression describing how the values for the new variable should be calculated in the argument 'cexpr'", call.=FALSE)
	} # end if	
	if (deparse(cexpr) == "expression(\"\")") {
		stop("Please provide an expression describing how the values for the new variable should be calculated in the argument 'cexpr'", call.=FALSE)
	} # end if
	checkForCharacter(name, "name", len=1)
	checkForCharacter(type, "type", len=1)
	if (!type %in% c("c", "n")) {
		stop("Please provide either 'c' or 'n' in the argument 'type'", call.=FALSE)
	} # end if
	
} # EOF

calculate_postChecking <- function(header, newDF, name, type) {
	stn <- get("stn", envir=.ap2)
	allSilent <- stn$allSilent
	#
	if (type == "n") {
		if (!all(is.numeric(newDF[,1]))) {
			stop(paste0("You want the the new variable '", name, "' to be numeric, but not all calculated values are numeric"), call.=FALSE)
		} # end if
	} # end if type == "n"
	if (type =="c") {
		# do nothign
	} # end if
	if (any(is.na(newDF[,1]))) {
		ind <- which(is.na(newDF[,1]))
		char <- "\nSome values calculated to 'NA'\n\n"
		if (!allSilent) {
			message(char)
			print(header[ind,])
		} # end if	
	} # end if
	
} # EOF

#' @title Calculate Variable
#' @description Calculate values for a new variable.
#' @param dataset The standard dataset as produced by \code{\link{gfd}}.
#' @param cexpr An arbitrary R expression describing how to calculate the 
#' values for the new variable. The expression will be evaluated using 'with', 
#' with the local environment being a single row of the provided dataset.
#' @param name Character length one. The name of the new variable.
#' @param type Character length one. The type of the new variable. Possible values 
#' are 'c' for class-variables and 'n' for numeric variables. Defaults to '"c"'.
#' @details Similarly as \code{\link{combineVariable}} is simply combining , i.e. 
#' pasting together all the values of the selected variables from a single 
#' observation (within one row), \code{calculateVariable} is using variables of 
#' one observation (within one row) to calculate a new value according to the 
#' expression provided in the argument 'cexpr'. The expression can contain logical 
#' clauses etc, and it pertains exclusively to the values within a single row. 
#' Typically, the expression could contain an if clause followed by two values,
#' See examples.
#' New variables of type 'numeric' can obviously only be calculated from numerical 
#' variables, but the numeric result of such a calculation can be used as class 
#' variable by setting the type of the new variable to 'class' voa providing 'c' 
#' to the argument 'type'.
#' The name of the new variable and the expression used to calculate it is stored 
#' in a list in the slot named 'calcVarInfo'. (\code{object@@calcVarInfo})
#' @return An object of class 'aquap_data' with a new variable in the header.
#' @examples
#' \dontrun{
#' fd <- gfd() # load a dataset
#' cexpr <- expression(Y_Foo + Y_Bar)
#' newFd <- calculateVariable(fd, cexpr, name="Addition")
#' # more examples of how to formulate the expression:
#' cexpr <- expression((Y_Foo * Y_Bar)/Y_Fuba)
#' cexpr <- expression(if (Y_Foo < 5 & C_Bar == "blabla") "Outcome1" else "Outcome2")
#' cexpr <- expression(if (C_Bar == "blabla" | C_Foo == "blibli") "Outcome1" else "Outcome2")
#' cexpr <- expression(as.numeric(paste0(Y_Foo, C_Bar))/Y_Foobar) # of course crazy, but
#' # it demonstrates that everything is possible in the expression. 
#' #
#' # use a numerical outcome as class variable:
#' newFd <- calculateVariable(fd, expression(Y_Foo - Y_Bar), name="Subtraction", type="c") 
#' newFd <- calculateVariable(fd, expression(Y_Foo - Y_Bar), name="Subtraction", type="n") 
#' # in that way you can color by the result of the calculation, and you 
#' # can also add the same variable as numerical variable
#' }
#' @family dataset modification functions
#' @seealso combineVariable
#' @export
calculateVariable <- function(dataset, cexpr=expression(""), name=NULL, type="c") {
	autoUpS()
	stn <- get("stn", envir=.ap2)
	clpref <- stn$p_ClassVarPref
	ypref <- stn$p_yVarPref
	#
	calculate_checkEntry(dataset, cexpr, name, type) # checks the input
	#
	if (type == "c") {
		newColname <- paste0(clpref, name)
	} else {
		newColname <- paste0(ypref, name)
	}
	header <- getHeader(dataset)
	if (newColname %in% colnames(header)) { # check for double column names
		stop(paste0("The column name '", newColname, "' already exists in the provided dataset '", deparse(substitute(dataset)), "'."), call.=FALSE)
	} # end if
	#
	newDF <- as.data.frame(matrix(NA, nrow=nrow(header), ncol=1))
	colnames(newDF) <- newColname
	for (i in 1: nrow(header)) {   # I know. Something with "apply" or so would be way more elegant. Could not quickly find how to do that. Sorry. :-) 
		aa <- 	try(with(header[i,], eval(cexpr)), silent=TRUE)   ###### CORE ######
		if (class(aa) != "try-error") { # so all went well
			newDF[i,1] <- aa
			# check for warnigns. No. Maybe an other time. It is late now.
		} else { # so we have an error
			msgChar <- paste0("Sorry, there was an error while applying the expression \n'", paste0(enquote(cexpr), collapse="("), ")'.\n")
			message(msgChar)
			errMsg <- trimws(strsplit(aa[[1]], ":")[[1]][2])
			cat(paste0("Error text: \n"))
			message(paste0(errMsg, "\n\n"))
			cat(paste0("The error occurred at row #", i, " in the provided dataset '", deparse(substitute(dataset)), "'.\n\n"))
			print(header[i,])
			stop(call.=FALSE)
		} # end else
	} # end for i
	if (type == "c") {
		newDF[,1] <- factor(newDF[,1]) # turn characters into factors
	} # end if
	header <- cbind(header, newDF)
	calculate_postChecking(header, newDF, newColname, type)
	dataset$header <- I(header)
	dataset <- reColor(dataset)
	dataset@calcVarInfo <- c(dataset@calcVarInfo, list(list(name=newColname, cexpr=cexpr)))  # make new CalcInfo List
	return(dataset)	
} # EOF



###############################################################################

#' @title Generate Merge Labels
#' @description Generate an object of class 'aquap_mergeLabels' that can be used 
#' to specify the names and values of new variables in the merged dataset.
#' @details Provide either two datasets via the arguments ds1 and ds2, or a list
#' containing any number of datasets at the argument 'ds1'. In the latter case, 
#' argument ds2 is ignored. The lengths of the vectors in argument 'varNames' and 
#' 'varTypes' have to be the same. Providing a named list at argument 'ds1' leads 
#' to these names being displayed as row names in the displayed data frame when 
#' inspecting the resulting objet of class 'aquap_mergeLabels', potentially making 
#' it easier to assign the values. If the argument 'values' is left at its default 
#' NULL, an empty data frame displaying the new variables and possibly the names 
#' of the datasets to merge is displayed when inspecting the generated object. 
#' The values for each column of this dataframe now can be filled in via standard 
#' dataframe subselection. 
#' It can be easier to first generate the 'aquap_mergeLabels' object without 
#' providing the values, and then, when one can look at the displayed data frame, 
#' to fill in the values column by column via standard dataframe subselection. 
#' At any checking process (done automatically) the definitions given in argument 
#' 'varTypes' are decisive. The last column in the data frame shows the number of 
#' observations in each dataset, and it can not be changed manually. It is advised 
#' to change the valus exclusively via subsetting the object ("\code{'[,j]<-'} or 
#' \code{'object$name <-'}). It is advised to name the arguments. 
#' @note Naming the input-arguments is advised. 
#' @param ds1 An object of class 'aquap_data' or a (possibly named) list containing 
#' any number of objects of class 'aquap_data'
#' @param ds2 An object of class 'aquap_data'. Is ignored when a list is provided 
#' at the argument 'ds1'.
#' @param varNames Character or character vector. The names of the new variables in 
#' the merged dataset.
#' @param varTypes Character or character vector, denoting if a new variable is either 
#' a class variable or a numerical variable. Two values are possible, 'c' for class 
#' variables and 'n' for numerical variables. Provide either a vector the same length
#' as argument 'varNames', or a vector with length one that gets recycled over the 
#' length of 'varNames'.
#' @param values A list with the same length as there are new variables, containing 
#' the values of the new variables for each dataset to be merged. Can be left at 
#' NULL, see details. If a list is provided every element within this list must be the 
#' same length as there are numbers of datasets to merge (as for every single dataset 
#' for every variable one value must be provided).
#' @examples
#' \dontrun{
#'	fd1 <- gfd()
#'	fd2 <- fd1[1:20] # just create a second dataset
#'	labels <- generateMergeLabels(fd1, fd2, c("foo", "bar", "foobar"), c("c", "n", "c")) 
#'  labels[,2] <- c(7,8) # manually fill in values for the variable 'bar'
#' 	labels
#'	# now provide values as well
#' 	vlist <- list(c("a", "b"), c(1, 2), c("peter", "paul")) 
#'	labels <- generateMergeLabels(fd1, fd2, c("foo", "bar", "foobar"), c("c", "n", "c"), vlist) 
#' 	labels
#' }
#' @family dataset modification functions
#' @return An object of class 'aquap_mergeLabels'.
#' @seealso mergeDatasets
#' @name generateMergeLabels
NULL

#pv_noMatchHChar <- c("ask", "delete", "fillIn", "stop")
#' @title Merge Datasets
#' @description Merge together two or more datasets, and possibly add class- or 
#' numerical variables to each dataset via the 'mergeLabels' object.
#' @details The resulting dataset is void of metadata (object@metadata) and analysis 
#' procedures. The order of column names in each header in a dataset is irrelevant, 
#' e.g. a header with the column names 'AA, BB, CC' does overlap with a header with 
#' the column names 'AA, CC, BB'.
#' @param ds1 An object of class 'aquap_data' or a list containing any number of 
#' objects of class 'aquap_data'
#' @param ds2 An object of class 'aquap_data', can be missing.
#' @param mergeLabels An object of class 'aquap_mergeLabels' as generated by 
#' \code{\link{generateMergeLabels}}, can be missing.
#' @param noMatchH Character length one. Defines what should happen in the case of 
#' non-matching header structures, i.e. the column names of the headers of the 
#' datasets to me merged can not be overlapped. The default value is defined in 
#' the settings.r file (\code{gen_merge_noMatchH}). Possible values are: 
#' \describe{
#' \item{ask}{The non-matching header-columns in each dataset are 
#' displayed, and the user is asked interactively what to do, with the three options 
#' below as possible options.}
#' \item{delete}{Non-matching header columns are automatically deleted.}
#' \item{fillIn}{Each column name not existing in all of the datasets 
#' to be merged is added to those datasets where it does not exist. The data is filled 
#' in with 'NAs'.}
#' \item{stop}{In case of non-overlapping header structures, the merging 
#' process is stopped, with possibly a message being displayed.}
#' }
#' @param noMatchW Character length one. Defines what should happen in the case of 
#' non-matching wavelengths, i.e. the wavelengths in the datasets to be merged are 
#' not identical. The default value is defined in the settings.r file 
#' (\code{gen_merge_noMatchH}). Possible values are:
#' \describe{
#' \item {ask}{The non-matching wavelenghts in each dataset are displayed, and the 
#' user is asked interactively what to do, with the five options below as possible 
#' options.}
#' \item{cut}{All wavelengths outside a range common to all datasets will be deleted. 
#' In other words, for some datasets the 'outsiders', i.e. the wavelengths outside 
#' of that common range, will be deleted.}
#' \item{fill}{Missing wavelengths will be filled in with 'NAs'. In other words, the 
#' wavelengths of all datasets will be expanded to encompass the overal mimimum and 
#' the overal maximum of the wavelengths of the datasets.}
#' \item{cutresa}{XXX}
#' \item{fillresa}{XXX}
#' \item{stop}{In case of non-matching wavelengths, the merging process is stopped, 
#' with possibly a message being displayed.}
#' }
#' @param dol Logical length one. If outliers should be detected based on the scope 
#' of the new, merged dataset. The default value is defined in the settings file at
#' \code{gen_merge_detectOutliers}.
#' @return An object of class 'aquap_data', with all the single datasets merged
#' together.
#' @family dataset modification functions
#' @seealso generateMergeLabels
#' @name mergeDatasets
NULL
