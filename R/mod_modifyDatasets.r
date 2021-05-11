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
mergeDatasets_two <- function(ds1, ds2, mergeLabels, noMatch=get(".ap2$stn$gen_merge_noMatch"), dol=get(".ap2$stn$gen_merge_detectOutliers")) { # newLabels can come in as NULL
	dsList <- list(ds1, ds2)
#	names(dsList) <- c(deparse(substitute(ds1)), deparse(substitute(ds2))) # get the names of the provided objects ### does NOT work
	names(dsList) <- paste0("dataset_", c(1,2))
	return(mergeDatasets_list(dsList, mergeLabels, noMatch, dol))
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

merge_checkNoMatchChar <- function(noMatch) {
	#	pv_noMatchChar <- c("ask", "delete", "rename", "stop")
	if (! all(is.character(noMatch)) | length(noMatch) > 1) {
		stop("Please provide a character length one to the argument 'noMatch'", call.=FALSE)
	}
	if (! noMatch %in% pv_noMatchChar) {
		stop(paste0("Please provide one of '", paste(pv_noMatchChar, collapse="', '"), "' to the argumet 'noMatch'"), call.=FALSE) 
	}
	return(noMatch)
} # EOF

merge_readInChoice <- function() {
	maxTries <- 3
	ask <- TRUE
	aa <- 1
	txt <- "Please type in either 1, 2, or 3."
	#
	checkChoice <- function(choice) {
		if (length(choice) == 0) {
			message(txt)
			return(TRUE)			
		}
		if (! choice %in% c(1,2,3)) {
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
	missVisual <- NULL
	aa <- as.vector(unlist(sapply(headerList, colnames)))   	# first collect all column names into a single vector
	cnsTab <- table(aa) # gives a named numeric
	tabMissing <- sort(cnsTab[which(cnsTab < length(headerList))], decreasing = TRUE) # gives back a named numeric
	if (length(tabMissing) != 0) { # so we do have non-matching column names
		namesMissing <- sort(names(tabMissing))  #  gives the names of those columns that are NOT in all datasets
		missVisual <- as.data.frame(matrix("ok", nrow=length(headerList), ncol=length(tabMissing)))
		rownames(missVisual) <- dsNames
		colnames(missVisual) <- namesMissing
		for (i in 1: length(headerList)) { # the rows in missVisual
			for (k in 1: length(namesMissing)) { # the columns in missVisual
				haveIt <- namesMissing[k] %in% colnames(headerList[[i]])
				if (!haveIt) {
					missVisual[i,k] <- "miss"
				} # end if
			} # end for k (going through the missing names)
		} # end for i (going through the datasets)
	} # end if  length(tabMissing) != 0
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

###### CORE #########
mergeDatasets_list <- function(dsList, mergeLabels, noMatch=get(".ap2$stn$gen_merge_noMatch"), dol=get(".ap2$stn$gen_merge_detectOutliers")) { # newLabels can come in as NULL   ####CORE####
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
					add <- "\nWe are terribly sorry, but apparently there was an error while deleting / filling in header columns\n"
				} # end if
				stop(paste0("Sorry, at the moment only identical ", char, "-structures can be merged", add), call.=FALSE)
				return(FALSE)
			} # end if
		} # end for i
	} # EOIF
	getRLEs_num <- function(x, cn=snrColName_y) {
		return(rle(as.numeric(x$header[,cn])))
	} # EOIF
	#
	#
	#### entry-checking ######
	merge_checkListInput(dsList)
	noMatch <- merge_checkNoMatchChar(noMatch) # check if input is valid
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
			
	############## check for non-matches, handle them ################
	aa <- as.vector(unlist(sapply(headerList, colnames)))   	# first collect all column names into a single vector
	cnsTab <- table(aa) # gives a named numeric
	namesComplete <- names(which(cnsTab == length(headerList))) # gives the names of those columns that are present in all datasets
	tabMissing <- sort(cnsTab[which(cnsTab < length(headerList))], decreasing = TRUE) # gives back a named numeric
	if (length(tabMissing) != 0) { # so we have a non-match
		namesMissing <- names(tabMissing)  #  gives the names of those columns that are NOT in all datasets
		missVisual <- merge_makeMissVisual(headerList, dsNames) 
		txtGen <- paste0("\nThere were ", length(namesMissing), " non-matching header columns detected.\n")
		#		
		if (noMatch == pv_noMatchChar[1]) { # "ask"   # yes, I know.  I tried a "switch". Could not make it work. Shame.
			message(txtGen)
			print(missVisual)
			cat(paste0("\nShould the non-matching header columns be:\n1 - deleted,\n2 - filled in, or\n3 - should the merging of datasets be stopped?\n"))
			choice <- merge_readInChoice()
			if (choice == 1) {
				if (!thisSilent) {
					cat(paste0("Deleting header-columns... \n"))
				} # end if
				noMatch <- pv_noMatchChar[2] ## so we go into the delete below
				thisSilent <- TRUE
			} # end if
			if (choice == 2) {
				if (!thisSilent) {
					cat(paste0("Filling in header-columns... \n"))
				} # end if
				noMatch <- pv_noMatchChar[3] ## so we go into the filling in below
				thisSilent <- TRUE
			} # end if
			if (choice == 3) {
				noMatch <- pv_noMatchChar[4] ## so we go into the stop below
				thisSilent <- TRUE
			} # end if
		} # end "ask"
		##
		if (noMatch == pv_noMatchChar[2]) { # "delete"
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
		if (noMatch == pv_noMatchChar[3]) { # "fill in"
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
		if (noMatch == pv_noMatchChar[4]) { # "stop"
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
	##
	##		
#	print(merge_makeMissVisual(headerList, dsNames)); wait()
	checkForSameness(headerList, "header")   ## after the corrections above, this never should give a no-match. Leave it here just to check.
	checkForSameness(nirList, "wavelength")  #### here checking for sameness !! #####  # now, for the moment, all NIR has to be the same
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
    fd@ncpwl <- getNcpwl(dsList[[1]]) # just take the first one
    fd@mergeInfo <- merge_labelObjectToSlot(dsList, mergeLabels)
    fd@version <- pv_versionDataset
    if (!stn$allSilent & !dol) {
		cat(paste0("ok. \n"))
	}
	return(fd)
} # EOF

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
#' @name generateMergeLabels
NULL

#pv_noMatchChar <- c("ask", "delete", "fillIn", "stop")
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
#' @param noMatch Character length one. Defines what should happen in the case of 
#' non-matching header structures, i.e. the column names of the headers of the 
#' datasets to me merged can not be overlapped. The default value is defined in 
#' the settings.r file (\code{gen_merge_noMatch}). Possible values are: 
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
#' @param dol Logical length one. If outliers should be detected based on the scope 
#' of the new, merged dataset. The default value is defined in the settings file at
#' \code{gen_merge_detectOutliers}.
#' @return An object of class 'aquap_data', with all the single datasets merged
#' together.
#' @family dataset modification functions
#' @name mergeDatasets
NULL
