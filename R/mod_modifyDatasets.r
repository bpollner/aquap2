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

#' @title Generate Merge Labels
#' @description Generate an object of class 'aquap_mergeLabels' that can be used 
#' to specify the names and values of new variables in the merged dataset.
#' @details Provide either two datasets via the arguments ds1 and ds2, or a list
#' containing any number of datasets. In the latter case, argument ds2 is ignored. 
#' The lengths of the vectors in argument 'varNames' and 'varTypes' have to be the 
#' same. Providing a named list at argument 'ds1' leads to these names being 
#' displayed as row names in the displayed data frame when inspecting the resulting 
#' objet of class 'aquap_mergeLabels', potentially making it easier to assign the 
#' values. If the argument 'values' is left at its default NULL, an empty data frame 
#' displaying the new variables and possibly the names of the datasets to merge is 
#' displayed when inspecting the generated object. The values for each column of this 
#' dataframe now can be filled in via standard dataframe subselection. It can be easier 
#' to first generate the 'aquap_mergeLabels' object without providing the values, and 
#' then, when one can look at the displayed data frame, to fill in the values column 
#' by column via standard dataframe subselection. At any checking process (done 
#' automatically) the definitions given in argument 'varTypes' are decisive. The last 
#' column in the data frame shows the number of observations in each dataset, and it 
#' can not be changed manually. It is advised to change the valus exclusively via 
#' subsetting the object ("[<-" or "$<-").
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
#' @export
generateMergeLabels <- function(ds1, ds2=NULL, varNames, varTypes, values=NULL) {
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
		dsNameVec <- c(deparse(substitute(ds1)), deparse(substitute(ds2))) # get the names of the provided objects
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

mergeDatasets_two <- function(ds1, ds2, mergeLabels, dol) { # newLabels can come in as NULL
	dsList <- list(ds1, ds2)
	names(dsList) <- c(deparse(substitute(ds1)), deparse(substitute(ds2))) # get the names of the provided objects
	return(mergeDatasets_list(dsList, mergeLabels, dol))
} # EOF

mergeLabelObj_toList <- function(mergeLabels) {
	ml <- mergeLabels
#	char <- "These are not the metadata, but the info generated from the mergeLabels"
	char <- "from mergeLabels"
	out <- list(note=char, numVec=ml@numVec, varNames=ml@varNames, varTypes=ml@varTypes, values=ml@values, dsNames=ml@dsNames)
	return(out)	
} # EOF

### CORE ###
mergeDatasets_list <- function(dsList, mergeLabels, dol) { # newLabels can come in as NULL   ####CORE####
	stn <- get("stn", envir=.ap2)
	clpref <- stn$p_ClassVarPref
	olc <- stn$p_outlierCol
	#
	checkForSameness <- function(xList, char=NULL) {
		cns1 <- colnames(xList[[1]])
		for (i in 2: length(xList)) {
			cnsi <- colnames(xList[[i]])
			if (!identical(cns1, cnsi)) { # compares the first one with all the others
				stop(paste0("Sorry, at the moment only identical ", char, "-structures can be merged"), call.=FALSE)
				return(FALSE)
			} # end if
		} # end for i
	} # EOIF
	#
	newLabelBlock <- NULL
	numVec <- sapply(dsList, nrow)
	dsNames <- names(dsList)
	if (!is.null(mergeLabels)) {
		newLabelBlock <- merge_makeNewLabelBlock(mergeLabels)
		numVec <- mergeLabels@numVec # not really necessary
		dsNames <- mergeLabels@dsNames
	} # end if 
	headerList <- lapply(dsList, getHeader)
	nirList <- lapply(dsList, getNIR)
	colRepList <- lapply(dsList, getColRep)
	wlsList <- lapply(dsList, getWavelengths)
	##
	checkForSameness(headerList, "header")   #### here checking for sameness !! #####
	checkForSameness(nirList, "wavelength")
	##
	# now, for the moment, all structure is the same
	header <- as.data.frame(matrix(NA, nrow=sum(numVec) , ncol=ncol(dsList[[1]]$header) ))
	NIR <- as.data.frame(matrix(NA, nrow=sum(numVec) , ncol=ncol(dsList[[1]]$NIR) ))
	colRep <- as.data.frame(matrix(NA, nrow=sum(numVec) , ncol=ncol(dsList[[1]]$colRep) ))
	riVec <- c(0, cumsum(numVec))+1
	if (!stn$allSilent) {
		cat(paste0("Merging ", length(dsList), " datasets... "))
	}
	for (k in 1: (length(riVec)-1)) {
		ris <- riVec[k]
		rie <- riVec[k+1]-1
		for (i in 1: ncol(header)) {
			aa <- headerList[[k]][,i]
			if (is.factor(aa)) {
				aa <- as.character(aa)	# otherwise we destroy the numeric-information
			}
			header[ris:rie,i] <- aa  # colwise			### CORE ###
			colRep[ris:rie,] <- colRepList[[k]]			### CORE ###
			NIR[ris:rie,] <- nirList[[k]]				### CORE ###
		} # end for i
	} # end for k
	header <- cbind(header, newLabelBlock)
	colnames(header) <- c(colnames(headerList[[1]]), colnames(newLabelBlock))	# just take the first one -- for the moment, all are the same
	ind <- which(colnames(header) == paste0(clpref, olc, "_all"))
	colnames(header)[ind] <- paste0(clpref, olc, "_single_all")
	colnames(colRep) <- colnames(colRepList[[1]])
	colnames(NIR) <- colnames(nirList[[1]])
	rownames(header) <- paste0("r", 1:nrow(header))
	rownames(colRep) <- paste0("r", 1:nrow(colRep))
	rownames(NIR) <- paste0("r", 1:nrow(NIR))
	NIR <- as.matrix(NIR)
	#
	if (dol) {
		outliers <- flagOutliers_allScope(NIR, detectOutliers=TRUE)
		header <- cbind(header, outliers)
	} # end if
	#
	fd <- data.frame(I(header), I(colRep), I(NIR))
	names(fd) <- names(dsList[[1]])
	fd <- new("aquap_data", fd)
	rownames(fd) <- paste0("r", 1:nrow(fd))
	for (i in 1:ncol(fd$header)) {
       if (any(is.character(fd$header[, i]))) {
           fd$header[i] <- factor(fd$header[, i])
       } # end if
    } # end for i
    fd <- reColor(fd)
    fd@ncpwl <- getNcpwl(dsList[[1]]) # just take the first one
    fd@metadata <- mergeLabelObj_toList(mergeLabels)
    fd@version <- pv_versionDataset
    if (!stn$allSilent & !dol) {
		cat(paste0("ok. \n"))
	}
	return(fd)
} # EOF

# setClass("aquap_mergeLabels", slots=c(numVec="integer", varNames="character", varTypes="character", values="list", dsNames="character"), contains="data.frame")

# general idea: first generate the complete block with new labels, then merge the x datasets, then merge new big dataset with the block of new labels, then reFactor and reColor the final dataset

#' @title Merge Datasets
#' @description Merge together two or more datasets, and possibly add class- or 
#' numerical variables to each dataset.
#' @details The resulting dataset is void of metadata (object@metadata) and analysis 
#' procedures.
#' @param ds1 An object of class 'aquap_data' or a list containing any number of 
#' objects of class 'aquap_data'
#' @param ds2 An object of class 'aquap_data'.
#' @param mergeLabels An object of class 'aquap_mergeLabels' as generated by 
#' \code{\link{generateMergeLabels}}.
#' @param dol Logical length one. If outliers should be detected based on the scope 
#' of the new, merged dataset. Defaults to 'FALSE'.
#' @return An object of class 'aquap_data', with all the single datasets merged
#' together.
#' @family dataset modification functions
#' @name mergeDatasets
NULL
