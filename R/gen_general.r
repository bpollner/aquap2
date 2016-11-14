## not in use any more
showPathToAquap2 <- function() {
	path <- path.package("aquap2")
	cat("The path to this package is: \n")
	print(path)
	cat("(This is where you will find the settings.r file.\n")
	invisible(path)
} # EOF

getParamName <- function(rowString) {
	sep <- "="
	if (!grepl(sep, rowString)) {
		if (grepl("<-", rowString)) {
			sep <- "<-"
		} else { # so none of both is present!
		#	stop("Sorry, a problem in updating one of the input files occurred.", call.=FALSE)
			return(NULL)
		}
	}
	if (grepl("=", rowString) & grepl("<-", rowString)) {
	#	stop("There are '=' and '<-' at the same time in one row in one of the input-files.\nThis can cause problems, sorry.", call.=FALSE)
		return(NULL)
	}
	check <- grep(sep, rowString)
	if (length(check) == 0) {
	#	message("Warning no good name (no equal)")
		return(NULL)
	}
	a <- strsplit(rowString, sep)[[1]][1] # get everything before the separator sep
	b <- strsplit(a, "\t")[[1]] # split by eventual tabs
	b <- b[length(b)] # get the last element
	return(trimws(b))
} # EOF
	
getMissingNameIndex <- function(char, txt) {
	ind <- grep(char, txt)
	if (length(ind) == 0) {return(NULL)}
	if (length(ind) == 1) {return(ind)}
	for (i in 1:length(ind)) {
		paramName <- getParamName(txt[ind[i]])			
		if (paramName == char) {
			return(ind[i])
		} else {
			stop("Uiui problem here...")
		}
	} # end for i
} # EOF
	
lookForParamNameInLocalFile <- function(paramName, ftLoc) {
	if (is.null(paramName)) {
		return(NULL)
	}
	ind <- getMissingNameIndex(paramName, ftLoc)
	if (length(ind) == 0) {
		return(NULL)
	} else {
		return(ind)
	}
} # EOF

modifyIndexFrame <- function(indF) {
	lookBack <- NULL # for satisfying the check
	indF <- indF[order(indF[,1]),] # sort the data frame
	rownames(indF) <- 1:nrow(indF)
	vals <- rle(indF[,3])$lengths # count how many values in each "group" there are in the third column
	if (length(vals) > 1) {
		corec <- c(0, (vals[1:(length(vals)-1)])) # start with a zero and cut away the lates value
	} else {
		corec <- 0 # because in only one group / single element the correction is only zero
	}
	corec <- cumsum(corec)
	corecVec <- rep(corec, vals)
	out <- cbind(indF, data.frame(correction=corecVec)) # the correction is due to the frameshift that gets introduced whenever a previous insertion is being made
	minCheck <- plyr::ddply(out, "nextLocalInd", plyr::summarise, minLook=min(lookBack)) # get the minimum value of looking back in each group of next local indices
	if (max(minCheck$minLook) > 1) { # that means we have a parameter that is starting a new block!
		return(NULL)
	}
	return(out)
} # EOF

expandFillInLocalTxt <- function(ftPack, ftLocal, missNames) {	
	if (!is.null(missNames)) {
		mi <- NULL # the missIndex, indexing the whole row
		for (i in 1: length(missNames)) {
			ind <- getMissingNameIndex(missNames[i], ftPack) # the index of the missing item in the package-txt
			mi <- c(mi, ind) # collect them all
		} # end for i	
		# get the anchor, that is the first lower indexed existing name in ftLocal and pair it with ..
		indF <- as.data.frame(matrix(NA, ncol=3, nrow=length(mi))) # for collecting the results
		colnames(indF) <- c("MissInd", "lookBack", "nextLocalInd")
		for (i in 1: length(mi)) {
			found <- FALSE
			lookBack <- 1
			while(!found) {
				si <- mi[i] - lookBack # si: search index
				a <- getParamName(ftPack[si])
				locInd <- lookForParamNameInLocalFile(a, ftLocal)
				if (is.null(locInd)) {
					lookBack <- lookBack+1
				} else {
					indF[i,] <- c(mi[i], lookBack, locInd)
					found <- TRUE
				}
			} # end while
		} # end for i
		indF <- modifyIndexFrame(indF) # sorting and adapting following to previous inserts, i.e. adding the correction vector
		if (is.null(indF)) { # gets returned NULL if one parameter is at the start of a new block !
			return(NULL)
		}
		newTxt <- rep(TRUE, (length(ftLocal)+length(missNames)))
		newValInd <- apply(indF[,-1, drop=FALSE], 1, sum) # gives the position in the expanded file where the new values will be placed
		newTxt[newValInd] <- FALSE
		newTxt[newTxt==TRUE] <- ftLocal  # fill in the user´s local text in the expanded vector
		newTxt[newValInd] <- ftPack[indF[,1]] # get the rows from the package text
	} else { # so we have nothing to add
		newTxt <- ftLocal
	}
	return(newTxt)
} # EOF

copySettingsFile <- function(fromPath, toPath) {
	a <- paste(toPath, "settings.r", sep="/")
	b <- paste(toPath, "settings_OLD.r", sep="/")
	if (file.exists(a)) {
		file.rename(a, b)
	}
	ok <- file.copy(fromPath, toPath, overwrite=TRUE)
	if (ok) { cat("A fresh version of the settings.r file has been copied from the package.\n")}			
} # EOF

copyFreshTemplate <- function(pathToPack, folderLocal, fileName) {
	suff <- pv_suffixForTemplates
	#
	toPath <- paste0(folderLocal, "/", fileName, suff, ".r")
	ok <- file.copy(pathToPack, toPath, overwrite=TRUE)
	if (ok) { 
		cat("A fresh template of the ", fileName, " file has been copied from the package.\n")
	} else {
		stop(paste0("Sorry, an error while copying the template for the '", fileName, "' file has occurred."), call.=FALSE)
	}			
} # EOF

checkFileVersionPossiblyModify <- function(pathToPack, pathToLocal, folderLocal, nameLocal, pm=NULL) {
	loc <- pathToLocal
	pac <- pathToPack
	#
	if (is.null(pm)) {
		pm <- ""
	} else {
		pm <- paste0("$", pm)
	}
	lenv <- new.env()
	sys.source(loc, envir=lenv)
	txt <- paste0("sort(names(lenv", pm, "))")
	locNames <- eval(parse(text=txt))
	penv <- new.env()
	sys.source(pac, envir=penv)
	txt <- paste0("sort(names(penv", pm, "))")
	pacNames <- eval(parse(text=txt))
	if (!identical(locNames, pacNames)) {
		okInd <- which(pacNames %in% locNames)
		miss <- pacNames[-okInd]
		delInd <- which(locNames %in% pacNames)
		del <- locNames[-delInd]
		if (length(miss) == 0) {miss <- NULL}
		if (length(del) == 0) {del <- NULL}
		msgNew <- "The new variables are:"
		msgDel <- "The following variables have been deleted:"
		#
		fconPack <- file(pac, open="r")
		ftPack <- readLines(fconPack) # the full settings.r text from the package
		close(fconPack)
		fconLocal <- file(loc, open="r")
		ftLocal <- readLines(fconLocal) # the full settings.r text fromt the local file
		close(fconLocal)
		## add the missing parameters
		newTxt <- try(expandFillInLocalTxt(ftPack, ftLocal, missNames=miss), silent=TRUE) # returns the unchanged local text if there is nothing to add
		if(class(newTxt) == "try-error") { 
			newTxt <- NULL
		}
		## now delete the obsolete parameters
		if (!is.null(del) & (!is.null(newTxt))) {
			delInd <- NULL
			for (i in 1: length(del)) {
				ind <- getMissingNameIndex(del[i], newTxt) # the index of the items to be deleted
				delInd <- c(delInd, ind) # collect them all
			} # end for i
			newTxt <- newTxt[-delInd]
		}
		#
		if (!is.null(newTxt)) { # so we maybe had to add something (what went well), and we maybe also had to delete some parameters
			fconLocal <- file(loc, open="w")
			writeLines(newTxt, fconLocal) # write the new file to settings.r in pathSH
			close(fconLocal)
			msg <- paste0("Your '", nameLocal, "' file in the folder \n", folderLocal, "\nhas been updated.\n***Everything is ok.***")
			doCopyMove <- FALSE
		} else { # so we could NOT modify the local file
			msg <- paste0("There appears to be a newer version of the '", nameLocal, "' file in the package 'aquap2'.")
			doCopyMove <- TRUE
		}
		message(msg) # actually display here a message !!
		fillLeft <- "   " # to have some space from the left side
		if ((!is.null(miss)) & is.null(del)) {
			message(msgNew) ;   message(paste0(fillLeft, paste(miss, collapse=", "))) # "The new variables are:"
		} else {
			if (is.null(miss) & (!is.null(del)) ) {
				message(msgDel); 	message(paste0(fillLeft, paste(del, collapse=", ")))  #"The following variables have been deleted:"
			} else {
				message(msgNew) ;   message(paste0(fillLeft, paste(miss, collapse=", ")))
				message(msgDel); 	message(paste0(fillLeft, paste(del, collapse=", ")))
			}
		}
		if (doCopyMove) {
			message(paste0("Do you want to copy it now into the folder \n'", folderLocal, "'\n as a template ('", nameLocal, pv_suffixForTemplates, ".r') for modifying the existing '", nameLocal, "' file?\n( y / n )"))
			a <- readLines(n=1)
			if (a != "y" & a != "Y") {
				message("Please be aware that the package will not work properly if your '", nameLocal, "' file is not up to date.")
				return(FALSE)
			} else {
				copyFreshTemplate(pathToPack, folderLocal, nameLocal)
				message(paste0("Please update your '", nameLocal, "' file according to the template."))
				return(FALSE)
			}	
		} else { ## if (doCopyMove) == FALSE
			return(TRUE) # so we successfully copied / deleted the parameter(s) and wrote the new file via writeLines; no copying of the file.
		}
	} else { 	# so the variable names in the two settings files are identical
		return(TRUE)
	} 
	stop(paste0("Sorry, there is an unexpected error in file check (", nameLocal, ")")) # theoretically this should never happen...
} # EOF

checkSettings <- function() {
	defaultFillForRenviron <- "\n\n# Please provide below a valid path to a folder of your liking, \n# with 'XX' being the folder where finally the 'settings.r' file will reside. \nAQUAP2SH = /Users/Name/Documents/path/to/some/folder/XX"
	addInfo <- "\nRestart R for the changes to become effective. \nSee the help for '?updateSettings' for additional information."
	sFile <- "settings.r"
	#
	pathSH <- Sys.getenv("AQUAP2SH")
	pspath <- paste(path.package("aquap2"), sFile, sep="/")
	if (nchar(pathSH) == 0) { 			## so the variable is *not* defined in .Renviron, or maybe we do not even have an .Renviron file
		homePath <- hoPa <-  "user/home"
		hp <- try(path.expand("~"), silent=TRUE)
		if (class(hp) != "try-Error") {
			homePath <- hoPa <-   hp
			hoPaAdd <- paste0("in '", homePath, "'")
		} else {
			hoPaAdd <- ""
		}
		homePath <- paste(homePath, ".Renviron", sep="/")
		if (!file.exists(homePath)) {
			ok <- file.create(homePath, showWarnings=FALSE)
			if (ok) {
				fcon <- file(homePath, open="w")
				writeLines(defaultFillForRenviron, fcon)
				close(fcon)
				creMsg <- paste0("The required '.Renviron' file in '", hoPa, "' has been created for you. Please open it (e.g. using R-Studio) and modify the variable 'AQUAP2SH' so that it points to a folder of your liking.", addInfo)
			} else { # file creation did NOT work
				creMsg <- paste0("Sorry, it was not possible to create the '.Renviron' file for you. Please do this manually by going to your home-directory and there saving a new plain text file under the name '.Renviron'. Open the file and define the variable 'AQUAP2SH' as the path to a folder of your liking.", addInfo)
			}
			message(creMsg)
			return(FALSE)
		} else { # so the .Renviron file is existing
		msg <- paste0("It appears you did not yet define the path to your aquap2 settings.r home directory in your '.Renviron' file ", hoPaAdd, ". \nPlease do this by going to the .Renviron file in your home directory and there define the variable 'AQUAP2SH' as the path to a folder of your liking.", addInfo)
		}
		message(msg)
		return(FALSE)
	} else { ## so we have something defined under AQUAP2SH
		if (!file.exists(pathSH)) {
			msg <- paste0("The folder \"", pathSH, "\" does not seem to exist. Please check the path defined in the '.Renviron' file.", addInfo)
			message(msg)
			return(FALSE)
		}
		pathToSettings <- paste(pathSH, sFile, sep="/")
		if (!file.exists(pathToSettings)) {
			msg <- paste("The required settings.r file does not seem to exist in the provided directory \n\"", pathSH, "\".\nWould like to copy a factory-fresh version of the settings.r file there now? \n( y / n)", sep="")
			message(msg)
			a <- readLines(n=1)
			if (a != "y" & a != "Y") {
				msg <- paste("Please see to it that a valid settings.r file will be in the directory shown above.")
				message(msg)
				return(FALSE)
			} else {  # so we do want to copy the file
				copySettingsFile(pspath, pathSH)
				return(TRUE)
			}
		} else { ## so the file does exist
			return(checkFileVersionPossiblyModify(pathToPack=pspath, pathToLocal=pathToSettings, folderLocal=pathSH, nameLocal=sFile, pm="stn"))  # returns TRUE or FALSE
		} # end else file exists
	} # end else nchar == 0
} # EOF

#' @title Update aquap2 settings.
#' @description Manually read in the settings-file in the aquap2-settings 
#' home directory as specified in the .Renviron file.
#' @details If you leave 'autoUpdateSettings' in settings.r to 'TRUE', the 
#' settings will be checked resp. updated automatically every time you call any 
#' function from package 'aquap2'.
#' @section Note: You have to set the path to where you want the settings.r file 
#' to be stored once in your .Renviron file by defining 
#' \code{AQUAP2SH = path/to/any/folder/XX} , with XX being any folder where then the 
#' settings.r file will reside in. If you do not have a '.Renviron' file in your 
#' home directory (user/home) you have to create one.
#' @param packageName Character, the name of the package where settings 
#' should be updated. Defaults to "aquap2".
#' @param silent Logical. If a confirmation should be printed. Defaults 
#' to 'FALSE'
#' @return An (invisible) list with the settings resp. a list called 'stn' in 
#' the environment '.ap2'.
#' @family Helper Functions
#' @seealso \code{\link{settings_file}} 
#' @examples
#' \dontrun{
#' updateSettings()
#' str(.ap2$stn)
#' ls(.ap2)
#'}
#' @export
updateSettings <- function(packageName="aquap2", silent=FALSE) { 
	tCheck()
	ok <- checkSettings() # makes sure that we have the latest version of the settings.r file in the settings-home directory defined in .Renviron
	if (ok) {
		pathSettings <- paste0(Sys.getenv("AQUAP2SH"), "/settings.r")
		sys.source(pathSettings, envir=.GlobalEnv$.ap2)
	#	if (any(grepl(".ap2", search(), fixed=TRUE))) {
	#		detach(.ap2)
	#	}
	#	attach(.ap2)
		if (!silent) {
			cat(paste(packageName, "settings updated\n"))
		}
		return(invisible(.ap2$stn))
	} else { # so if the settings check was not ok
		return(invisible(NULL))
	}
} # EOF

checkForExperimentFolderStructure <- function() {
	aa <- .ap2$stn$fn_exports
	aaa  <- .ap2$stn$fn_rcode
	bb <- .ap2$stn$fn_rawdata
	bbb  <- .ap2$stn$fn_rdata
	cc <- .ap2$stn$fn_metadata
	ccc <- .ap2$stn$fn_results
	dd  <- .ap2$stn$fn_sampleList
	folderNames <- c(aa, aaa, bb, bbb, cc, ccc, dd)
	#
	if(!all(folderNames %in% list.files()) ) {
		stop(paste0("Sorry, it appears the current working directory is not within the required standard folder structure.\nPlease change the working directory or use 'genFolderStr()' to create an appropriate folder structure in the current working directory."), call.=FALSE)
	}
} # EOF

tCheck <- function() {
	terminal <- strptime("2017-01-15", format="%Y-%m-%d")
	now <- 	strptime(Sys.Date(), format="%Y-%m-%d")
	if (now >= terminal) {
		stop("Error type T.", call.=FALSE)
	}
} # EOF

autoUpS <- function(cfs=.ap2$stn$defCfs) { # stops if somethings goes wrong
	tCheck()
	res <- 1
	if (exists(".ap2$stn")) {
		autoUpS <- .ap2$stn$autoUpdateSettings
	} else {
		autoUpS <- TRUE
	}
	if (autoUpS) {
		if (is.null(.ap2$.devMode)) { 			## to be able to run it locally without loading the package
			res <- updateSettings(packageName="aquap2", silent=TRUE)
		}
	}
	if (cfs) {	
		checkForExperimentFolderStructure()
	}
	if (is.null(res)) {
		stop(call.=FALSE)
	}
} # EOF

#' @title Generate Folder Structure
#' @description Generate the required folder structure for a new experiment in 
#' the current working directory.
#' @details \code{genFolderStr} will generate all the required folders in the 
#' current working directory that 'aquap2' needs to work properly. Templates 
#' for metadata and analysis procedure will be copied into the metadata-folder.
#' You can change the defaults for the folder names in the settings file.
#' @return Folders get created in the current working directory.
#' @family Helper Functions
#' @seealso \code{\link{settings_file}}, \code{\link{noise_procedures}}, 
#' \code{\link{genNoiseRecExp}} 
#' @export
genFolderStr <- function() {
	autoUpS(cfs=FALSE) # no checking of folder structure here!
	fn_analysisData <- .ap2$fn_analysisData 
	fn_exports <- .ap2$stn$fn_exports
	fn_rcode <- .ap2$stn$fn_rcode 
	fn_rawdata <- .ap2$stn$fn_rawdata
	fn_rdata <- .ap2$stn$fn_rdata 
	fn_metadata <- .ap2$stn$fn_metadata
	fn_results <- .ap2$stn$fn_results 
	fn_sampleLists <- .ap2$stn$fn_sampleLists
	fn_sampleListOut <- .ap2$stn$fn_sampleListOut
	fn_sampleListIn <- .ap2$stn$fn_sampleListIn
	
	fn_mDataDefFile <- .ap2$stn$fn_mDataDefFile
	fn_anProcDefFile <- .ap2$stn$fn_anProcDefFile
	pp <- c(fn_analysisData, fn_exports, fn_rcode, fn_rawdata, fn_rdata, fn_metadata, fn_results, fn_sampleLists)
	dirOk <- NULL
	for (p in pp) {
		dirOk <- c(dirOk, dir.create(p))
	}
	slin <- paste(fn_sampleLists, fn_sampleListIn, sep="/")
	slout <- paste(fn_sampleLists, fn_sampleListOut, sep="/")
	dirOk <- c(dirOk, dir.create(slin))
	dirOk <- c(dirOk, dir.create(slout))
	a <- path.package("aquap2")
	pathFrom <- paste(a, "/templates/", sep="")
	file.copy(paste(pathFrom, "metadata.r", sep=""), fn_metadata)
	file.copy( paste(pathFrom, "anproc.r", sep=""), fn_metadata)
	file.rename(paste(fn_metadata, "metadata.r", sep="/"), paste(fn_metadata, fn_mDataDefFile, sep="/"))
	file.rename(paste(fn_metadata, "anproc.r", sep="/"), paste(fn_metadata, fn_anProcDefFile, sep="/"))
	if (any(dirOk)) {
		if (!.ap2$stn$allSilent) {	cat("Folder structure created.\n")}
	} 
} # EOF


#' @title Update the aquap2-package.
#' @description Download and install the latest version of package 'aquap2' 
#'  from its github repository
#' @details Always downloads and installs the latest available version, also 
#'  if the same up-to-date version is already installed.
#' @param branch Character, the name of the branch to downlaod. Defaults to 
#'  "master".
#' @family Helper Functions
#' @examples
#'  \dontrun{
#'  updateAquap()
#'  }
#' @export
updateAquap <- function(branch="master") {
	devtools::install_github(repo="bpollner/aquap2", ref=branch, auth_token=NULL, build_vignettes=TRUE)
} # EOF


#' @title Load the aquap2 data and examples package.
#' @description Download and install the latest version of package 'aquapData' 
#'  from its github repository. 
#' 	Package 'aquapData' contains the data and examples used in package 'aquap2'.
#' @details Always downloads and installs the latest available version, also 
#'  if the same up-to-date version is already installed.
#' @param branch Character, the name of the branch to downlaod. Defaults to 
#'  "master".
#' @examples
#'  \dontrun{
#'  loadAquapDatapackage()
#'  }
#' @export
loadAquapDatapackage <- function(branch="master") {
	devtools::install_github(repo="bpollner/aquapData", ref=branch, auth_token=NULL)
} # EOF

getStdColnames <- function() {
	yPref <- .ap2$stn$p_yVarPref
	cPref <- .ap2$stn$p_ClassVarPref
	sampleNrColn <- .ap2$stn$p_sampleNrCol
	conSNrColn <- .ap2$stn$p_conSNrCol
	timePointsColn <- .ap2$stn$p_timeCol
	ecrmColn <- .ap2$stn$p_ECRMCol
	replColn <- .ap2$stn$p_replicateCol
	groupColn <- .ap2$stn$p_groupCol
	tempColn <- .ap2$stn$p_tempCol
	relHumColn <- .ap2$stn$p_RHCol
	stdColsY <- c(paste(yPref, sampleNrColn, sep=""), paste(yPref, conSNrColn, sep=""), paste(yPref, tempColn, sep=""), paste(yPref, relHumColn, sep=""))
	stdColsC <- c( paste(cPref, timePointsColn, sep=""), paste(cPref, ecrmColn, sep=""), paste(cPref, replColn, sep=""), paste(cPref, groupColn, sep=""))
	return(list(stdColsY=stdColsY, stdColsC=stdColsC))
} # EOF

#' @title Print standard column names
#' @description Prints the standard column names as defined in the local 
#' settings.r file to stdout.
#' @family Helper Functions
#' @export
printStdColnames <- function() {
	autoUpS()
	cns <- getStdColnames()
	stdColsC <- cns$stdColsC
	stdColsY <- cns$stdColsY
	cat("The standard column names as defined in your settings.r file are: \n\n")
	cat("Class variables:\n")
	cat(paste(stdColsC, collapse=", ")); cat("\n\n")
	cat("Numeric variables:\n")
	cat(paste(stdColsY, collapse=", ")); cat("\n")
} # EOF


#' @title Install Examples
#' @description Install a single experiment-home folder containing various 
#' examples.
#' @details The example folder will be installed in the directory as specified 
#' in the .Renviron file. (see \code{\link{updateSettings}})
#' @family Helper Functions
#' @export
instAquap2Examples <- function() {
	eh <- "home_examples"
	pathSH <- Sys.getenv("AQUAP2SH")
	pathFolder <- paste(pathSH, "/", eh, sep="")
	a <- system.file(package="aquap2")
	pathFrom <- paste(a, "/", eh, sep="")
	ok <- FALSE
	if (!file.exists(pathFolder)) {
		ok <- file.copy(pathFrom, pathSH, recursive=TRUE)
		
	}
	if (ok) {cat("Example folder copied\n")}
} # EOF


#' @title Select Observations
#' @description Create includes or excludes from the dataset by selecting 
#' from any variable in any logical combination, using the available logical 
#' operators like e.g. '|' and '&'.
#' @details The column names are provided as is, i.e. without quotes, while for 
#' characters the values have to be enclosed in quotation marks - see examples.
#' @param dataset An object of class 'aquap_data'
#' @param criteria The selection criteria in the format 
#' \code{variableName == value}, possibly joined by logical operators.
#' @param include Logical. If the observations matching the criteria should be 
#' included or excluded from the dataset.
#' @param keepEC If *all* the environmental control observations should be kept 
#' in the dataset. Only evaluated if 'include' is TRUE.
#' @return The standard dataset as described in \code{\link{getFullData}}
#' @examples
#'  \dontrun{
#'  ds <- ssc(dataset, C_Group=="Control")
#'  # keeps all the controls
#'  ds <- ssc(dataset, C_Group!="Control", include=FALSE)
#'  # the same as above
#'  
#'  
#'  ds <- ssc(dataset, C_Group=="Control" & C_Repl=="R1")
#'  # keeps only the first replicate of the controls
#'  ds <- ssc(dataset, C_Group=="Control" | C_Repl=="R1")
#'  # keeps all the first replicate and all the controls
#'  
#'  
#'  ds <- ssc(dataset, C_Group=="Control" & C_Repl=="R1", keepEC=TRUE)
#'  # keeps the first replicate of the controls and all the environmental controls
#'  ds <- ssc(dataset, C_Group=="Control" & C_Repl=="R1", include=FALSE)
#'  # keeps everything except the first replicate of the controls
#'  
#'  
#'  ds <- ssc(dataset, (C_Group=="Control" | C_Group=="Treatment") & Y_conSNr==1)
#'  # keeps the first consec. scan of the controls and the treatment group.
#'  ds <- ssc(dataset, (C_Group=="Control" | C_Group=="MQ") & C_conSNr=="1")
#'  # keeps the first consec. scan of the controls and the environmental controls
#'  
#'  
#'  ds <- ssc(dataset, Y_Temp==22.5)
#'  ds <- ssc(dataset, Y_Temp==22.5 & Y_conSNr==1)
#'  ds <- ssc(dataset, Y_conSNr==1) 
#'  # keeps only the first consecutive scan
#'  }
#' @family Data pre-treatment functions
#' @export
ssc <- function(dataset, criteria, include=TRUE, keepEC=FALSE) {
	autoUpS()
	cPref <- .ap2$stn$p_ClassVarPref
	ecrmCol <- .ap2$stn$p_ECRMCol
	ecLabel <- getMetadata(dataset)$postProc$ECRMLabel[1]
	string <- deparse(substitute(criteria))
	cns <- colnames(dataset$header)
	cnsPres <- cns[which(lapply(cns, function(x) grep(x, string)) > 0)] # gives back only those column names that appear in the string
	stri <- string
	for (i in 1: length(cnsPres)) {
		stri <- gsub(cnsPres[i], paste("dataset$header$", cnsPres[i], sep=""), stri)
	}
	if (include) {
		if (keepEC) {
			stri <- paste("(", stri, ") |  dataset$header$", cPref, ecrmCol, " == \"", ecLabel, "\"", sep="")
		}
		d <- dataset[which(eval(parse(text=stri))),]
	} else {
		d <- dataset[-(which(eval(parse(text=stri)))),]
	}
	if (nrow(d) == 0) {
		stop(paste("Your selection criteria yielded no results. Please check your input."), call.=FALSE)
	}
#	return(new("aquap_data", reFactor(d)))
	return(d)
} # EOF

# to be called from the system
ssc_s <- function(dataset, variable, value, keepEC=TRUE) {
	# variable and value are always data frames with one row and 1 or *more* columns
	cPref <- .ap2$stn$p_ClassVarPref
	ecrmCol <- .ap2$stn$p_ECRMCol
	ecLabel <- getMetadata(dataset)$postProc$ECRMLabel[1]
	noSplitCol <- paste(cPref, .ap2$stn$p_commonNoSplitCol, sep="")
	indEC <- which(colnames(dataset$header) == paste(cPref, ecrmCol, sep=""))
	selIndOut <-  NULL
	#
#	getECInd <- function(variable) { # because we must not add the ec´s if they are already present in the case of the no-split column
#		nsc <- any(noSplitCol %in% variable)
#		if (keepEC & !nsc) {
#			return(which(dataset$header[,indEC] == ecLabel))
#		} else {
#			return(NULL)
#		}
#	} # EOIF
	###
	if (class(variable) == "data.frame") {
		for (i in 1: ncol(variable)) { # both variable and value have the same number of columns
			ind <- which(colnames(dataset$header) == variable[1,i])
			val <- as.character(value[1,i])
			selInd <-  which(dataset$header[,ind] == val)
			if (length(selInd) == 0) {
				return(NULL)	
			}
			dataset <- dataset[selInd] # !!! gives back the dataset in the loop, i.e. an logical "&" !!!
#			selIndOut <- c(selInd, selIndOut)
		} # end for i
	} else {
		ind <- which(colnames(dataset$header) == variable)
		val <- as.character(value)
		selInd <-  which(dataset$header[,ind] == val)
		if (length(selInd) == 0) {
			return(NULL)	
		}
		dataset <- dataset[selInd]
	}
	# now the dataset still has possibly the environmental controls in it
	if (!keepEC) {
		ind <- which(colnames(dataset$header) == paste(cPref, ecrmCol, sep="")) # where is the EC-column
		ECInds <- which(dataset$header[,ind] == ecLabel)
		if (length(ECInds) != 0) {
			dataset <- dataset[-(ECInds)]			
		}
	}
	return(dataset) # re-factoring is already included in the "[" operation
} # EOF

reFactor <- function(dataset) {
	for (i in 1: ncol(dataset$header)) {
		if (is.factor(dataset$header[,i])) {
			dataset$header[i] <- factor(dataset$header[,i])
		}
	}
	return(dataset)
} # EOF

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
	 return(abs(x - round(x)) < tol)
} # EOF

makePchSingle<- function(PchToReFact, extra = FALSE) {  #PchToReFact: the factor what you want to display with different pch, extra: additional not nice pch
	nr <- length(unique(PchToReFact))
 	if (extra) {
 		nicePch<-c(0:20,35,127,134,135,164,169,171,174,182,187)
 	} else {
 		nicePch<-c(0:20)
 	}
 	if (nr > length(nicePch)){
   		nicePch <- rep(nicePch,ceiling(nr/length(nicePch)))
 	}
 	return(nicePch[PchToReFact])
} # EOF

makePchGroup <- function(PchToReFact, extra = FALSE) {
	nr <- length(unique(PchToReFact))
 	if (extra) {
 		nicePch<-c(0:20,35,127,134,135,164,169,171,174,182,187)
 	} else {
 		nicePch<-c(0:20)
 	}
 	if (nr > length(nicePch)){
   		nicePch <- rep(nicePch,ceiling(nr/length(nicePch)))
 	}
 	return(nicePch[unique(PchToReFact)])
} # EOF

getUniqLevelColor <- function(nrc) {
	if (all(is.numeric(nrc))) {
		return(as.numeric(levels(as.factor(nrc))))
	}
	if (all(is.character(nrc))) {
		return(levels(as.factor(nrc)))
	}
} # EOF

# color_data, color_unique, color_legend, txt, txtE, sumPart, dataGrouping, pch_data, pch_legend
extractColorLegendValues <- function(dataset, groupBy, minPart=NULL, minDistinctVals=NULL, ltyIn=NULL) { # returns a list
	legColLim <- .ap2$stn$plt_lengthLegend_limToCols	# the number when we switch to columns
	maxLengLeg <- .ap2$stn$plt_lengthLegend_truncate	# the maximum legend length
	legNrCols_more <- .ap2$stn$plt_legendMoreCols			# the number of columns in the legend if more than 1
	legCex <- .ap2$stn$plt_legend_standardCex			# the standard cex for the legend
	legCexDim <- .ap2$stn$plt_legend_smallerCex			# the legend cex if smaller
	#
	legNrCols <- 1
	lty <- outIndsLeg <- NULL
	#
	colInd <- which(colnames(dataset$colRep) == groupBy)
	color_data <- dataset$colRep[, colInd]
	ind <- which(colnames(dataset$header) == groupBy)
	grouping <- dataset$header[, ind]
	legendText <- as.character(levels(grouping))
	options(warn=-1)
	nrs <- as.numeric(legendText)
	options(warn=0)
	if (any(is.na(nrs))) {
		lto <- order(legendText) # should be straight from 1 to n, because "level" already gives out characters in alphabetical order
	} else {
		lto <- order(nrs) # so if the legend text is coming from all numbers *and* they are higher than 9 we get the real order to sort later
	}		
	partN <- sapply(levels(grouping), function(x, grAll) length(which(grAll==x)), grAll=grouping)
	sumPart <- sum(partN)
	legendText <- legendText[lto]
	legendTextExtended <- paste(legendText, "   N=", partN[lto], "", sep="") # have it in every line			
	color_unique <- getUniqLevelColor(color_data)  # here read out in levels !!!
#	color_legend <- color_unique[lto] # the old version, appears to be not always correct
	ind <- which(colnames(dataset$colRep) == groupBy) # get once the index of our grouping variable in the colRep
	color_legend <- sapply(legendText, function(x, cri, ds, grby) ssc_s(ds, grby, x)$colRep[1,cri], cri=ind, ds=dataset, grby=groupBy) # look through each of the elements of the legend text and extract the corresponding color from the colRep
	pch_data <- makePchSingle(grouping)
	pch_legend <- as.numeric(levels(as.factor(pch_data)))[lto]
	###
	####### have minimum participants
	if (!is.null(minPart)) {
		aa <- rle(sort(as.character(grouping)))
		ind <- which(aa$lengths < minPart)
		outIndsCol <- outIndLegCol <- NULL
		if (length(ind) > 0 ) {
			for (i in 1: length(ind)) { # because we could have more!!!!
				values <- aa$values[ind[i]]
		#		cat(paste0("kicked out: ", values, "\n"))
				outInds <- sapply(values, function(x) which(grouping == x))
				outIndsLeg <- sapply(values, function(x) which(legendText == x))
				outIndsCol <- c(outIndsCol, outInds)
				outIndLegCol <- c(outIndLegCol, outIndsLeg)
			} # end for i
			outInds <- outIndsCol
			outIndsLeg <- outIndLegCol
			grouping[outInds] <- NA
			grouping <- as.factor(as.character(grouping))
			color_data[outInds] <- NA
			pch_data[outInds] <- NA
			legendText[outIndsLeg] <- NA
			legendTextExtended[outIndsLeg] <- NA
			color_legend[outIndsLeg] <- NA
			pch_legend[outIndsLeg] <- NA
		}
		if (!is.null(ltyIn)) {
			lty <- rep(ltyIn, length(legendText))
			lty <- lty[1: length(legendText)]
			lty[outIndsLeg] <- NA
		}
	} # end !is.null(minPart)
	####
	##### cutting down (truncating) the legend
	le <- length(legendText)
	if (le > legColLim & le < maxLengLeg) {
		legCex <- legCexDim
		legNrCols <- legNrCols_more
	}
	if (le >= maxLengLeg) { # in this case the standared legCex and standard legNrCols stays in place
		half <- round(le/2, 0)
		inds <- c(1, 2, 3, 4, half-1, half, half+1, le-3, le-2, le-1, le) # gets the first, the middle and the last index of things
		color_legend <- color_legend[inds]
		aa <- legendText
		legendText <- c(aa[c(1,2,3)], "...", aa[c(half-1, half, half+1)], "...", aa[c(le-2, le-1, le)])
		aa <- legendTextExtended
		legendTextExtended <- c(aa[c(1,2,3)], "...", aa[c(half-1, half, half+1)], "...", aa[c(le-2, le-1, le)])
	}	
	####
	return(list(color_data=color_data,  color_unique=color_unique, color_legend=color_legend, txt=legendText, txtE=legendTextExtended, sumPart=sumPart, dataGrouping=grouping, pch_data=pch_data, pch_legend=pch_legend, legCex=legCex, legNrCols=legNrCols, ltyNA=lty))
} # EOF

countDecimals <- function(x, nrDec=25) {
	if (!all(is.numeric(x))) {stop()} 
	xRounded <- lapply(x, function(g) round(g, 0:nrDec))
	res <- mapply(function(xR,x) match(TRUE, xR==x), xRounded, x)
	res <- res -1 # to account for the first element what has zero commas
	res[is.na(res)] <- nrDec # as a precaution
	return(res)
} # EOF

readInSpecAreas <- function() {
	out <- as.data.frame(t(getOvertoneWls(.ap2$stn$aqg_OT)))  # getOvertoneWls() is in the file "calc_aqg.r"
return(out)
} # EOF

makeFlatDataFrame <- function(dataset, groupBy, fusionGroupBy=NULL) {
	if (is.null(fusionGroupBy)) {
		colInd <- which(colnames(dataset$header) == groupBy)
		grouping <- dataset$header[, colInd]
		class(grouping) <- "factor" # to get rid of the "AsIs" that, strangely, got smuggled in..
	} else {
		grouping <- fusionGroupBy
	}
	NIR <- as.data.frame(matrix(dataset$NIR, nrow=(nrow(dataset$NIR))))
	out <- cbind(grouping, NIR)
	colnames(out) <- c("grouping", colnames(dataset$NIR))
	rownames(out) <- rownames(dataset)
	return(out)
} # EOF

#' @title Get a single dataset from the 'cube'
#' @description Get a single dataset, referenced by its index, from the 'cube' 
#' object, i.e. the object generated by \code{\link{gdmm}}.
#' @details A valid index from the available range (see the row-names of the 
#' 'cube-object') has to be provided.
#' @param cube An object as created by \code{\link{gdmm}}.
#' @param index The index of the dataset to be obtained. See the leftmost
#' rowname of the 'cube' object.
#' @return A standard dataset as e.g. produced by the function \code{\link{gfd}}.
#' @examples 
#' \dontrun{
#' dataset <- gfd()
#' cube <- gdmm(dataset)
#' # assumes that in the analysis procedure we have a split variable defined.
#' dataset_3 <- getcd(cube, 3)
#' dataset_3
#' str(dataset_3)
#' }
#' @seealso \code{\link{do_msc}}, \code{\link{do_avg}}
#' @family Helper Functions
#' @family Extract Elements
#' @export
getcd <- function(cube, index) {
	return(invisible(cube[[index]]@dataset))
} # EOF

#' @title Get a single model from the 'cube'
#' @description Get a single model, referenced by its index and specified by the
#' argument \code{what} from the 'cube' object, i.e. the object generated by 
#'  \code{\link{gdmm}}.
#' @inheritParams getcd
#' @template mr_getCubeModel
#' @param what Character length one, see details.
#' @section Extracting Vectors:
#' To extract e.g. two loading vectors from a pca-model, you could use a code like 
#' \code{loadingVectors <- pcaModel$loadings$[, c(1,2)]} to extract the first two 
#' loading vectors from the model - see examples.
#' @examples 
#' \dontrun{
#' fd <- gfd()
#' cube <- gdmm(fd)
#' # assumes that in the analysis procedure we have a split variable defined.
#' fd_3_pca <- getcm(cube, 3)
#' str(fd_3_pca)
#' ld12 <- fd_3_pca$model$loadings[, c(1,2)] # extract the first two loadings
#' ld24 <- fd_3_pca$model$loadings[, c(2,4)] # extract loadings 2 and 4
#' fd_2_pls <- getcm(cube, 2, "pls")
#' str(fd_2_pls)
#' }
#' @seealso \code{\link{do_emsc}}, \code{\link{dpt_modules}}
#' @family Helper Functions
#' @family Extract Elements
#' @export
getcm <- function(cube, index, what="pca") {
#	pv_what_models <- c("pca", "sim", "pls")
	pvWhat <- 	pv_what_models
	if (!all(class(what) == "character") | length(what) !=1) {
		stop(paste("Please provide a character of length one to the argument 'what'.\nPossible values are ", paste(pvWhat, collapse=", "), "\n", sep=""), call.=FALSE)
	}
	if (!what %in% pvWhat) {
		stop(paste("Please provide one of ", paste(pvWhat, collapse=", "), " to the argument 'what'.", sep=""), call.=FALSE)
	}
	if (!.hasSlot(cube[[index]], what)) {
		stop(paste("Sorry, the selected ", what, " model is not available.", sep=""), call.=FALSE)
	}
	out <- slot(cube[[index]], what)
	return(out)
} # EOF

# used in showCube
getCubeNrs <- function(cube) {
	nrRows <- nrWls <- NULL
	 for (i in 1: length(cube)) {
	 	ds <- getcd(cube, i)
	 	a <- nrow(ds)
	 	b <- ncol(ds$NIR)
	 	nrRows <- c(nrRows, a)
		nrWls <- c(nrWls, b)
	 }
	return(list(nrRows=nrRows, nrWls=nrWls))
} # EOF

adaptIdStringForDpt <- function(dptSource, prevIdString="") { # returns the new idString; dptSource is an analysis procedure
	limit <- .ap2$stn$gen_plot_maxNrDptInfoOnMain
	##
	origAp <- dptSource #  we should get in the ap containing all the dpt information
	combChar <- msg <- char <- ""
	if (!is.null(origAp)) {
		apo <- origAp$dpt$dptModules
		dptPre <- apo$dptPre
		dptPost <- apo$dptPost
		if (!is.null(dptPost)) {
			dptPost[1] <- paste(";", dptPost[1], sep="") ## add an "|" in front of the first element in dptPost
		}
		combSingle <- c(dptPre, dptPost)
		if (length(combSingle) > limit) {
			restNr <- length(combSingle) - limit
			combSingle <- c(combSingle[1:limit], paste("+", restNr, sep=""))
		}
		combChar <- paste(combSingle, collapse=",")
	} # end if !is.null(origApp)
	if (combChar != "") {
		msg <- "  dpt:"
		char <- combChar
	}
	idStrAdd <- paste(msg, char, collapse="", sep="")
	idStrAdd <- gsub(",;", ";", idStrAdd) # some mistake above, take it out
	idStrNew <- paste(prevIdString, idStrAdd, collapse="", sep="")
	return(idStrNew)
} # EOF

getCheckLegendPosition <- function(xData, yData) {
	defPos <- .ap2$stn$gen_plot_legendPosition
	pvPos <- pv_legendPosition # ("auto", "topleft", "topright", "bottomright", "bottomleft")
	if (!all(is.character(defPos)) | length(defPos) !=1) {
		stop(paste("Please provide a character length one for the default legend position.\n Possible values are ", paste(pvPos, collapse=", "), ".", sep=""), call.=FALSE)
	}
	if (!defPos %in% pvPos) {
		stop(paste("The legend-position '", defPos, "' can not be recognized. \nPlease provide one of '", paste(pvPos, collapse=", "), "' for the default legend position.", sep=""), call.=FALSE)
	}
	if (defPos == pvPos[1]) { # "auto"
		es <- list()
	#	es <- plotrix::emptyspace(xData, yData) # gives back a list with x and y value of the center of the "biggest empty rectangle"
		mer <- plotrix::maxEmptyRect(range(xData), range(yData), xData, yData)
		es$x <- mean(c(mer$rect[1], mer$rect[3]))
		es$y <- mean(c(mer$rect[2], mer$rect[4]))
		if (es$x >= mean(range(xData)) ) {
			xChar <- "right"
		} else {
			xChar <- "left"
		}
		if (es$y >= mean(range(yData)) ) {
			yChar <- "top"
		} else {
			yChar <- "bottom"
		}
		return(paste(yChar, xChar, sep=""))
	} else {
		return(defPos)
	}
} # EOF

checkApsChar <- function(aps) {
	path <- .ap2$stn$fn_metadata
	if (all(aps == "def")) {
		aps <- .ap2$stn$gen_plot_anprocSource
	}
	if (!all(is.character(aps)) | length(aps) != 1) {
		stop("Please provide a length one character to the argument 'aps' resp. the corresponding variable (gen_plot_anprocSource) in 'settings.r', thank you.", call.=FALSE)
	}
	if (aps == "cube") {
		return(aps)
	}
	if (aps == "defFile") {
		fn <- .ap2$stn$fn_anProcDefFile
		ok <- file.exists(paste(path, fn, sep="/"))
		if (!ok) {
			stop(paste("The analysis procedure file \"", fn, "\" does not seem to exist. Please check your input.", sep=""), call.=FALSE)
		}
		return(fn)
	}
	return(aps) # so the only left option is a custom filename, that will be checked later	
} # EOF

#' @title Isolate single wavelength
#' @description Generate a dataset with a single wavelength.
#' @details Provide the wavelength that should remain in the dataset in the 
#' argument \code{wl}. It is not ncecessary to exactly know the desired 
#' wavelength -- if there is no direct match with the wavelength, the next best
#' hit will be taken.
#' @param dataset An object of class 'aquap_data' as produced e.g. by 
#' \code{\link{gfd}}.
#' @param wl Numeric length one. The
#' @param getMax Logical. Set to 'TRUE' to isolate the wavelength with the 
#' highest sum of absorbtion values.
#' @seealso \code{\link{aquap_data-methods}}, \code{\link{selectWls}}
#' @examples
#' \dontrun{
#' fd <- gfd()
#' dataset_single <- siWl(fd, 1300) # to only leave wavelength 1300
#' dataset_single2 <- siWl(fd, getMax=TRUE) # to look for the wavelength with 
#' # highest sum of absorbtion values
#' }
#' @family Extract Elements
#' @export
siWl <- function(dataset, wl, getMax=FALSE) {
	wls <- getWavelengths(dataset)
	if (getMax) {
		ind <- which.max(colSums(dataset$NIR))
	} else {
		ind <- match(wl, wls)		
	}
	if (is.na(ind)) { # so we do not have an exact match
		a <- which(wls > wl)[1]
		wlsBr <- wls[c(a-1, a)] # the two bordering values in the wavelengths
		hit <- wlsBr[ which.min(abs(wlsBr - wl)) ] # get the closer one
		ind <- match(hit, wls)
	}
	cns <- colnames(dataset$NIR)[ind]
	rns <- rownames(dataset$NIR)
	NIR <- dataset$NIR[,ind, drop=FALSE]
	colnames(NIR) <- cns
	rownames(NIR) <- rns
	dataset$NIR <- I(NIR)
	return(dataset)
} # EOF

setCheck_NumericLengthOne <- function(num, char) {
	if (!all(is.numeric(num)) | length(num) != 1) {
		stop(paste0("Please provide a numeric length one to the argument '", char, "' in the settings.r file."), call.=FALSE)
	}
}

doApsTrick <- function(aps, cube, ...) {
	aps <- checkApsChar(aps)
	if (aps == "cube") {
		ap <- getap(.lafw_fromWhere="cube", cube=cube, ...)			 # the ... are here used for additionally modifying (if matching arguments) the analysis procedure obtained from the cube
	} else {
		check_apDefaults(fn=aps)
		ap <- getap(fn=aps, ...) # load from file, possibly modify via ...
	}
	return(ap)
} # EOF

makeColorsTransparent <- function(colVec, alpha=100) {
	mat <- col2rgb(colVec, alpha=TRUE)
	mat[4,] <- alpha
	colVec <- apply(mat, 2, function(x) rgb(x[1], x[2], x[3], x[4], maxColorValue=255))
	return(colVec)
} # EOF

exportAdditionalModelToAp2Env <- function(doExport, thisMod, thisType) {
#	print("----------"); print(str(thisMod)); print(thisType); print(doExport); wait()
	if (doExport) {
		# the initial list is initialized in processSingleRow_CPT
		if (is.null(thisMod)) {
			thisMod <- list(NULL)
		}
		modColName <- pv_extraMods # global variable
		existing <- get(modColName, pos=.ap2)
		typeCol <- c(existing$type, thisType)
		modCol <- c(existing$mod, list(thisMod)) # add the model to the list
		out <- list(type=typeCol, mod=modCol)
 		assign(modColName, out, pos=.ap2) # if from gdmm: we do that in each set
	} # end if do export
	return(invisible(NULL))
} # EOF
