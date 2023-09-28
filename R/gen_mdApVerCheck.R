# functions to check and maybe modify (delete keys or add keys) the metadata or analysis procedure file.access
# modeled after the code used in uniset

getCheckForDoubleNames <- function(pathToLocal, pathToPack, pmu, folderLocal, nameLocal) {
    #
    checkIt <- function(charVec, what) {
        aa <- length(charVec)
        bb <- length(unique(charVec))
        if (bb < aa) {
            rl <- rle(sort(charVec)) # but they already come in sorted *
            ind <- which(rl$lengths > 1)
            vals <- rl$values[ind]
            stop(paste0("Sorry, the keys '", paste0(vals, collapse="', '"), "' ", "seem to appear more than once in the ", what, "settings.R file."), call.=FALSE)
        } # end if
    } # EOIF
    ##
    lenv <- new.env()
    aa <- try(sys.source(pathToLocal, envir=lenv), silent=TRUE)
	if (is(aa, "try-error")) {
		message(paste0("An error occurred while trying to source content from the file \n'", pathToLocal, "' in the folder '", folderLocal, "'."))
		cat("Probably some unsavoury punctuation (e.g.',' or '=') got introduced at some point... by someone other than you, of course :-).\nA template with correct syntax will be made available.\n")
		pleaaseCopyAsTemplate(pathToPack, folderLocal, nameLocal)
		stop(call.=FALSE)
	} # end if 
    txt <- paste0("sort(names(lenv", pmu, "))") # *
    locNames <- eval(parse(text=txt))
    penv <- new.env()
    sys.source(pathToPack, envir=penv)
    txt <- paste0("sort(names(penv", pmu, "))") # *
    pacNames <- eval(parse(text=txt))
    #
    checkIt(locNames, "local ")
    checkIt(pacNames, "package ")
    #
    return(list(locNames=locNames, pacNames=pacNames))
} # EOF

getKeyTxtCenter <- function(pac, taPaObj, lastChar) {
	fconPack <- file(pac, open="r")
	ftPack <- readLines(fconPack)   # read in the pac file
	close(fconPack)
	indPm <- which(startsWith(trimws(ftPack), taPaObj)) # on which line number is the "stn" object
	indLast <- which(startsWith(trimws(ftPack), lastChar))
	return(ftPack[indPm:indLast])
} # EOF

getKeysOnlyFromText <- function(ftXX, splitChar) {
    ftXXr <- unlist(lapply(strsplit(trimws(ftXX), splitChar), function(x) trimws(x[1])))
    ftXXr[is.na(ftXXr)] <- "" # replace NAs with empty character. NAs were those that did not have a key = value pair. (--> an "<-")
	ind <- which(ftXXr == "")
    return(ftXXr)
} # EOF

cleanOutLastEmptiesFromftLocalR <- function(locNames, ftLocalR) {
	ind <- which(ftLocalR == locNames[length(locNames)])
	if (length(ftLocalR) > ind) {
		ftLocalR <- ftLocalR[-((ind+1):(length(ftLocalR)))]
	}
	return(ftLocalR)
} # EOF

getMissingKVPs <- function(ftPack, ftPackR, missingKeys) {
	out <- character(length(missingKeys))
    for (i in 1: length(missingKeys)) {
        out[i] <- ftPack[which(ftPackR == missingKeys[i])]
    } # end for i
    return(out)
} # EOF

getTxtsBetweenLocal <- function(ftLocal, ftLocalR, indLocHook, indLocNext, ftPack, ftPackR, indKey, maxS) {
#   indKey is in ftPack/R dimension
#   txtBetweenLocal: tbl
#   txtBetweenLocalUpper: tbl_U
#   txtBetweenLocalLower: tbl_L
    #
    if (indLocHook+1 == indLocNext | indLocHook == indLocNext) { # so there is no between text, or it is the last
        return(list(tbl=NULL, tbl_U=NULL, tbl_L=NULL)) # direct insertion between two adjacent lines
    } # end if
    #
    tbl <- ftLocal[(indLocHook+1):(indLocNext-1)]
    ######
    indPacHook <- which(ftPackR == ftLocalR[indLocHook])
    if (indPacHook+1 == indKey) { # there is no space above; the new key is directly below the hook
        return(list(tbl=tbl, tbl_U=NULL, tbl_L=tbl))
    } # end if
    #
    indPacNext <- which(ftPackR == ftLocalR[indLocNext])
    if (indPacNext-1 == indKey) { # there is no space below, the new key is directly above the next
        return(list(tbl=tbl, tbl_U=tbl, tbl_L=NULL))
    } # end if
    #######
    if (all(trimws(tbl) == "")) {
        return(list(tbl=tbl, tbl_U=tbl, tbl_L=NULL)) # classic case for new block
    } # end if
    if (all(trimws(tbl) != "")) {
        return(list(tbl=tbl, tbl_U=rep("", maxS), tbl_L=tbl)) # can be either or (top or bottom, no way to know). But we are "somewhere in the middle", so insert empty lines above
    } # end if
    # by now tbl has to be longer than 1
    # and by now the new key HAS to be "somewhere in the middle"
    indFirstSpace <- which(trimws(tbl) == "")[1] # so the text could belong more to below
    if (indFirstSpace == 1) {
        return(list(tbl=tbl, tbl_U=rep("", maxS), tbl_L=tbl))
    } # end if
    #
    tbl_U <- tbl[1:(indFirstSpace-1)]
    tbl_L <- tbl[indFirstSpace: length(tbl)]
    return(list(tbl=tbl, tbl_U=c(tbl_U, rep("", maxS)), tbl_L=tbl_L)) # because we have to be "in the middle" somewhere
} # EOF

getTextBetweenPac <- function(pacNames, singleMissingKey, ftPackR, ftPack, indKey, ftLocal, onDEV=FALSE) {
    pacHookAbove <- pacNames[which(pacNames == singleMissingKey)-1]
	# if (onDEV) {print("pacHookAbove: "); print(pacHookAbove); print("---------");}
    indPacHook <- which(ftPackR == pacHookAbove)
    keyIndFT <- which(ftPackR == singleMissingKey)
    if (indPacHook == (keyIndFT-1)) { # so there is no space between the two keys
        return(NULL)
    } # end if
    #
    txtBetweenPac <- ftPack[(indPacHook+1):(indKey-1)]
	# if (onDEV) {print("txtBetweenPac Raw: "); print(txtBetweenPac); print("---------");}

    # cut away everything that is above an empty line
    txtT <- trimws(txtBetweenPac)
    if (all(txtT == "")) {
        return(NULL)
    } # end if
    indEmpty <- which(txtT == "")
    if (length(indEmpty) != 0) {
	#	aa <- max(indEmpty)    # get the index of the last empty line
		aa <- min(indEmpty)    # get the index of the first empty line
    } else { # so it is 0, we have no empty line in the text
    	aa <- 0
    } # end else
    if (aa == length(txtBetweenPac)) { 
        return(NULL)
    } # end if
    out <- txtBetweenPac[(aa+1):(length(txtBetweenPac))]
    # if (onDEV) {print("txtBetweenPac selected: "); print(out); print("---------");}
    ind <- which(out %in% ftLocal)
    if (length(ind) != 0) {
      #  out <- out[-ind] # make sure that we do not copy anything that is already in the local file
    } # end if
    if (length(out) == 0) {
        out <- NULL # just to be sure
    } # end if
    return(out)
} # EOF

getTxtEmptyBelowPacKey <- function(indKey, pacNames, ftPack, ftPackR, ftLocal, onDEV=FALSE) { # indKey is in ftPackR-dimension
    ind <- which(pacNames == ftPackR[indKey])
    if (ind == length(pacNames)) {
    	aa <- ind
    } else {
    	aa <- ind+1
    } # end else
    indPacNext <- which(ftPackR == pacNames[aa])
    ##    
    if (indKey+1 == indPacNext) { # so there is no space between
        return(NULL)
    } # end if
    if (indKey == indPacNext) {
    	return(NULL)
    } # end if 
    ##
    txtPackBelowKey <- ftPackR[(indKey+1):(indPacNext-1)]
    txtPackBelowKey_Full <- ftPack[(indKey+1):(indPacNext-1)]
	##
    if (all(txtPackBelowKey == "")) {
        return(txtPackBelowKey)
    } # end if
    if (all(txtPackBelowKey != "")) {
        return(NULL) # that means, if in the pack the two keys above and below are all connected with #txt, nothing is copied.
    } # end if
    rl <- rle(txtPackBelowKey == "")
    if (rl$values[1]) { # we have some empty lines as first block
        return(rep("", rl$lengths[1]))
    } else {
        txt <- txtPackBelowKey_Full[1:rl$lengths[1]] # first get the characters
        aa <- which(txt %in% ftLocal)
        if (length(aa) > 0) {
			txt <- txt[-aa]        
        } # end if
        empty <- rep("", rl$lengths[2]) # then get the empty lines
        out <- c(txt, empty)
        return(out)
    } # end else
    return("\t# something went wrong... sorry...") # we should never get here
} # EOF

getMaxSpace <- function(ftLocal) {
    aa <- trimws(ftLocal)
    rl <- rle(aa=="")
    if (length(rl$values) == 1 & all(rl$values == FALSE)) {
    	return(0)
    } # end if
    out <- max(rl$lengths[rl$values])
    if (out < 1) { # should not happen any more
    	out <- 0
    } # end if
    return(out)
} # EOF

reduceEmptyLines <- function(ftLocal, maxS, maxSD, manMax=NULL) { # maxSD is after deleting and adding (is the higher one); manMax a possible manually provided max value
    if (!is.null(manMax)) {
	   	maxS <- manMax
    } # end if
    # find the indices where the too big space maxSD is
    if (maxSD > maxS) { # so we have to cut down on big empty spaces
        ftT <- trimws(ftLocal)
        rl <- rle(ftT == "")
        aboveSIndRL <- which(rl$lengths > maxS & rl$values)
        indDelOut <- NULL
        for (i in 1: length(aboveSIndRL)) {
            ind <- aboveSIndRL[i]
            thisLeng <- rl$lengths[ind]
            cutAwayLeng <- thisLeng - maxS
            maxIndTxt <- sum(rl$lengths[1:ind])
            minIndTxt <- maxIndTxt - cutAwayLeng +1
            indDelOut <- c(indDelOut, (minIndTxt : maxIndTxt)) # cut down delete to the max empty lines of before deletion
        } # end for i
        return(ftLocal[-indDelOut])
    } else {
    return(ftLocal) # nothing to cut away, return the original
    }
} # EOF

tellKeyAddDelete <- function(keys, folderLocal, nameLocal, what="add") {
    if (what == "add") {
        whatTxt <- " added to"
    } else {
        whatTxt <- " deleted from"
    }
    if (length(keys) > 1) {
        plS <- "s"; plC <- " were"
    } else {
        plS <- ""; plC <- " was"
    }
    cat(paste0("The following ", length(keys), " key", plS, plC, whatTxt, " the file '", nameLocal, "' in \nthe folder '", folderLocal, "':\n\t"))
    message(paste0(keys, collapse=", "), "\n")
} # EOF

adaptSort_X_Names <- function(xNames, ftXR) {
	# ftXR holds every line including hashtags in the correct order
	# xNames holds only the names, but the order is scrambled --> we are using the xNames to get the indices of what to delete from ftXR.
	indNoKey <- which(!ftXR %in% xNames)
	if (length(indNoKey) > 0) {
		ftXR <- ftXR[-indNoKey] # clears out everything that is not a key	
	} # end if
	return(ftXR) # gets the new corrected xNames	
} # EOF

addMissingKeys <- function(ftLocal, splitChar, taPaObj, pathToPack, folderLocal, nameLocal, pacNames, locNames, maxS, lastCharPack, manMax=NULL) {
	DEV <- FALSE
  #	DEV <- TRUE
    missingKeys <- pacNames[which(!pacNames %in% locNames)]
	# if (onDEV) {print("Missing Keys: "); print(missingKeys);}
    if (length(missingKeys != 0)) { # so we do have to add something
        ftLocalR <- getKeysOnlyFromText(ftLocal, splitChar)
        ftLocalR <- cleanOutLastEmptiesFromftLocalR(locNames, ftLocalR) # because, after deletion it might be possible that we have some "" empty chars on the end. Things are complicated here... 
        # if (onDEV) {print("FIRST ftLocalR"); print(ftLocalR);}
        ftPack <- getKeyTxtCenter(pathToPack, taPaObj, lastCharPack)
        ftPackR <- getKeysOnlyFromText(ftPack, splitChar)
        pacNames <- adaptSort_X_Names(pacNames, ftPackR)
        locNames <- adaptSort_X_Names(locNames, ftLocalR)
        # if (onDEV) {print("FIRST locNames: "); print(locNames); print("---------");}
        missingKVPs <- getMissingKVPs(ftPack, ftPackR, missingKeys) # all the missing key-values pairs in one object
        for (i in 1: length(missingKeys)) {
		# if (onDEV) {print("Working on Key: "); print(missingKeys[i]) ; print("---------");}
            indKey <- which(ftPackR == missingKeys[i])  # the pac index of a key missing in loc
            if (length(indKey) > 1) {stop("Sorry, it seems that a key name appears twice.", call.=FALSE)}
            newKVP <- missingKVPs[i]

            # now look up for the next hook
            pacHook <- pacNames[which(pacNames == missingKeys[i])-1]  ## XXX mod here if the first is missing !!!
            locHookKeyInd <- which(locNames == pacHook)
            locHook <- locNames[locHookKeyInd] # the name of the key in loc one higher than the missing one
           	# if (onDEV) {print("pacHook:"); print(pacHook); print("---------"); print("locNames: "); print(locNames); print("locHook:"); print(locHook); print("---------");}
			
            # get the comments above and empty spaces below the pacHook (if there are any)
            txtBetweenPack <- getTextBetweenPac(pacNames, missingKeys[i],ftPackR, ftPack, indKey, ftLocal, onDEV=DEV)
            txtEmptyBelowPacKey <- getTxtEmptyBelowPacKey(indKey, pacNames, ftPack, ftPackR, ftLocal, onDEV=DEV)
			# if (onDEV) {print("txtBetweenPack: "); print(txtBetweenPack); print("txtEmptyBelowPacKey: "); print(txtEmptyBelowPacKey); print("---------");}

            # get all local lines between hook and next
            indLocHook <- which(ftLocalR == locHook) # next higher hook in local file !!
           	if (indLocHook == length(ftLocalR)) { # so it is the last, one below last is missing
           		indLocNext <- indLocHook
           	} else {
				locNext <- locNames[which(locNames == locHook)+1] # the name of the next key present in the local file           	
				indLocNext <- which(ftLocalR == locNext)
           	} # end else   
			# if (onDEV) {print("indLocHook");print(indLocHook); print("indLocNext");print(indLocNext); print("---------")}
            aa <- getTxtsBetweenLocal(ftLocal, ftLocalR, indLocHook, indLocNext, ftPack, ftPackR, indKey, maxS) # here it is decided where in the between text the new KVP is put
                txtBetweenLocal <- aa$tbl
                txtBetweenLocalUpper <- aa$tbl_U
                txtBetweenLocalLower <- aa$tbl_L
            txtUpper <- ftLocal[(1):(indLocHook)] # !!!!! changes this if pacHook == taPaObj.
            if (indLocNext == indLocHook) {
            	txtLower <- NULL
            } else {
				txtLower <- ftLocal[(indLocNext):length(ftLocal)]            
            } # end else
#           print(txtUpper); print("---------");print(txtLower); print("---------")
            #
            # put together the text, add to locNames etc. as well.
            ftLocal <- c(txtUpper, txtBetweenLocalUpper, txtBetweenPack, newKVP, txtEmptyBelowPacKey, txtBetweenLocalLower, txtLower) ## CORE ##
            ftLocalR <- getKeysOnlyFromText(ftLocal, splitChar)
			if (indLocNext == indLocHook) {
				locNames <- c(locNames[1:locHookKeyInd], missingKeys[i])
			} else {
				locNames <- c(locNames[1:locHookKeyInd], missingKeys[i], locNames[(locHookKeyInd+1):length(locNames)])			
			} # end else
            ftLocalR <- cleanOutLastEmptiesFromftLocalR(locNames, ftLocalR)
			# if (onDEV) {print("New locNames: ");  print(locNames); print("---------"); print("--------xxxxxxxx-x-x-x-x-xxxxxxxxxxxxxxxxxx------");}
			# if (onDEV) {print("New ftLocalR: "); print(ftLocalR); print("---------");}
            # if (onDEV) {
		#		print(txtUpper)
		#		print(locHook)
		#		print(txtBetweenLocalUpper)
		#		print(txtBetweenPack)
		#		print(newKVP)
		#		print(txtEmptyBelowPacKey)
		#		print(txtBetweenLocalLower)
		#		print(locNext)
		#		print(txtLower)
				#
#				print("---------------------------")
#				print(ftLocal)
#				print("---------------------------")
#		#		wait()
#        		} # end if TRUE  # dev helpers, is printing things.
        } # end for i (going through missing keys)
    	maxSD <- getMaxSpace(ftLocal) # the max space after adding keys
      	ftLocal <- reduceEmptyLines(ftLocal, maxS, maxSD, manMax)
        #
        tellKeyAddDelete(missingKeys, folderLocal, nameLocal, what="add")
    } ########### end if length(missingKeys) != 0 # until here, things were added. OR not.
    return(ftLocal)
} # EOF

deleteSurplusKeys <- function(folderLocal, nameLocal, ftLocal, splitChar, taPaObj, locNames, pacNames, maxS, manMax=NULL) {
   DEV <- FALSE
#  DEV <- TRUE
   #
    surplusKeys <- locNames[which(!locNames %in% pacNames)]
    # if (onDEV) {print("surplusKeys"); print(surplusKeys); print("---------");}
    if (length(surplusKeys != 0)) { # so we do have to delete something
        ftLocalR <- getKeysOnlyFromText(ftLocal, splitChar) # the incoming ftLocal is "stn" only, and was possibly modified above in the additions
        # if (onDEV) {print("Initial ftLocalR: "); print(ftLocalR); print("---------");}
   #    # if (onDEV) {print("Initial ftLocal: "); print(ftLocal); print("---------");}
        for (i in 1: length(surplusKeys)) { # we are collecting possible single line comments above a block
            indKey <- which(ftLocalR == surplusKeys[i])
            # if (onDEV) {print("indKey (local del.): "); print(indKey); print("working on: "); print(surplusKeys[i]); print("---------");}
            aa <- trimws(ftLocal) # so that tabs etc go to ""
            # if (onDEV) {print("ftLocal trimmed: "); print(aa); print("---------");}
            ikm <- NULL
            min1 <- indKey -1 ; if (min1 < 1) {min1 <- 1}
            min2 <- indKey -2 ; if (min2 < 1) {min2 <- 1} # so we can delete also on second place
            if ( (aa[min1] != "")  & (aa[min2] == "") ) { # that means we have a single line of comment above a key
                ikm <- indKey-1 # ikm: index key minus
                if (ftLocalR[ikm] %in% locNames) { # now this above could be a key, check....
                    ikm <- NULL
                } # end if
                if (ftLocalR[indKey+1] %in% locNames) { # means we have an other key directly below the one to be deleted, so we will *not* delete the one comment line above
                    ikm <- NULL
                } # end if
            } # end if
            indDel <- c(indKey, ikm)
            ftLocal <- ftLocal[-indDel] # delete here #### ******************
            ftLocalR <- getKeysOnlyFromText(ftLocal, splitChar)
        } # end for i going through surplusKeys
        #
        maxSD <- getMaxSpace(ftLocal) # the max space after deleting keys
        ftLocal <- reduceEmptyLines(ftLocal, maxS, maxSD, manMax)
        #
        tellKeyAddDelete(surplusKeys, folderLocal, nameLocal, what="delete")
        ind <- which(locNames %in% surplusKeys)
        locNames <- locNames[-ind]
    } # end if (length(surplusKeys != 0))
    return(list(ftLocal=ftLocal, locNames=locNames))
} # EOF

pleaaseCopyAsTemplate <- function(pathToPack, folderLocal, nameLocal) {
	# pathToPack comes in "inst-test" corrected; it contains the complete path to the metadata or anproc file in the package. 
	# nameLocal does contain the .R at the end
    tmpl <- "_TEMPLATE.R"
    #
	localPath <- paste0(folderLocal, "/", nameLocal, tmpl) # nameLocal does have the .R already
    ok <- file.copy(from=pathToPack, to=localPath, overwrite = TRUE) # 
    if (!ok) {
        message("Sorry, for unknown reasons the required template file could not be copied.\n")
        return(invisible(FALSE))
    } else {
    message(paste0("The file '", paste0(nameLocal, tmpl), "' has been copied into \n'", folderLocal, "'\n"))
    return(invisible(TRUE))
    } # end else
} # EOF

checkFileVersionPossiblyModify <- function(pathToPack, folderLocal, nameLocal, pm=NULL, tmpl, manMax=NULL){
	# pm comes in either as the first key of the metadata or the first key of the analysis procedure
    # pathToPack comes in "inst-corrected", so it is test-safe
    pv_suffixForTemplates <- tmpl
    taPaObj <- pm
    splitChar <- "<-"
    pmu <- ""
    #
    loc <- pathToLocal <- paste0(folderLocal, "/", nameLocal)
    pac <- pathToPack
    #
    aa <- getCheckForDoubleNames(pathToLocal, pathToPack, pmu, folderLocal, nameLocal) # is checking for non-unique keys
    if (identical(aa$locNames, aa$pacNames)) {
        return(invisible(TRUE))
    } # end if identical

    # we only continue, if the locNames and pacNames are NOT identical

    ######## first we will ADD any possible keys
    # get the name of the keys that are missing in local / added in pathToPack
    lenv <- new.env()
    sys.source(pathToLocal, envir=lenv)
    txt <- paste0("names(lenv", pmu, ")") # NOT sorted
    locNames <- c(eval(parse(text=txt))) # add taPaObj as first in case of a first key is introduced. Need a hook then.
    penv <- new.env()
    sys.source(pathToPack, envir=penv)
    txt <- paste0("names(penv", pmu, ")") # NOT sorted
    pacNames <- c(eval(parse(text=txt))) # but the order is scrambled now !    
    #
    #get parts before and after the list (TaPaPbj)
    fconPack <- file(pathToPack, open="r")
    ftPack <- readLines(fconPack)   # read in the pac file
    close(fconPack)
    fconLocal <- file(pathToLocal, open="r")
    ftLocal <- ftLocalBackup <- readLines(fconLocal)   # read in the local file
    close(fconLocal)
    ###
    ftLocalR <- getKeysOnlyFromText(ftLocal, splitChar)
    locNames <- adaptSort_X_Names(locNames, ftLocalR)
    lastCharLocal <- locNames[length(locNames)] # gives the last of the locNames
    #
    ftPackR <-  getKeysOnlyFromText(ftPack, splitChar)
    pacNames <- adaptSort_X_Names(pacNames, ftPackR)
    lastCharPack <- pacNames[length(pacNames)]
    ###
    indPm <- which(startsWith(trimws(ftLocal), taPaObj)) # on which line number is the first key object
    indLast <- which(startsWith(trimws(ftLocal), lastCharLocal))
    if (indPm == 1) {
    	txtAbove <- ""
    } else {
		txtAbove <- ftLocal[1:(indPm-1)] # get the txtAbove and txtBelow from the local file. The user could have written something in there that should stay.
    } # end else
    if (indLast == length(ftLocal)) {
		txtBelow <- ""
    } else {    
		txtBelow <- ftLocal[(indLast+1):length(ftLocal)] 
    } # end else
    #
    ftLocal <- getKeyTxtCenter(pathToLocal, taPaObj, lastCharLocal) # might need that in the deletions. !!! gets possibly modified in the additions below
    maxS <- getMaxSpace(ftLocal) # get the maximum number of continuous empty lines
    #######  
    msgAd <- "not "
    aa <- try(deleteSurplusKeys(folderLocal, nameLocal, ftLocal, splitChar, taPaObj, locNames, pacNames, maxS, manMax), silent=TRUE) # *************** # usually does not crash
    if (!is(aa, "try-error")) {    
        ftLocal <- aa$ftLocal
        locNames <- aa$locNames # locNames have to updated due a possible deletion of keys
		ftLocal <- try(addMissingKeys(ftLocal, splitChar, taPaObj, pathToPack, folderLocal, nameLocal, pacNames, locNames, maxS, lastCharPack, manMax), silent=TRUE) # ************** loves to craash
    	msgAd <- "not completely "
    } # end if 
    #######
    # now write into local file if also adding did not produce an error
    if (!is(ftLocal, "try-error")) {
		fconLocal <- file(loc, open="w")
		writeLines(c(txtAbove, ftLocal, txtBelow), fconLocal) # write the new file to settings.r in pathSH. # If deleting already fails, the original file gets written back.
		close(fconLocal)
    } # end if
    #
    # just to be sure, check again
    aa <- getCheckForDoubleNames(pathToLocal, pathToPack, pmu)
    if (!identical(aa$locNames, aa$pacNames)) { # can happen if things fail above
        message(paste0("We are very sorry, but the keys in the file '", nameLocal, "' in the folder '", folderLocal, "' could ", msgAd, "be updated."))
    	  	fconLocal <- file(loc, open="w")
		   	writeLines(ftLocalBackup, fconLocal) # just to be sure; write the original backup file to the file
    	   	close(fconLocal)
        cat(paste0("A template containing the required key-value pairs will be made available.\n"))
        pleaaseCopyAsTemplate(pathToPack, folderLocal, nameLocal)
        return(invisible(FALSE))
    } # end if !identical
    #
    return(invisible(TRUE))
} # EOF

