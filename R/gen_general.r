#  @title Show Path to Package Aquap2
#  @description Displays a character string with the path to the packae 
#    'Aquap2'
#  @details This is where you will find the \code{\link{settings_file}}.
#  @return A character string with the path.
#  @family Helper Functions
#  @export
showPathToAquap2 <- function() {
	path <- path.package("aquap2")
	cat("The path to this package is: \n")
	print(path)
	cat("(This is where you will find the settings.r file.\n")
	invisible(path)
} # EOF


copySettingsFile <- function(fromPath, toPath) {
	ok <- file.copy(fromPath, toPath, overwrite=TRUE)
	if (ok) { cat("A fresh version of the settings.r file has been copied from the package.\n")}			
} # EOF

checkSettings <- function() {
	pspath <- paste(path.package("aquap2"), "/settings.r", sep="")
	pathSH <- Sys.getenv("AQUAP2SH")
	if (nchar(pathSH) == 0) { 			## so the variable is *not* defined in .Renviron
		msg <- ("It appears you did not yet define the path to your aquap2 settings.r home directory in the .Renviron file. \nPlease do this by going to the .Renviron file (in your home directory) and there define the variable 'AQUAP2SH' as the path to a folder of your liking. \nRestart R for the changes to become effective. \nSee the help for '?updateSettings' for additional information")
		message(msg)
		return(FALSE)
	} else { ## so we have something defined under AQUAP2SH
		if (!file.exists(pathSH)) {
			msg <- paste("The folder \"", pathSH, "\" does not seem to exist. Please check the path defined in the '.Renviron' file. Thanks.", sep="")
			message(msg)
			return(FALSE)
		}
		sFile <- "/settings.r"
		pathToSettings <- paste(pathSH, sFile, sep="")
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
			loc <- pathToSettings
			pac <- pspath
			fenv <- new.env()
			source(loc, local=fenv)
			locNames <- names(fenv$stn)
			source(pac, local=fenv)
			pacNames <- names(fenv$stn)
			if (!identical(locNames, pacNames)) {
				okInd <- which(pacNames %in% locNames)
				miss <- pacNames[-okInd]
				delInd <- which(locNames %in% pacNames)
				del <- locNames[-delInd]
				msgNew <- "The new variables are:"
				msgDel <- "The following variables have been deleted:"
				#
				message("There appears to be a newer version of the settings.r file in the package \"aquap2\".")
				if (length(miss) != 0 & length(del) == 0) {
					message(msgNew) ;   message(paste(miss, collapse=", "))
				} else {
					if (length(miss) == 0 & length(del) != 0) {
						message(msgDel); 	message(paste(del, collapse=", "))
					} else {
						message(msgNew) ;   message(paste(miss, collapse=", "))
						message(msgDel); 	message(paste(del, collapse=", "))
					}
				}
				message(paste("Do you want to copy it now into \n\"", pathSH, "\" ? \n( y / n )", sep=""))
				a <- readLines(n=1)
				if (a != "y" & a != "Y") {
					message("Please be aware that the package will not work properly if your settings.r file is not up to date.")
					return(TRUE)
				} else {
					copySettingsFile(pspath, pathSH)
					return(TRUE)
				}
			} else { 	# so the variable names in the two settings files are identical
				return(TRUE)
			} 	
		} # end else file exists
	} # end else nchar == 0
} # EOF


#' @title Update aquap2 settings.
#' @description Read in the settings-file in the aquap2-settings home directory 
#' as specified in the .Renviron file
#' @details Must be run first after loading the package, and after every change 
#' in the settings.r file to initialize /reload the settings into the 
#' list called 'stn' in the environment '.ap2'.
#' @section Note: You have to set the path to where you want the settings.r file 
#' to be stored once in your .Renviron file by defining 
#' \code{AQUAP2SH = path/to/any/folder/XX} , with XX being any folder where then the 
#' settings.r file will reside in.
#' @param packageName Character, the name of the package where settings 
#' should be updated. Defaults to "aquap2".
#' @return An (invisible) list with the settings \code{stn} 
#' @family Helper Functions
#' @seealso \code{\link{settings_file}} 
#' @examples
#' \dontrun{
#' updateSettings()
#' str(.ap2$stn)
#' ls(.ap2)
#'}
#' @export
updateSettings <- function(packageName="aquap2") { 
	ok <- checkSettings() # makes sure that we have the latest version of the settings.r file in the settings-home directory defined in .Renviron
	if (ok) {
		pathSettings <- paste(Sys.getenv("AQUAP2SH"), "/settings.r", sep="")
		sys.source(pathSettings, envir=.GlobalEnv$.ap2)
	#	if (any(grepl(".ap2", search(), fixed=TRUE))) {
	#		detach(.ap2)
	#	}
	#	attach(.ap2)
		cat(paste(packageName, "settings updated\n"))
		invisible(.ap2$stn)
	}
} # EOF
