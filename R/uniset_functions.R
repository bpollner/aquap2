######################################################################
####### Custom-tailored functions for package 'aquap2' ######
######################################################################



#######################
# The following functions
# checkOnTest() and
# test_getLocalStn()
# are custom-tailored by package 'uniset', and are required for the package
# 'aquap2' to be able to run tests. (Otherwise the non-existent
# system-variable 'AQUAP2SH' in a remote test-run would be a problem.)

checkOnTest <- function() {
	if (exists("get_settings_from_aquap2_package_root", where = .GlobalEnv)) {
		if (get("get_settings_from_aquap2_package_root", pos = .GlobalEnv)) {
			return(TRUE)
		} else {
			return(FALSE)
		}
	} else {
		return(FALSE)
	}
} # EOF

checkOnLocal <- function() {
	ptp <- path.package("aquap2")
	if (dir.exists(paste0(ptp, "/inst"))) { # so we are in "local" mode, running the package maybe via devtools::load_all
		return(TRUE)
	} else {
		return(FALSE)
	} # end else
} # EOF

test_getLocalStn <- function() {
	ptp <- path.package("aquap2")
	# set up the stn object
	if (dir.exists(paste0(ptp, "/inst"))) { # to be able to run tests manually line by line
			stn <- source(paste0(ptp, "/inst/aquap2_settings.R"))$value
		} else {
			stn <- source(paste0(ptp, "/aquap2_settings.R"))$value
	} # end else
	return(stn)
} # EOF

#######################

# The following functions
# updateSettings(), 
# autoUpS() and
# getstn() 
# are custom-tailored by package 'uniset', and
# intended to be used inside functions defined in
# the package 'aquap2':

# Can be used inside a function of the package 'aquap2' to manually
# update the settings. If silent=FALSE, upon success a message
# will be displayed.



#' @title Perform Setup
#' @description Is performing the required setup regarding the settings.R file. 
#' Only has to be called once, but can be called repeatedly to conveniently 
#' change the location of the settings-home folder.
#' @details If argument \code{path} is left at its default \code{NULL}, the path 
#' to the desired folder can be selected interactively. 
#' @param path Character length one. The path to the folder where the settings-
#' home folder should be located. Defaults to \code{NULL}.
#' @param path Character length one, holding the path to the location where the 
#' folder containing the settings.R file should be located. Defaults to 
#' \code{NULL}. The path has to be given either in forward-slashes 
#' (\code{/Users/Name/aFolder} on Unix machines and \code{C:/Users/Name/aFolder} 
#' on windows machines), or in double back-slashes 
#' \code{C:\\\\Users\\\\Name\\\\aFolder} on windows machines.
#' If left at the default \code{NULL}, the location should be selectable interactively.
#' @return Called for its side-effects, i.e. performing the required setup 
#' regarding the settings.R file. 
#' @family Helper Functions
#' @export
ap2_settings_setup <- function(path=NULL) {
	uniset::uniset_setup(where = path, uniset_handover)	
} # EOF

#' @title Update Settings
#' @description Is first updating the settings.R file itself, and then is reading 
#' in the key=value pairs from the setting.R file.
#' @param silent Logical, defaults to FALSE.
#' @return An (invisible) list with the key=value pairs from the settings.R 
#' file.
#' @family Helper Functions
#' @export
updateSettings <- function(silent=FALSE) {
	checkTransitToUniset_MaybeRenameSettingsFile() # is renaming the settings.R file
	if (checkOnTest() | checkOnLocal()) {
		return(invisible(test_getLocalStn() ))
	} # end if
	#
	stn <- try(uniset::uniset_updateSettings(get("uniset_handover"),
		setupFunc="ap2_settings_setup", silent), silent=TRUE)
	if (is(stn, "try-error")) {
		stop(gl_errMsgUniset, call.=FALSE)
	} # end if
	return(invisible(stn))
} # EOF

#' @title Get settings object
#' @description Is reading in the key=value pairs from the setting.R file.
#' @return An (invisible) list with the key=value pairs from the settings.R 
#' file.
#' @family Helper Functions
#' @export
getstn <- function() {
	if (checkOnTest() | checkOnLocal()) {
		return(test_getLocalStn())
	} # end if
	#
	stn <- uniset::uniset_getstn(get("uniset_handover"))
	if (is.null(stn)) {
		# gets returned as NULL if the file could not be sourced, that means
		# 'updateSettings()' has not been called yet. Hence, we have to force the
		# manual update here.
		checkTransitToUniset_MaybeRenameSettingsFile()		
		stn <- try(uniset::uniset_updateSettings(get("uniset_handover"),
			setupFunc="ap2_settings_setup", silent=TRUE), silent=TRUE)
		if (is(stn, "try-error")) {
			stop(gl_errMsgUniset, call.=FALSE)
		} # end if
	} # end if
	return(invisible(stn))
} # EOF


# Can be used inside a function of the package 'aquap2' to
# automatically update the settings. No message will be displayed
# upon success.
autoUpS <- function(cfs=getstn()$defCfs) {
	if (cfs) {	
		checkForExperimentFolderStructure()
	} # end if
	#
	checkTransitToUniset_MaybeRenameSettingsFile() # is renaming the settings.R file 
	#
	if (checkOnTest() | checkOnLocal()) {
		return(invisible(test_getLocalStn()))
	} # end if
	#
	stn <- try(uniset::uniset_autoUpS(get("uniset_handover"),
		setupFunc="ap2_settings_setup"), silent=TRUE)
	if (is(stn, "try-error")) {
		stop(gl_errMsgUniset, call.=FALSE)
	} # end if		
	return(invisible(stn))
} # EOF


#' @title Test for Uniset
#' @description Is testing if the uniset integration is set up correctly.
#' @return A list of length 4.
#' @export
testUniset <- function() {
	uniset::uniset_test(uniset_handover)
} # EOF

#######################

# The following function is required to hand over information to package
# "uniset". If you are not using Roxygen, delete the Raxygen-code (starting
# with "#'" and make sure that the function is exported into the namespace of
# package 'aquap2'.


#' @title Handover to Uniset
#' @description Is handing over values to package uniset. These values are
#' required for the correct functioning of the dynamic settings file system
#' provided by \code{\link[uniset]{uniset}}.
#' @export
aquap2_handover_to_uniset <- function() {
	return(list(pkgUniset_UserPackageName = "aquap2",
				pkgUniset_RenvironSettingsHomeName = "AQUAP2SH",
				pkgUniset_SettingsObjectName = "settings",
				pkgUniset_SuffixForTemplate = "_TEMPLATE"
				))
} # EOF

#######################
