###################################
## introducing uniset and the class structure via xlsx
checkTransitToUniset_MaybeRenameSettingsFile <- function() {
	# 1) rename the settings.R file into "aquap2_settings.R"
	settingsHome <- Sys.getenv("AQUAP2SH")
	renameInto <- "aquap2_settings.R"
	if (!file.exists(paste0(settingsHome, "/", renameInto))) {	
		aa <- try( {
			if (file.exists(paste0(settingsHome, "/settings.R")) ) {
				file.rename(from=paste0(settingsHome, "/settings.R"), to=paste0(settingsHome, "/", renameInto))
				message(paste0("It seems you are updating from an older version of 'aquap2':\n   Your settings file in the folder '", settingsHome, "' has been renamed into \n   '", renameInto, "'.\n"))
			} # end if
		} , silent=TRUE) # end try
		#
		if (is(aa, "try-error") | settingsHome=="") {
			stop(paste0("You might have to run 'ap2_settings_setup'."), .call=FALSE)
		} # end if	
	} # end if	
} # EOF

checkTransitTo_SLxlsx_MaybeMakeMetadataBackup <- function(fn, stn) {
	# ) make a backup copy of the old metadata file (as the L1 and L2 will be deleted)
	mdFolder <- stn$fn_metadata
	##
	aa <- try( {
		mdf <- paste0(mdFolder, "/", fn)
		if (file.exists(mdf)) {
			bb <- new.env()
			sys.source(mdf, bb)
			if (exists("L1", bb)) {
				file.copy(from=mdf, to=paste0(mdFolder, "/", fn, "_old.R"))
				message(paste0("It seems you are updating from an older version of 'aquap2': \nThe class-structure is now designed via an Excel file and not within the metadata file any more.\n   A local copy (ending in '_old.R') of the file '", fn, "' has been created in the folder '", mdFolder, "'.\n"))
			} # end if
		} # end if
	}, silent=TRUE ) # end try
#	if (is(aa, "try-error")) {
#		# do nothing for now
#	} # end if
} # EOF
###################################
