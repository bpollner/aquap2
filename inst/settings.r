###########################################################################################
######################## Settings file for package "aquap2" #########################
###########################################################################################

stn <- list(
	# tag = value,
	
	
	## Folder Management
	fn_analysisData = "anData",				## the folder name for storing any analysis data
	fn_exports = "exports", 				## the folder name for any exported file
	fn_rcode = "R-code", 					## the folder where all the r-code is kept
	fn_rawdata = "rawdata", 				## the folder where you put the raw-data from your data acquisition
	fn_rdata = "R-data", 					## the folder where R is keeping all its raw-data after importing from the data-acquisition format
	fn_metadata = "metadata", 				## the folder where you put the metadata and the analysis procedure r-files
	fn_results = "results",					## the folder where all the result-pdfs get saved
	fn_sampleLists = "sampleLists", 		## the folder for the sample lists used in randomizing the samples and for importing the raw-data


	## General behaviour
	allSilent = FALSE,						## if false, "status" messages will be displayed


	### smoothing ###
	## settings for the Sav. Golay filter at the smoothing process
	savGolayOrder_p = 2, 					## default is 3
	savGolayLength_n = 25,					## default is 5
	savGolayDeriv_m = 0, 					## default is 0



	##
	last = 0
	## the last one without comma !!
) # end of list
