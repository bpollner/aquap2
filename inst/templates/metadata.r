####################################################################
#################### aquap2 metadata file ##########################
####################################################################

### do NOT change the names of the variables !!  ####

	
	expName <- "KCl-Dilutions"				## the name of the experiment

	filetype <- "def"						## the filetype of the raw-data file. Leave at "def" to get the default from the settings (variable "imp_specFileType"). If a value other than "def" is provided in the argument "filetype" in "getFullData", this is overriding the value of "filetype" in the metadata file. Please refer to the help for "getFullData" for possible values.
	
	commonValue <- "def"					## provide a value that will be the same in all rows of the dataset, leave at 'def' to take the default from the settings

	TimePoints <- FALSE						## how many points in time does the experiment cover? Provide a label for each time-point in the format c("T0", "T1", "T2", ...) or leave at 'FALSE' for no time-splitting
#	TimePoints <- c("T0", "T1", "T2")
	
	nrConScans <- 3							## how many consecutive scans for every sample?

	spacing <- 5							## the space between environmental control samples

	envControlLabel <- "def"				## the label for the environmental control. Leave at 'def' to take the default value from the settings
	realMeasurementLabel <- "def"			## the label for the "real measurement", for all the samples of your experiment. Leave at 'def' to take the default value from the settings

	###
	
	columnNamesL1 <- c("C_Range", "C_Mode")		## please note that there is a special prefix for class- and numerical variables: the default is "C_" for class-variables and "Y_" for numerical variables
	columnNamesL2 <- c("Y_Conc", "C_DELETE")	## "DELETE" is the default character for those columns that, after the generation of the sample list, should be omitted

	L1 <- list(list("RangeA", "RangeB", "RangeC"), list("Serial", "Direct")) 	## XXX explain this L1 and L2 thing
	L2 <- list(list(as.character(seq(0.1,1, by=0.1)), as.character(seq(0.01, 0.1, by=0.01)), as.character(seq(0.001, 0.01, by=0.001))), list("Ser", "Dir")) 

	###

	Repls <- 2								## how many replications for each sample?

	Group <- c("Exp", "Cont") 				## additional groups to split the above generated classes into, like e.g. experiment and control



### do NOT change the names of the variables !!  ####
####################################################################
