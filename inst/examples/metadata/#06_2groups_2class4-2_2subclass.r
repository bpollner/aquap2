####################################################################
#################### aquap2 metadata file ##########################
####################################################################

### do NOT change the names of the variables !!  ####

expName <- "2gr 2class4-2 2subclass"						## the name of the experiment
filetype <- "def"						## the filetype of the raw-data file. Leave at "def" to get the default from the settings (variable "imp_specFileType"). If a value other than "def" is provided in the argument "filetype" in the function "getFullData", this is overriding the value of "filetype" in the metadata file. Please refer to the help for ?getFullData for possible values.
noiseFileName <- "def"					## the name of the corresponding noise-data file residing in the AQUAP2SH folder. Leave at 'def' to get the default from the settings (parameter 'noi_noiseDataFilename'). If a value other than "def" is provided in the argument 'noiseFile' in the function 'gdmm', this is overriding the value of 'noiseFileName' here in the metadata file. Please refer to the help for ?noise_procedures for more information.
tempCalibFileName <- "def" 				## the name of the corresponding temperature-data file residing in the AQUAP2SH folder. Leave at 'def' to get the default from the settings (parameter 'aqg_tempCalib_Filename'). If a value other than "def" is provided in the argument 'tempFile' in the function 'gdmm', this is overriding the value of 'tempCalibFileName' here in the metadata file. Please refer to the help for ?tempCalib_procedures for more information.
commonValue <- "def"					## provide a value that will be the same in all rows of the dataset, leave at 'def' to take the default from the settings
envControlLabel <- "def"				## the label for the environmental control. Leave at 'def' to take the default value from the settings
realMeasurementLabel <- "def"			## the label for the "real measurement", for all the samples of your experiment. Leave at 'def' to take the default value from the settings
###
TimePoints <- NULL						## how many points in time does the experiment cover? Provide a label for each time-point in the format c("T0", "T1", "T2", ...) or leave at 'FALSE' for no time-splitting
nrConScans <- 3							## how many consecutive scans for every sample?
spacing <- 7							## the space between environmental control samples (set to FALSE for not inserting environmental controls)
###
columnNamesL1 <- c("C_waterName", "C_waterCondition")		## please note that there is a special prefix for class- and numerical variables: the default is "C_" for class-variables and "Y_" for numerical variables
columnNamesL2 <- c("C_source", "Y_sparkLevel")					## "DELETE" is the default character for those columns that, after the generation of the sample list, should be omitted
L1  <- list(list("Pinoir", "Aquavit", "Fountainbleu", "Ramedi"), list("sparkling", "still")) 	## Please look at the vignette 'Examples for Experiment-Design' for more information on the use of the arguments L1 and L2.
L2  <- list(list(c("A", "D", "CH"), c("A", "D", "CH"), c("CH", "F", "N"), c("CH", "F", "N", "D", "A")), list(as.character(c(12,24,36)), as.character(c(0))))
###
Repls <- 2								## how many replications for each sample?
Group <- c("Exp", "Cont")		## additional groups to split the above generated classes into, like e.g. experiment and control


### do NOT change the names of the variables !!  ####
####################################################################
 
