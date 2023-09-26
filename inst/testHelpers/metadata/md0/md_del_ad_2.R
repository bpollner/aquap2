####################################################################
#################### aquap2 metadata file ##########################
####################################################################

### do NOT change the names of the variables !!  ####

expName <- "DZ3W"						## the name of the experiment
filetype <- "vision_NSAS.da"			## the filetype of the raw-data file. Leave at "def" to get the default from the settings (variable "imp_specFileType"). If a value other than "def" is provided in the argument "filetype" in the function "getFullData", this is overriding the value of "filetype" in the metadata file. Please refer to the help for ?getFullData for possible values.
noiseFileName <- "def"					## the name of the corresponding noise-data file residing in the AQUAP2SH folder. Leave at 'def' to get the default from the settings (parameter 'noi_noiseDataFilename'). If a value other than "def" is provided in the argument 'noiseFile' in the function 'gdmm', this is overriding the value of 'noiseFileName' here in the metadata file. Please refer to the help for ?noise_procedures for more information.
thirdStart <- 123
mustGo <- TRUE


###
tempCalibFileName <- "TempCalib_XDS" 	## the name of the corresponding temperature-data file residing in the AQUAP2SH folder. Leave at 'def' to get the default from the settings (parameter 'aqg_tempCalib_Filename'). If a value other than "def" is provided in the argument 'tempFile' in the function 'gdmm', this is overriding the value of 'tempCalibFileName' here in the metadata file. Please refer to the help for ?tempCalib_procedures for more information.
commonValue <- "def"					## provide a value that will be the same in all rows of the dataset, leave at 'def' to take the default from the settings
envControlLabel <- "MQ"					## the label for the environmental control. Leave at 'def' to take the default value from the settings
realMeasurementLabel <- "RM"			## the label for the "real measurement", for all the samples of your experiment. Leave at 'def' to take the default value from the settings
###


###
newBlo <- TRUE
secondN <- TRUE
thirdn <- TRUE


sampleListType <- "def"					## The type of sample-list file in the sampleLists/sl_in folder. Leave at 'def' to get the default from the settings (parameter 'imp_sampleListType').  If a value other than "def" is provided in the argument "slType" in the function "getFullData", this is overriding the value of "sampleListType" in the metadata file. Please refer to the help for ?getFullData for possible values.
tempHumLog <- "def"						## If data from temperature and rel.humidity logger should be imported and aligned to a timestamp in the dataset. Leave at 'def' to get the default from the settings (parameter 'imp_use_TRH_logfile').  If a value other than "def" is provided in the argument "trhLog" in the function "getFullData", this is overriding the value of "tempHumLog" in the metadata file. Please refer to the help for ?getFullData for possible values.
multiplyRows <- "def"					## Character ('def') or Logical. If all the rows in the sample list should be multiplied by the number of consecutive scans as specified in the metadata of the experiment. Leave at 'def' to get the default from the settings (parameter 'imp_multiplyRows'). If a value other than "def" is provided in the argument "multiplyRows" in the function "getFullData", this is overriding the value of "multiplyRows" in the metadata file. Please refer to the help for ?getFullData for possible values.



### do NOT change the names of the variables !!  ####
####################################################################
