####################################################################
#################### aquap2 metadata file ##########################
####################################################################

### do NOT change the names of the variables !!  ####

expName <- "DZ3W"						## the name of the experiment
delSec <- TRUE
delToo <- TRUE
filetype <- "vision_NSAS.da"			## the filetype of the raw-data file. Leave at "def" to get the default from the settings (variable "imp_specFileType"). If a value other than "def" is provided in the argument "filetype" in the function "getFullData", this is overriding the value of "filetype" in the metadata file. Please refer to the help for ?getFullData for possible values.
noiseFileName <- "def"					## the name of the corresponding noise-data file residing in the AQUAP2SH folder. Leave at 'def' to get the default from the settings (parameter 'noi_noiseDataFilename'). If a value other than "def" is provided in the argument 'noiseFile' in the function 'gdmm', this is overriding the value of 'noiseFileName' here in the metadata file. Please refer to the help for ?noise_procedures for more information.
tempCalibFileName <- "TempCalib_XDS" 	## the name of the corresponding temperature-data file residing in the AQUAP2SH folder. Leave at 'def' to get the default from the settings (parameter 'aqg_tempCalib_Filename'). If a value other than "def" is provided in the argument 'tempFile' in the function 'gdmm', this is overriding the value of 'tempCalibFileName' here in the metadata file. Please refer to the help for ?tempCalib_procedures for more information.
isFalseHere <- "smoe"
xaxDenominator <- "Wavelength"			## the unit that is displayed on the x-axis when plotting e.g. the raw spectra.
yaxDenominator <- "Absorbance"			## the unit that is displayed on the y-axis when plotting e.g. the raw spectra.
typeCaster <- "Corruscant"
###

###
mustGoToo <- TRUE
TimePoints <- FALSE						## how many points in time does the experiment cover? Provide a label for each time-point in the format c("T0", "T1", "T2", ...) or leave at 'FALSE' for no time-splitting
nrConScans <- 5							## how many consecutive scans for every sample?
isNowLast <- "fineFumes"

### do NOT change the names of the variables !!  ####
####################################################################
