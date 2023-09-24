####################################################################
#################### aquap2 metadata file ##########################
####################################################################

### do NOT change the names of the variables !!  ####

expName <- "DZ3W"						## the name of the experiment
filetype <- "vision_NSAS.da"			## the filetype of the raw-data file. Leave at "def" to get the default from the settings (variable "imp_specFileType"). If a value other than "def" is provided in the argument "filetype" in the function "getFullData", this is overriding the value of "filetype" in the metadata file. Please refer to the help for ?getFullData for possible values.
realMeasurementLabel <- "RM"			## the label for the "real measurement", for all the samples of your experiment. Leave at 'def' to take the default value from the settings
yaxDenominator <- "Absorbance"			## the unit that is displayed on the y-axis when plotting e.g. the raw spectra.


###
TimePoints <- FALSE						## how many points in time does the experiment cover? Provide a label for each time-point in the format c("T0", "T1", "T2", ...) or leave at 'FALSE' for no time-splitting
###


###
sl_classes <- "mdx"   				 	## the name of the xlsx file in the metadata folder holding the class structure. Leave at 'def' to get the default from the settings (parameter 'fn_class_structure').
###




###
Repls <- 3								## how many replications for each sample?


### do NOT change the names of the variables !!  ####
####################################################################
