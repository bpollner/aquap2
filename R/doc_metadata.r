#' @title The Metadata File
#' @description The metadata file contains all the metadata of an experiment 
#' like experiment name etc. It is also used to design the experiment and 
#' to export a randomized sample list that then can be used in the data 
#' acquisition process. Additionally, the references to the temperature 
#' calibration file (necessary for plotting Aquagrams) and to the file containing 
#' the noise-spectra are kept here.
#' @details While the parameters \code{TimePoints, nrConScans, spacing, Repls} 
#' and \code{Group} can be used to further modify an existing sample list, the 
#' class structure as provided in the Excel file referenced at argument 
#' \code{sl_classes} is defining the basic class-structure of an experiment. 
#' Please refer to XXX for examples on how define the class structure via 
#' xlsx-input. 
#' @param expName The name of the experiment. This will be used in many plot 
#'  titles and as a first part of the name of saved PDFs.
#' @param filetype Character length one. The filetype of the raw-data file. 
#' Leave at "def" to get the default from the settings 
#' (parameter \code{imp_specFileType}). If a value other than "def" is provided 
#' in the argument "filetype" in "getFullData", this is overriding the value of 
#' "filetype" in the metadata file. Please refer to the help for "getFullData" 
#' for possible values.
#' @param noiseFileName The name of the corresponding noise-data file residing 
#' in the AQUAP2SH folder. Leave at 'def' to get the default from the settings 
#' (parameter 'noi_noiseDataFilename'). If a value other than "def" is provided 
#' in the argument 'noiseFile' in the function 'gdmm', this is overriding the 
#' value of 'noiseFileName' here in the metadata file. Please refer to the help 
#' for ?noise_procedures for more information.
#' @param tempCalibFileName The name of the corresponding temperature-data file 
#' residing in the AQUAP2SH folder. Leave at 'def' to get the default from the 
#' settings (parameter 'aqg_tempCalib_Filename'). If a value other than "def" 
#' is provided in the argument 'tempFile' in the function 'gdmm', this is 
#' overriding the value of 'tempCalibFileName' here in the metadata file. Please 
#' refer to the help for ?tempCalib_procedures for more information.
#' @param commonValue Character. A value that will be be present in all the rows 
#'  of the dataset. The default 'def' reads in the default value from the 
#' settings.r file. (Parameter \code{p_commonNoSplit}.) 
#' @param envControlLabel The label for the environmental control. Leave at 
#' code{def} to take the default value from the settings 
#' (parameter \code{p_envControlLabel}).
#' @param realMeasurementLabel The label for the "real measurement", for all 
#' the samples of an experiment. Leave at \code{def} to take the default value 
#' from the settings (parameter \code{p_realMeasurementLabel}).
#' @param xaxDenominator The unit that is displayed on the x-axis when 
#' plotting e.g. raw spectra.
#' @param yaxDenominator The unit that is displayed on the y-axis when 
#' plotting e.g. raw spectra.
#' @param TimePoints Logical 'FALSE' or a character vector. Leave at 'FALSE' if 
#' your experiment does not cover more than one point in time, otherwise provide 
#' a label for each time-point in the format c("T0", "T1", "T2", ...) or leave 
#' at 'FALSE' for no time-splitting.
#' @param nrConScans Numeric length one. The number of consecutive scans for each 
#' sample.
#' @param spacing Numeric length one. The number of "real measurements" between 
#' each "environmental control", i.e. the "space" between environmental control 
#' samples (set to \code{FALSE} for not inserting environmental controls).
#' @param envControlLabel Character. The label for the enironmental control. 
#' The default 'def' reads in the default value from the settings.r file.
#' @param realMeasurementLabel Character. The label for the "real measurements". 
#' The default 'def' reads in the default value from the settings.r file.
#' @param sl_classes Character length one. The name of the xlsx file located in 
#' the metadata folder holding the desired class structure for the experiment, 
#' see details. 
#' @param Repls Numeric. How many replicates of each sample to measure. The values 
#' in the dataset wil be prefixed with the default character for the replicates, 
#' which can be set in the settings (default is "R"). So, with e.g. three 
#' replicates you will find the values "R1", "R2", and "R3" in the dataset.
#' @param Group Character vector. Additional groups to split the above generated 
#' classes into, like e.g. experiment and control. Please see details.
#' @seealso \code{\link{getmd}}
#' @family fileDocs
#' @name metadata_file
NULL
