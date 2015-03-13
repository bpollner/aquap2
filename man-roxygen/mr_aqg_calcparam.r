#' @param do.aqg Logical. If Aquagrams should be calculated with a given dataset.
#' @param aqg.vars NULL or character vector. Which class variables 
#' should be used for grouping the data for the Aquagram. Provide a character 
#' vector with the column names of class variables for grouping data and generate 
#' an Aquagram for every one of them, or set to NULL if you are providing the 
#' Aquagram class-variable later when calculating and plotting the Aquagram.
#' @param aqg.nrCorr Character or Logical. If the number of observations in each
#' spectral pattern should be corrected (if necessary by random sampling) so
#' that all the spectral pattern are calculated out from the same number of
#' observations. If left at the default "def", the default value from the settings
#' will be used. Provide "TRUE" or "FALSE" to switch number correction manually
#' on or off.
#' @param aqg.spectra Logical or Character. If left at "FALSE" (the default) 
#' no additional spectra are calculated / prepared for plotting. Other possible 
#' values are one or more of:
#' \itemize{
#'  \item "raw" for the raw spectra 
#'  \item "avg" for the averaged spectra of the data represented in the aquagram;
#'  \item "subtr" for subtractions in the averaged spectra (see \code{"minus"} below)
#'  \item "all" for all of the aforementioned
#' }
#' @param aqg.minus Character. Which of the levels present in the variable 
#' provided in \code{aqg.vars} should be used for subtractions -- the average of 
#' this 'minus' gets subtracted from all the other averages. 'aqg.minus' is used for 
#' the subtractions in the raw spectra as well as for the subtractions within the
#' Aquagram, should you choose any of the -diff modes.
#' @param aqg.mod Character. What mode, what kind of Aquagram should be calculated?
#' Possible values are: "classic", "classic-diff", "sfc", "sfc-diff", "aucs",
#' "aucs-diff", "aucs.tn", "aucs.tn-diff", "aucs.tn.dce", "aucs.tn.dce-diff",
#' "aucs.dce" and "aucs.dce-diff" and "def" for reading in the default from 
#' settings.r.
#' @param aqg.TCalib Character, numeric or NULL. The default (leave at 'def') can be 
#' set in the settings.  If 'NULL' the complete temperature range of the 
#' calibration data is used for calibration.  Provide a numeric length two 
#' [c(x1, x2)] for manually determining the calibration range.  Provide a 
#' character 'symm@@x', with 'x' being the plus and minus delta in temperature 
#' from the temperature of the experiment for having a calibration range from 
#' Texp-x to Texp+x.  The 'Factory' default is 'symm@@2'. 
#' Applies to all modes except the 'classic' and 'sfc'  modes. 
#' If, in any of the modes showing percentages, the numbers on the 
#' Aquagram are below 0 or above 100, then the calibration range has to be 
#' extended.
#' @param aqg.Texp Numeric length one. The temperature at which the 
#' spectra were taken. The default (leave at 'def') can be set in the settings.
#' @param aqg.bootCI Logical. If confidence intervalls for the selected wavelengths
#' should be calculated within each group (using bootstrap). Leave at 'def' for 
#' getting the default from the settings.
#' @param aqg.R Character or numeric. Given aqg.bootCI = TRUE, how many bootstrap
#' replicates should be performed? Leave at 'def' for choossing the default from 
#' the settings, where the factory-default is "nrow@@3" for for 3 x nrow(samples). 
#' By manually providing a character in the form of 'nrow@@x' where x is any 
#' number, you can set the factor with which the number of rows get multiplicated, 
#' the result of this multiplication is then used for the number of bootstrap 
#' replicates. By providing a length one numeric you can directly set the number 
#' of bootstrap replicates.
#' @param aqg.smoothN Only used in the 'classic' and 'sfc' modes. Numeric length 1.
#' Must be odd. Smoothing points for the Sav. Golay smoothing that is applied
#' before making the calculations. Change to NULL or anything not-numeric to
#' switch off smoothing.
#' @param aqg.selWls Only used in the 'classic' and 'sfc' modes.  Numerical vector.
#' If provided and in the mode "classic", classic-diff", "sfc" and "sfc-diff"
#' these numbers will be used to determine the coordinates of the aquagram.
#' Leave at 'def' to use the defaults from the settings file.
#' @param aqg.msc Only used in the 'classic' and 'sfc' modes. Logical. If MSC
#' should be performed.
#' @param aqg.reference Only used in the 'classic' and 'sfc' modes. An optional
#' numerical vector (loadings, etc..) used for MSC.
