#'	@title The Settings File
#' 	@description The settings file is used to store parameters and constants used
#'  in several functions, and it is living in the folder specified by the 
#'  variable \code{AQUAP2SH} in the .Renviron file. 
#'	@details Once you have specified a folder under the variable \code{AQUAP2SH} in 
#'  your .Renviron file, you can use \code{\link{updateSettings}} to copy a fresh 
#'  version of the settings.r file to this destination. 
#'  Change the values in settings.r in this folder to change the behaviour of the 
#'  package 'aquap2'.
#'  @section Note: You will get notified if there should be a newer version of 
#'  the settings.r file available within the  package. If you changed some values 
#'  of the settings.r file and then copy a new setings.r file from the package, 
#'  your changes will be lost.
#'	@param fn_analysisData The folder name for storing any analysis data
#'	@param fn_exports The folder name for any exported file
#'	@param fn_rcode The folder where all the r-code is kept
#'	@param fn_rawdata  The folder where you put the raw-data from your data 
#'  acquisition
#'	@param fn_rdata The folder where R is keeping all its raw-data after importing 
#'  from the data-acquisition format
#'	@param fn_metadata The folder where you put the metadata and the analysis 
#'  procedure r-files
#'	@param fn_results The folder where all the result-pdfs get saved
#'	@param fn_sampleLists The folder for the sample lists used in randomizing the 
#'  samples and for importing the raw-data
#'  @param XXX complete this 
#'  @family fileDocs
#'  @name settings_file
NULL
