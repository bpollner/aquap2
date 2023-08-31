#' @title The Settings File
#' @description The settings file is used to store parameters and constants used
#' in several functions, and it is living in the folder specified by the 
#' variable \code{AQUAP2SH} in the .Renviron file. 
#' @details  If not present, the required `.Renviron` file will be 
#' automatically created. If the variable `AQUAP2SH` is not defined in the 
#' .Renviron file, it will be automatically added, and its default path is 
#' pointing to the (possibly also created) folder `aquap2SH` in the users home 
#' directory, where the `settings.r` file is automatically copied to if not 
#' already present. It is possible to manually provide a different path in the 
#' variable `AQUAP2SH` in the .Renviron file, pointing to any folder where then 
#' the settings.r file and all other relevant general files will reside.
#' Once you have specified a folder under the variable \code{AQUAP2SH} in 
#' your .Renviron file, you can use \code{\link{updateSettings}} to copy a fresh 
#' version of the settings.r file to this destination. 
#' Change the values in settings.r in this folder to change the behaviour of the 
#' package 'aquap2'.
#' @section Note: You will get notified if there should be a newer version of 
#' the settings.r file available within the  package. If you changed some values 
#' of the settings.r file and then copy a new setings.r file from the package, 
#' you have to adapt the newly copied file to contain your changed values.
#' @family fileDocs
#' @name settings_file
NULL
