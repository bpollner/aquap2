
#' @title Customising Spectral Import
#' @description Cou can provide your own import function for importing spectral 
#' data. The .r file containing this function has to be located in the folder 
#' specified in the .Renviron file. (see 'Note' in \code{\link{updateSettings}}) 
#' To tell the package to use your custom function, you  have to provide the 
#' character \code{"custom@@yourFile.r"} to the approrpiate arguments (like e.g. 
#' the function  \code{\link{getFullData}}), with  "yourFile.r" being the 
#' name of the .r file containing the function.
#' @details The requirements are as follows: 
#' There have to be two and only to objects in this file, having the names 
#' "fileExtension" and "spectralImport".
#' \itemize{
#'    \item fileExtension: Provide a character length one containing the extension 
#'    of your raw-spectra file, e.g. ".txt" for a text file.
#'    \item spectralImport: This is the function itself. The first argument has 
#'    to be the data file. Provide any necessary defaults in the 
#'    function definition.
#' }
#' @return The returned value of the function has to be a list with the following 
#' elements:
#' \itemize{
#'  \item NIR a data frame with the NIR spectra
#'  \item 
#' 
#' }
#' addCols if time then column name "Timestamp", if Temp and RH then XXX
#' NIR the NIR spectra
#' nchar Prev wl in info
#' if time called Timestamp format POSIXct
#' @aliases gfd
#' @examples
#' \dontrun{
#' # this could be the content of the .r file for defining a custom function 
#' # to import data from a text file. 
#'    fileExtension <- ".txt"
#'    spectralImport <- function(dataFile, naStrings="NA") {
#'      # XXX modify this
#'      return(read.table(dataFile, na.strings=naStrings))
#'    }
#' }
#' @family Development
#' @name custom_import
NULL


# outList <- list(sampleNr=sampleNr, timePoints=timePoints, ecrm=ecrm, repl=repl, group=group, temp=temp, relHum=relHum, C_cols=C_cols, Y_cols=Y_cols, timestamp=timestamp, info=info, NIR=NIR)
