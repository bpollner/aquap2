#' @title Customising Import from Logger-files
#' @description Cou can provide your own import function for importing temperature 
#' and relative humidity data. The .r file containing this function has to be 
#' located in the folder specified in the .Renviron file. 
#' (see 'Note' in \code{\link{updateSettings}}) 
#' To tell the package to use your custom function, you  have to provide the 
#' character \code{"custom@@yourFile.r"} to the approrpiate arguments (like e.g. 
#' the function \code{\link{getFullData}}), with  "yourFile.r" being the 
#' name of the .r file containing the function.
#' @details The requirements for this .r file are as follows: 
#' There need to be two and only to objects in this file, having the names 
#' "fileExtension" and "trhImport". All other objects will be ignored.
#' \itemize{
#'    \item fileExtension: Provide a character length one containing the extension 
#'    of your raw-spectra file, e.g. ".txt" for a text file.
#'    \item trhImport: This is the function itself. The first argument has 
#'    to be the data file. No additional arguments than the first one will be used.
#'    Provide any necessary defaults in the function itself.
#' }
#' @return The returned value of the function has to be data frame with three 
#' columns having the following names:
#' \itemize{
#'    \item Time The column containing the timestamps in 'POSIX' format.
#'    \item Temp A numeric column containing temperature values.
#'    \item RelHum A numeric column containing values for relative humidity.
#' }
#' @examples
#' \dontrun{
#' # this could be the content of the .r file for defining a custom function, 
#' # in this example for an import from a text file where the first three 
#' # columns contain the data. The first four rows get skipped.
#'  fileExtension <- ".txt"
#'  trhImport <- function(dataFile) {  
#'    ##
#'    TRH <- read.table(dataFile, header=F, skip=4)[,c(1,2,3)]
#'    # assuming time, temp and rel. hum are in column 1, 2, and 3
#'    colnames(TRH) <- c("Time", "Temp", "RelHum")
#'    TRH$Time <- strptime(TRH$Time, format = "%Y/%m/%d %H:%M'%S")
#'    return(TRH)
#'  } # EOF
#' 
#' }
#' @family Development Functions
#' @name custom_TRH
NULL
