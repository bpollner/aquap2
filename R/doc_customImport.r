
#' @title Customising Spectral Import
#' @description Cou can provide your own import function for importing spectral 
#' data. The .r file containing this function has to be located in the folder 
#' specified in the .Renviron file. (see 'Note' in \code{\link{updateSettings}}) 
#' To tell the package to use your custom function, you  have to provide the 
#' character \code{"custom@@yourFile.r"} to the approrpiate arguments (like e.g. 
#' the function \code{\link{getFullData}}), with  "yourFile.r" being the 
#' name of the .r file containing the function.
#' @details The requirements for this .r file are as follows: 
#' There need to be two and only to objects in this file, having the names 
#' "fileExtension" and "spectralImport". All other objects will be ignored.
#' \itemize{
#'    \item fileExtension: Provide a character length one containing the extension 
#'    of your raw-spectra file, e.g. ".txt" for a text file.
#'    \item spectralImport: This is the function itself. The first argument has 
#'    to be the data file. No additional arguments than the first one will be used.
#'    Provide any necessary defaults in the function itself.
#' }
#' @return The returned value of the function has to be a list with named elements 
#' as listed below. If you do not have or want to provide content for an element 
#' it has to be set to 'NULL'.
#' \itemize{
#'  \item sampleNr Data frame or NULL. A data frame with one column containing 
#'  the sample numbers.
#'  \item conSNr Data frame or NULL. A data frame with one column containing the 
#'  number of the consecutive scan.
#'  \item timePoints Data frame or NULL. A data frame with one column containing 
#'  a character representation of the points in time at which the measurements 
#'  were done.
#'  \item ecrm Data frame or NULL. A data frame with one column containing a  
#'  character representation to distinguish between 'environmental control' or 
#'  real measurement'.
#'  \item repl Data frame or NULL. A data frame with one column containing a
#'  character representation of the groups in the experiment like e.g. "Cont" for 
#'  control, ... etc.
#'  \item temp Data frame or NULL. A data frame with one column containing the 
#'  numerical values of the room temperature. 
#'  \item relHum Data frame or NULL. A data frame with one column containing the 
#'  numerical values of the relative humidity. 
#'  \item C_cols Data frame or NULL. A data frame with any number of columns 
#'  containing only class-variables, i.e. non-numeric variables.
#'  \item Y_cols Data frame or NULL. A data frame with any number of columns 
#'  containing only numeric variables.
#'  \item timestamp Data frame or NULL. A data frame with one column containing 
#'  a timestamp in POSIXct format.
#'  \item info A list containing the following named elements:
#'    \itemize{
#'      \item nCharPrevWl Numeric length one, indicating the numbers of characters 
#'      (if any) before the actual wavelength. Set to '0' if you do not have 
#'      characters before the wavelength.
#'    }
#'  \item NIR A matrix containing the NIR-spectra, with the wavelenghts as column 
#'  names and the sample-names as the rownames.
#' }
#' @section Note: Every list element except 'NIR' and 'info' can be set to NULL.
#' @examples
#' \dontrun{
#' # this could be the content of the .r file for defining a custom function, 
#' # in this example for an import from an Excel-file.
#' fileExtension <- ".xlsx"
#' ##
#' spectralImport <- function(dataFile) {
#'    require(xlsx)
#'    import <- read.xlsx(dataFile, sheetIndex=1)
#'    #
#'    sampleNr <- import[1]
#'    conSNr <- import[2]
#'    timePoints <- import[3]
#'    ecrm <- import[4]
#'    repl <- import[8]
#'    group <- import[9]
#'    temp <- import[10]
#'    relHum <- import[11]
#'    C_cols <- import[, c(5,7)]
#'    Y_cols <- import[6]
#'    timestamp <- NULL
#'    info <- list(nCharPrevWl = 2)
#'    NIR <- as.matrix(import[, 12:18])
#'    rownames(NIR) <- paste("S", 1:nrow(NIR), sep="")
#'    #
#'    return(list(sampleNr=sampleNr, conSNr=conSNr, timePoints=timePoints, 
#'    ecrm=ecrm, repl=repl, group=group, temp=temp, relHum=relHum, C_cols=C_cols, 
#'    Y_cols=Y_cols, timestamp=timestamp, info=info, NIR=NIR))
#'    #
#'  } # EOF
#' }
#' @family Development Functions
#' @name custom_import
NULL
