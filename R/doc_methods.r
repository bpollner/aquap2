#' @title Extract Elements
#' @description Extract single elements of a dataset or of a set, i.e. a single 
#' list element of a cube.
#' @details Function \code{getHeader} will return only the header of a dataset 
#' without the NIR-data.
#' @param object Either a dataset as produced by \code{\link{gfd}} or a set (see 
#' examples).
#' @examples
#' \dontrun{
#' fd <- gfd()
#' cube <- gdmm(fd)
#' header <- getHeader(fd)
#' header1 <- getHeader(cube[[1]]) # gets the header from the first dataset in 
#' # the cube.
#' }
#' @family Extract Elements
#' @name Extract_Elements
NULL



#' @title aquap_data methods
#' @description Available methods for objects of class 'aquap_data', used for
#' subtractions, subscripting etc. of datasets.
#' @details \describe{
#' \item{Subscripting via '[]'}{For subscripting via \code{'[]'}, only values 
#' for rownumbers are accepted.}
#' \item{Subtractions via '-'}{Use the subtraction via \code{'-'} to subtract the 
#' NIR-data in datasets that have the same structure, i.e. the same header, and 
#' possibly same wavelengths in the NIR. You can either subtract two equally long 
#' datasets, i.e. two datasets having the exactly same number of rows, or you can
#' subtract a dataset having exactly one row from a full dataset. The latter can
#' be useful for e.g. subtracting the averaged spectra of a single class from the
#' complete dataset - see examples. If parameter 
#' \code{gen_calc_allowSubtrDiffWavels}} in the settings.r file is set to TRUE, it 
#' is also possible to subtract two datasets that do \strong{not} have the same 
#' number of wavelengths. This can e.g. happen ater the usage of 
#' \code{\link{do_gapDer}}. In this case, the bigger dataset, i.e. the dataset 
#' having more wavelengths, is cut down to match exactly the wavelength-range of 
#' the smaller dataset. In other words, the return in this case is a dataset having 
#' only the number of wavelengths as 'dictated' by the smaller dataset.
#' }
#' @note \code{drop} is always set to \code{FALSE} for substrictping via 
#' \code{'[]'}.
#' @param x An object of class 'aquap_data'
#' @param i subsricpting indices for rows 
#' @param e1 Object of class 'aquap_data'
#' @param e2 Object of class 'aquap_data'
#' @examples 
#' \dontrun{
#'  dataset <- gfd()
#'  ds <- dataset[1:5,]
#'  ds <- dataset[1:5] # the same as above
#' 	dSub <- dataset - do_sgolay(dataset, n=71) # apply smoothing and subtract
#'  plot(dSub, pg.where="", pg.main=" | smoothed subtracted")
#' }
#' @family Data pre-treatment functions
#' @aliases aquap_data-methods
#' @name aquap_data-methods
NULL
