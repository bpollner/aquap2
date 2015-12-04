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
#' @docType methods
#' @name Extract_Elements
NULL
