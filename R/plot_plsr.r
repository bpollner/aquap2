#'  @title Plot PLSR
#'  @description Plot PLSR XXX.
#'  @details The width and height of the resulting pdf can be set in the settings.
#'  @param cube An object of class 'aquap_cube' as produced by \code{\link{gdmm}}.
#'  @param ... Optional 'pls' plotting parameters to override the values in the 
#'  analysis procedure stored in the 'cube' - for possible arguments see 
#'  \code{\link{plot_pls_args}}.
#'  @return A pdf or graphic device.
#'  @family Plot functions
#'  @family PLSR documentation
#'  @examples
#'  \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset)
#'  plot(cube)
#'  plot_pls(cube)
#'  }
#'  @export
plot_pls <- function(cube, ...) {
  autoUpS()
  ap <- getap(.lafw_fromWhere="cube", cube=cube, ...)  		 # the ... are here used for additionally modifying (if matching arguments) the analysis procedure obtained from the cube
  ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset=getDataset(cube[[1]])) # just take the first dataset, as we mainly need the header (and the wavelengths are already checked.. )
  if (is.null(ap$plsr)) {
    return(cat("*** PLSR model not available or not selected for plotting \n"))
  }
  where <- ap$genPlot$where
  onMain <- ap$genPlot$onMain
  onSub <- ap$genPlot$onSub
  fns <- ap$genPlot$fns
  #
  print("Plotting PLSR here soon yes please ...")
} # EOF



#' @title Plot PLSR - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{plot}} and \code{\link{plot_pls}} to override the values 
#' in the analysis procedure file and so to modify the graphics - see examples.
#' 
#' \code{plot(cube, ...)}
#' 
#' \code{plot_pls(cube, ...)}
#' 
#' @template mr_details_allParams
#' @template mr_pls_plot_param
#' @template mr_pg_genParams
#' @seealso \code{\link{plot_pls}}
#' @examples
#' \dontrun{
#' dataset <- gfd()
#' cube <- gdmm(dataset)
#' plot(cube, do.pls=FALSE) # to plot everything availalbe except the plsr-models
#' plot(cube, pls.colorBy="C_Temp")
#' plot_pls(cube, pls.colorBy="C_Temp")
#' }
#' @family Plot arguments
#' @family PLSR documentation
#' @name plot_pls_args
#' NULL



#' @title Calculate PLSR - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{getap}}, also within function \code{\link{gdmm}}, to 
#' override the values in the analysis procedure file and so to modify the 
#' calculation of PLSR models - see examples.
#' 
#' \code{getap(...)}
#'  
#' \code{gdmm(dataset, ap=getap(...))}
#' 
#' @section Note: Calculation of PLSR models is done with the function  
#' \code{\link[pls]{plsr}}. 
#' @template mr_details_allParams
#' @template mr_pls_calc_param
#' @seealso \code{\link{gdmm}}
#' @examples
#' \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset, ap=getap(pls.regOn="Y_Temp"))
#'  cube <- gdmm(dataset, ap=getap(pls.ncomp=5, pls.regOn="Y_Foo"))
#' }
#' @family Calc. arguments
#' @family PLSR documentation
#' @name calc_pls_args
#' NULL
