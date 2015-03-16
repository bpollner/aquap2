

#'  @title Plot Aquagram
#'  @description Plot Aquagram XXX.
#'  @details The width and height of the resulting pdf can be set in the settings.
#'  @param cube An object of class 'aquap_cube' as produced by \code{\link{gdmm}}.
#'  @param ... Optional 'aqg' plotting parameters to override the values in the 
#'  analysis procedure stored in the 'cube' - for possible arguments see 
#'  \code{\link{plot_aqg_args}}.
#'  @return A pdf or graphic device.
#'  @family Plot functions
#'  @family Aquagram documentation
#'  @examples
#'  \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset)
#'  plot(cube)
#'  plot_aqg(cube)
#'  }
#'  @export
plot_aqg <- function(cube, ...) {
  autoUpS()
  ap <- getap(.lafw_fromWhere="cube", cube=cube, ...)    	 # the ... are here used for additionally modifying (if matching arguments) the analysis procedure obtained from the cube
  ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset=getDataset(cube[[1]])) # just take the first dataset, as we mainly need the header (and the wavelengths are already checked.. )
  if (is.null(ap$aquagr)) {
    return(cat("*** Aquagram not available or not selected for plotting \n"))
  }
  where <- ap$genPlot$where
  onMain <- ap$genPlot$onMain
  onSub <- ap$genPlot$onSub
  fns <- ap$genPlot$fns
  #
  print("Plotting Aquagram here soon that would be nice ...")
} # EOF



#' @title Plot Aquagram - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{plot}} and \code{\link{plot_aqg}} to override the values 
#' in the analysis procedure file and so to modify the graphics - see examples.
#' 
#' \code{plot(cube, ...)}
#' 
#' \code{plot_aqg(cube, ...)}
#' 
#' @template mr_details_allParams
#' @template mr_aqg_plot_param
#' @template mr_pg_genParams
#' @seealso \code{\link{plot_aqg}}
#' @examples
#' \dontrun{
#' dataset <- gfd()
#' cube <- gdmm(dataset)
#' plot(cube)
#' }
#' @family Plot arguments
#' @family Aquagram documentation
#' @name plot_aqg_args
NULL



#' @title Calculate Aquagram - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{getap}}, also within function \code{\link{gdmm}}, to 
#' override the values in the analysis procedure file and so to modify the 
#' calculation of Aquagrams - see examples.
#' 
#' \code{getap(...)}
#'  
#' \code{gdmm(dataset, ap=getap(...))}
#' 
## @section Note: Calculation of PLSR models is done with the function \code{\link[pls]{plsr}}. 
#' @references XXX hopefully a nice reference to the Aquagram paper!! XXX
#' @template mr_details_allParams
#' @template mr_aqg_calc_param
#' @seealso \code{\link{gdmm}}
#' @examples
#' \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset, ap=getap())
#'  cube <- gdmm(dataset, ap=getap())
#' }
#' @family Calc. arguments
#' @family Aquagram documentation
#' @name calc_aqg_args
NULL



#' @title Calculate Aquagram - Modes
#' @description The following values can be provided to the argument \code{aqg.mod} 
#' in \code{getap}, specifiying what type of Aquagram should be calculated.
#' @details XXX
#' @section Possible Values: Possible Aquagram modes are
#' \itemize{
#' \item \code{classic}: The classical aquagram - smoothed, MSC, normalized
#' \item \code{classic-diff}: same as above + one group subtracted from all the
#' others. 
#' \item \code{sfc}: "Scaled, foreign center": smoothed, MSC, centered on the mean of
#' selected calibration data, scaled. 
#' \item \code{sfc-diff}: same as above + one group subtracted from all the others.
#' \item \code{aucs}: "Area Under Curve Stabilized": the area under the curve (auc)
#' from the spectra in all the 12/15 coordinates in a set of calibration data
#' at varying temperature is calculated, then divided by the total auc. Then
#' the smalles and biggest value is taken as 0% and 100%. The same auc is
#' calculated for every sample, and its value in percent ... XXX 
#' \item \code{aucs-diff}: same as above + one group subtracted from all the others. 
#' \item \code{aucs.tn}: aucs + temperature normalization: .... XXX the auc from a
#' sample at Texp gets subtracted from all the auc of the samples. 
#' \item \code{aucs.tn-diff}: same as above + one group subtracted from all the
#' others
#' \item \code{aucs.tn.dce}: same as aucs.tn, but the scale calculated to show
#' degrees celsius equivalent
#' \item \code{aucs.tn.dce-diff}: same as above + one group subtracted from all the
#' others
#' \item \code{aucs.dce}: same as aucs, but the scale calculated to show degrees
#' celsius equivalent
#' \item \code{aucs.dce-diff}: same as above + one group subtracted from all the
#' others
#' }
#' @seealso \code{\link{calc_aqg_args}}
#' @references XXX the Aquagram paper XXX
#' @name calc_aqg_modes
NULL





