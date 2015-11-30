

#' @title Plot SIMCA
#' @description Plot SIMCA interclass distances.
#' @details The width and height of the resulting pdf can be set in the settings.
#' @param cube An object of class 'aquap_cube' as produced by \code{\link{gdmm}}.
#' @param ... Optional 'sim' plotting parameters to override the values in the 
#'  analysis procedure - for possible arguments see 
#'  \code{\link{plot_sim_args}}.
#' @return A pdf or graphic device.
#' @family Plot functions
#' @family SIMCA documentation
#' @examples
#'  \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset)
#'  plot(cube)
#'  plot_sim(cube)
#' }
#' @export
plot_sim <- function(cube, ...) {
	autoUpS()
#	ap <- getap(.lafw_fromWhere="cube", cube=cube, ...)			 # the ... are here used for additionally modifying (if matching arguments) the analysis procedure obtained from the cube
	ap <- getap(...) # load from file, possibly modify via ...
	ap <- ap_cleanZeroValuesCheckExistenceDefaults(ap, dataset=getDataset(cube[[1]])) # just take the first dataset, as we mainly need the header (and the wavelengths are already checked.. )
	if (is.null(ap$simca)) {
		return(cat("*** SIMCA model not available or not selected for plotting \n"))
	}
	where <- ap$genPlot$where
	onMain <- ap$genPlot$onMain
	onSub <- ap$genPlot$onSub
	fns <- ap$genPlot$fns
	#
	print("Plotting Simca here soon hahaha...")
} # EOF



#' @title Plot SIMCA - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{plot}} and \code{\link{plot_sim}} to override the values 
#' in the analysis procedure file and so to modify the graphics - see examples.
#' 
#' \code{plot(cube, ...)}
#' 
#' \code{plot_sim(cube, ...)}
#' 
#' @template mr_details_allParams
#' @template mr_sim_plot_param
#' @template mr_pg_genParams
#' @seealso \code{\link{plot_sim}}
#' @examples
#' \dontrun{
#' dataset <- gfd()
#' cube <- gdmm(dataset)
#' plot(cube, pg.sub="bar") # adds 'bar' to the subtitle of each single plot
#' plot_sim(cube, pg.sub="bar", pg.fns="_bar") # adds '_bar' to the filename of 
#' # the pdf
#' }
#' @family Plot arguments
#' @family SIMCA documentation
#' @name plot_sim_args
NULL



#' @title Calculate SIMCA - Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' function \code{\link{getap}}, also within function \code{\link{gdmm}}, to 
#' override the values in the analysis procedure file and so to modify the 
#' calculation of SIMCA models - see examples.
#' 
#' \code{getap(...)}
#'  
#' \code{gdmm(dataset, ap=getap(...))}
#' 
#' @section Note: Calculation of SIMCA models is done with the function 
#' \code{\link[rrcovHD]{RSimca}} /  \code{\link[rrcovHD]{CSimca}} and with code 
#' adapted from the manual-page of the Chemometrics-software "Pirouette" XXX. 
#' @template mr_details_allParams
#' @template mr_sim_calc_param
#' @seealso \code{\link{gdmm}}
#' @examples
#' \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset, getap(do.sim=TRUE))
#'  cube <- gdmm(dataset, getap(do.sim=TRUE, sim.K=4))
#' }
#' @family Calc. arguments
#' @family SIMCA documentation
#' @name calc_sim_args
NULL
