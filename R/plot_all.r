#' @rdname plot
#' @template mr_whatPlotSubtypes
#' @export
plot_cube <- function(x, what="all", ...) {
	autoUpS()
	pv <- pv_what_subPlots 			# c("all", "pca", "sim", "pls", "aqg")
	if (!all(what %in% pv)) {
		stop(paste("Please provide one or more of \n'", paste(pv, collapse="', '"), "' \nto the argument 'what'.", sep=""), call.=FALSE)
	}
	if (any(c(pv[1], pv[2]) %in% what)) { # PCA
		plot_pca(x, ...)
	}
	if (any(c(pv[1], pv[3]) %in% what)) { # SIMCA
		plot_sim(x, ...)	
	}
	if (any(c(pv[1], pv[4]) %in% what)) { # PLSR
		plot_pls(x, ...)
	}
	if (any(c(pv[1], pv[5]) %in% what)) { # Aquagram
		plot_aqg(x, ...)
	}
	invisible(NULL)
} # EOIF


#' @title Plot - General Plotting  Arguments
#' @description The following parameters can be used in the \code{...} argument in 
#' any of the plotting-functions (see \code{\link{plot}}) to override the values 
#' in the analysis procedure file and so to modify the graphics / the pdf - see 
#' examples.
#' 
#' \code{plot(cube, ...)}
#' 
#' \code{plot_cube(cube, ...)}
#' 
#' \code{plot(dataset, ...)}
#' 
#' \code{plot_spectra(dataset, ...)}
#'
#' \code{plot_spectra(cube, ...)} 
#'
#' @template mr_details_allParams
#' @template mr_pg_genParams
#' @examples
#' \dontrun{
#' dataset <- gfd()
#' cube <- gdmm(dataset)
#' plot(cube, pg.main="Foo") 
#' # prints an additional "Foo" on the title of each plot.
#' plot(cube, pg.main="Foo", pg.fns="_foo")
#' # adds the string "_foo" to each of the generated pdfs.
#' }
#' @family Plot arguments
#' @name plot_pg_args
NULL


#' @title *** Plot All Available Models ***
#' @description Plot all available analysis graphics from the models in the 
#' cube-object. Function \code{plot} is a convenience function, it basically 
#' calls  \code{plot_cube}, what is the work-horse for plotting the 
#' cube-object.
#' @details The graphical parameters are taken from the analysis procedure 
#' stored in the cube-object. Via the \code{...} argument it is possible to 
#' override any of these parameters. Please see \code{\link{anproc_file}} for a 
#' complete listing and e.g. \code{\link{plot_pca_args}} and there the other 
#' functions of the 'Plot arguments' family for a separate listing of possible 
#' arguments.
#' @param x An object of class 'aquap_cube' as produced by \code{\link{gdmm}}.
#' @param y will be ignored
#' @param ... Additional parameter to override the values of the graphical 
#' parameter in the analysis procedure file of the cube-object, see details.
#' @return A PDF or graphic device.
#' @examples
#' \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset)
#'  plot(cube)
#'  plot(cube, what="all") # the same as above
#'  plot(cube, what="pca")
#' }
#' @family Core functions
#' @family Plot functions
#' @name plot
NULL

