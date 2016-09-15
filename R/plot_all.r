#' @rdname plot_all_modells
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
		plot_simca(x, ...)	
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
#' @details The graphical parameters for plotting are obtained either from the 
#' standard analysis procedure file, from a custom analysis procedure file, or 
#' from the analysis procedure embedded in the cube when it was generated via 
#' \code{\link{gdmm}}. To specify the source of the analysis procedure and so of 
#' the plottig parameters use the argument \code{aps} in the \code{...} argument;
#' the default for \code{aps} is "def", meaning that the default value as stored 
#' in the settings.r file (parameter \code{gen_plot_anprocSource}) is obtained. 
#' Generally, via the \code{...} argument it is possible to 
#' override any of the plotting parameters. Please see \code{\link{anproc_file}} 
#' for a complete listing and e.g. \code{\link{plot_pca_args}} and there the other 
#' functions of the 'Plot arguments' family for a separate listing of possible 
#' arguments. Consult the documentation for the other plot functions like e.g. 
#' \code{\link{plot_pca}} for the arguments in the underlying plot functions 
#' \strong{not} contained in the analysis procedure --  see examples.
#' @param x An object of class 'aquap_cube' as produced by \code{\link{gdmm}}.
#' @param y will be ignored
#' @param ... Additional parameters to override the values of the graphical 
#' parameters in the analysis procedure and to override the defaults of the 
#' arguments in the underlying plotting functions like e.g. \code{\link{plot_pca}}.
#' Plase see details and the documentation for the other plot functions. 
#' To specify the source of the analysis procedure and so of the plottig parameters 
#' use the argument 
#' \describe{
#' \item{aps}{Character length one. The default way to obtain the analysis 
#' procedure. Defaults to "def". Possible values are:
#' \describe{
#' \item{"def"}{The default from the settings.r file is taken. (Argument 
#' \code{gen_plot_anprocSource})}
#' \item{"cube"}{Take the analysis procedure from within the cube, i.e. the 
#' analysis procedure that was used when creating the cube via \code{\link{gdmm}}
#' is used.}
#' \item{"defFile"}{Use the analysis procedure with the default filename as 
#' specified in the settings.r file in \code{fn_anProcDefFile}.}
#' \item{Custom filename}{Provide any valid filename for an analysis procedure to 
#' use as input for specifying the plotting options.}
#' }}}
#' @return A PDF or graphic device.
#' @examples
#' \dontrun{
#'  dataset <- gfd()
#'  cube <- gdmm(dataset)
#'  plot(cube)
#'  plot(cube, what="all") # the same as above
#'  plot(cube, what="pca")
#'  plot(cube, what="pca", pg.main="FooBar") # add "FooBar" to main
#'  plot(cube, what="pca", aps="foo.r", ld.col=c("green", "blue")) # to load the
#'  # analysis procedure from a file called "foo.r", and to have the loadings in 
#'  # the pca drawn in green and blue.
#' }
#' @seealso \code{\link{plot_spectra}}
#' @family Core functions
#' @family Plot functions
#' @name plot_all_modells
NULL
