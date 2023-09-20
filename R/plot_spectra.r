plotSpectra_outer <- function(dataset, colorBy, onMain, onSub, idString="", md, ...) {
	stn <- getstn()
	if (!is.null(colorBy)) {
		if (any(colorBy == "all")) {	
			cPref <- stn$p_ClassVarPref
			le <- nchar(cPref)
			cns <- colnames(dataset$header)
			cnsM <- substr(cns, 1, le)
			ind <- which(cnsM == cPref)
			colorBy <- cns[ind] # get here all the classvariables
		}
	}
	cns <- colnames(dataset$header)
	if (!all(colorBy %in% cns)) {
		a <- which(! colorBy %in% cns)
		stop(paste("Sorry, the class-variable '", paste(colorBy[a], collapse=", "), "' does not exist. Please check your input.", sep=""), call.=FALSE)
	}	
	if (is.null(colorBy)) {
		plotSpectra_inner(dataset, singleColorBy=NULL, onMain, onSub, idString, md, ...)
	} else {
		for (i in 1: length(colorBy)) {
			plotSpectra_inner(dataset, colorBy[i], onMain, onSub, idString, md, ...)
		} # end for i	
	} 
} # EOF

# the ablv and after that values used for making vertical lines when plotting the HNA/LNA cutoff (flowdex / Xiaoxia Paper)
plotSpectra_inner <- function(dataset, singleColorBy, onMain, onSub, idString="", md, ablv=NULL, ablvCol="gray", ablvLty=3, bp=NULL, slwd=NULL, ...) {
	stn <- getstn()
	xlab <- md$meta$xaxDenom
	ylab <- md$meta$yaxDenom
	#
	if (is.null(singleColorBy)) {
		colorData <- 1
		msg <- ""
		partNMsg <- paste0("   (all N=", nrow(dataset), ")")
		makeLegend <- FALSE
#		lty <- 1
	} else {
		makeLegend <- TRUE
		cns <- colnames(dataset$colRep)
		colInd <- which(cns == singleColorBy)
		color <- dataset$colRep[,colInd]
		msg <- paste("Color by ", cns[colInd], sep="")
		aa <- extractColorLegendValues(dataset, singleColorBy)  # color_data, color_unique, color_legend, txt, txtE, sumPart, dataGrouping
		colorData <- aa$color_data
		colorLegend <- aa$color_legend
		legendTextExt <- aa$txtE
		sumPart <- aa$sumPart
		legCex <- aa$legCex
		legNrCols <- aa$legNrCols
		partNMsg <- paste0("  (all N=", sumPart, ")")
	} # end else
	dptInfo <- adaptIdStringForDpt(getAnproc(dataset), idString)
	onMain <- paste(onMain, dptInfo, sep="")
	wls <- getWavelengths(dataset)
	#
#	NIR <- getNIR(dataset)
#	pickResult <- pickPeaks(as.data.frame(t(NIR)), bandwidth=stn$pp_bandwidth, wavelengths=wls)
#	plotPeaks(pires, onMain="", onSub="", adLines=TRUE, pcaVariances=NULL, customColor=NULL, ylim=NULL, wls, clty=NULL)
	#
	onSub <- paste(onSub, msg, partNMsg, sep=" ")
	if (is.null(slwd)) {
		lwdUse <- 1
	} else {
		lwdUse <- slwd
	} # end else
	matplot(wls, t(dataset$NIR), type="l", xlab=xlab, ylab=ylab, main=onMain, sub=onSub, col=colorData, lty=1, lwd=lwdUse)
	abline(h=0, col="gray")
	if (!is.null(ablv)) {
		abline(v=ablv, col=ablvCol, lty=ablvLty)
	} # end if
	if (!is.null(bp)) {
		abline(v=c(bp$bp[1], bp$bp[2]), col=bp$bpCol, lty=bp$bpLty, lwd=bp$bpLwd)
	} # end if
	if (makeLegend) {
		legBgCol <- rgb(255,255,255, alpha=stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings
#		legPosition <- getCheckLegendPosition(as.numeric(matrix(rep(wls, ncol(t(dataset$NIR))), nrow=1)), as.numeric(matrix(t(dataset$NIR), nrow=1, byrow=TRUE)))
		legPosition <- "topright"
#		print(legPosition)
		legend(legPosition, legend=legendTextExt, col=colorLegend, lty=1, bg=legBgCol, cex=legCex, ncol=legNrCols)
	}
} # EOF

plot_spectra_Data <- function(x, colorBy=NULL, ...) {
	stn <- autoUpS()
	dataset <- x
	ap <- getap(...)
	md <- getmd()
	where <- ap$genPlot$where
	onMain <- ap$genPlot$onMain
	onSub <- ap$genPlot$onSub
	fns <- ap$genPlot$fns
	#
	if (!stn$allSilent & (where == "pdf" )) {cat("Plotting raw spectra ... ")}
#	expName <- getExpName(md)
	expName <- getExpName(dataset)
	height <-stn$pdf_Height_ws
	width <- stn$pdf_Width_ws
	path <- stn$fn_results
	suffix <- "rawSpectra"
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	onMain <- paste(expName, onMain, sep=" ")
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
	if (where != "pdf" & (Sys.getenv("RSTUDIO") != 1) & (!names(dev.cur()) == "quartz") ) {dev.new(height=height, width=width)}	
	plotSpectra_outer(dataset, colorBy, onMain, onSub, md=md, ...)
	if (where == "pdf") {dev.off()}
	if (!stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF

plot_spectra_Cube <- function(x, colorBy=NULL, ...) {
	# the incoming x is the cube
	stn <- autoUpS()
	ap <- getap(...)
	md <- getmd()
	where <- ap$genPlot$where
	onMain <- ap$genPlot$onMain
	onSub <- ap$genPlot$onSub
	fns <- ap$genPlot$fns
	#
	if (!stn$allSilent & (where == "pdf" )) {cat("Plotting raw spectra ...  \n")}
#	expName <- getExpName(md)
	expName <- getExpName(x)
	height <-stn$pdf_Height_ws
	width <- stn$pdf_Width_ws
	path <- stn$fn_results
	suffix <- "rawSpectra"
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	onMain <- paste0(expName, " ", onMain, " ")
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
	if (where != "pdf" & (Sys.getenv("RSTUDIO") != 1)) {dev.new(height=height, width=width)}	
	for (i in 1: length(x)) {
		dataset <- getDataset(x[[i]]) # the sets are in the list within the cube
		idString <- getIdString(x[[i]])
		if (!stn$allSilent & (where == "pdf" )) {cat(paste("   working on #", i, " of ", length(x), "\n", sep=""))}
		plotSpectra_outer(dataset, colorBy, onMain, onSub, idString, md, ...)
	} # end for i
	if (where == "pdf") {dev.off()}
	if (!stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF

#' @title Plot Raw Spectra
#' @description Plot the raw spectra contained in the dataset or in the cube.
#' @details If the provided object is of class \code{\link{aquap_cube}} and is 
#' containing more than one dataset, a single graphic with raw-spectra is 
#' produced for every dataset contained within the cube.
#' @param x The standard dataset as produced by \code{\link{gfd}}, or a data-cube
#' as produced by \code{\link{gdmm}}.
#' @param colorBy Character vector, possible values are the class variables in 
#' the dataset. Provide a character vector with length > 1 to color along all 
#' these class variables; provide 'all' to use all available class variables for 
#' coloring - see examples.
#' @param ... Optional general plotting options as defined in 
#' \code{\link{plot_pg_args}}.
#' @seealso \code{\link{plot_all_modells}}
#' @examples
#' \dontrun{
#' plot_spectra(dataset)
#' plot_spectra(dataset, colorBy="all")
#' plot(dataset) # the same as above
#' plot(dataset, colorBy=c("C_Group"))
#' plot_spectra(dataset, "C_Group")
#' plot_spectra(dataset, c("C_Group", "C_Repl")) # produces two plots
#' plot(dataset, pg.where="") # for plotting to graphic device
#' plot_spectra(dataset, c("C_Group", "C_Repl"), pg.where="", pg.main="foo")
#' cube <- gdmm(gfd())
#' plot_spectra(cube)
#' }
#' @family Plot functions
#' @family Plot arguments
#' @family Core functions
#' @name plot_spectra
NULL
