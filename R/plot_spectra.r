plotSpectra_outer <- function(dataset, colorBy, onMain, onSub) {
	cns <- colnames(dataset$header)
	if (!all(colorBy %in% cns)) {
		a <- which(! colorBy %in% cns)
		stop(paste("Sorry, the class-variable '", paste(colorBy[a], collapse=", "), "' does not exist. Please check your input.", sep=""), call.=FALSE)
	}	
	if (is.null(colorBy)) {
		plotSpectra_inner(dataset, singleColorBy=NULL, onMain, onSub)
	} else {
		for (i in 1: length(colorBy)) {
			plotSpectra_inner(dataset, colorBy[i], onMain, onSub)
		} # end for i	
	} 
} # EOF

plotSpectra_inner <- function(dataset, singleColorBy, onMain, onSub) {
	if (is.null(singleColorBy)) {
		color <- 1
		msg <- ""
		makeLegend <- FALSE
	} else {
		makeLegend <- TRUE
		cns <- colnames(dataset$colRep)
		colInd <- which(cns == singleColorBy)
		color <- dataset$colRep[,colInd]
		msg <- paste("Color by ", cns[colInd], sep="")
		ind <- which(colnames(dataset$header) == singleColorBy)
		grouping <- dataset$header[, ind]
		legendText <- as.character(levels(grouping))
		options(warn=-1)
		nrs <- as.numeric(legendText)
		options(warn=0)
		if (any(is.na(nrs))) {
			lto <- order(legendText) # should be straight from 1 to n, because "level" already gives out characters in alphabetical order
		} else {
			lto <- order(nrs) # so if the legend text is coming from all numbers *and* they are higher than 9 we get the real order to sort later
		}		
		partN <- sapply(levels(grouping), function(x, grAll) length(which(grAll==x)), grAll=grouping)
		legendTextExtended <- paste(legendText[lto], "   N=", partN[lto], "", sep="") # have it in every line			
		legendCol <- getUniqLevelColor(color) # here read out in levels !!!
		lty=1
	} # end else
	wls <- getWavelengths(dataset)
	onSub <- paste(onSub, msg, sep=" ")
	matplot(wls, t(dataset$NIR), type="l", xlab="Wavelengths", ylab="Absorbance", main=onMain, sub=onSub, col=color)
	abline(h=0, col="gray")
	if (makeLegend) {
		legBgCol <- rgb(255,255,255, alpha=.ap2$stn$col_alphaForLegends, maxColorValue=255) # is a white with alpha to be determined in the settings
		legend("topright", legend=legendTextExtended, col=legendCol[lto], lty=lty, bg=legBgCol)
	}
} # EOF

plot_spectra_Data <- function(x, colorBy=NULL, ...) {
	autoUpS()
	dataset <- x
	ap <- getap(...)
	md <- getmd()
	where <- ap$genPlot$where
	onMain <- ap$genPlot$onMain
	onSub <- ap$genPlot$onSub
	fns <- ap$genPlot$fns
	#
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("Plotting raw spectra ... ")}
	expName <- getExpName(md)
	height <-.ap2$stn$pdf_Height_ws
	width <- .ap2$stn$pdf_Width_ws
	path <- .ap2$stn$fn_results
	suffix <- "rawSpectra"
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	onMain <- paste(expName, onMain, sep=" ")
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
	if (where != "pdf" & (Sys.getenv("RSTUDIO") != 1) & (!names(dev.cur()) == "quartz") ) {dev.new()}	
	plotSpectra_outer(dataset, colorBy, onMain, onSub)
	if (where == "pdf") {dev.off()}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF

plot_spectra_Cube <- function(x, colorBy=NULL, ...) {
	# the incoming x is the cube
	autoUpS()
	ap <- getap(...)
	md <- getmd()
	where <- ap$genPlot$where
	onMain <- ap$genPlot$onMain
	onSub <- ap$genPlot$onSub
	fns <- ap$genPlot$fns
	#
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("Plotting raw spectra ...  \n")}
	expName <- getExpName(md)
	height <-.ap2$stn$pdf_Height_ws
	width <- .ap2$stn$pdf_Width_ws
	path <- .ap2$stn$fn_results
	suffix <- "rawSpectra"
	filename <- paste(expName, suffix, sep="_")
	filename <- paste(path, "/", filename, fns, ".pdf", sep="")
	onMain <- paste(expName, onMain, sep=" ")
	if (where == "pdf") { pdf(file=filename, width, height, onefile=TRUE, family='Helvetica', pointsize=12) }
	if (where != "pdf" & (Sys.getenv("RSTUDIO") != 1)) {dev.new(height=height, width=width)}	
	for (i in 1: length(x)) {
		dataset <- getDataset(x[[i]]) # the sets are in the list within the cube
		if (!.ap2$stn$allSilent & (where == "pdf" )) {cat(paste("   working on #", i, " of ", length(x), "\n", sep=""))}
		plotSpectra_outer(dataset, colorBy, onMain, onSub)
	} # end for i
	if (where == "pdf") {dev.off()}
	if (!.ap2$stn$allSilent & (where == "pdf" )) {cat("ok\n") }
} # EOF

#' @title Plot Raw Spectra
#' @description Plot the raw spectra contained in the dataset or in the cube.
#' @details If the provided object is of class \code{\link{aquap_cube}} and is 
#' containing more than one dataset, a single graphic with raw-spectra is 
#' produced for every dataset contained within the cube.
#' @param x The standard dataset as produced by \code{\link{gfd}}, or a data-cube
#' as produced by \code{\link{gdmm}}.
#' @param colorBy Character vector, possible values are the class variables in 
#' the dataset.
#' @param ... Optional general plotting options as defined in 
#' \code{\link{plot_pg_args}}.
#' @seealso \code{\link{plot_all_modells}}
#' @examples
#' \dontrun{
#' plot_spectra(dataset)
#' plot(dataset) # the same as above
#' plot(dataset, colorBy="C_Group")
#' plot_spectra(dataset, "C_Group")
#' plot_spectra(dataset, c("C_Group", "C_Repl"))
#' plot(dataset, pg.where="") # for plotting to graphic device
#' plot_spectra(dataset, c("C_Group", "C_Repl"), pg.where="", pg.main="foo")
#' cube <- gdmm(gfd())
#' plot_spectra(cube)
#' }
#' @family Plot functions
#' @family Plot arguments
#' @name plot_spectra
NULL
